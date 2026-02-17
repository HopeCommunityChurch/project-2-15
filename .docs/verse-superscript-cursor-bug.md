# Verse Superscript Click — Cursor Placement Bug

## Summary

When a user clicks a superscript verse number in the Bible text area of the ProseMirror editor, the cursor is supposed to land immediately before the first word of that verse. Instead, the cursor placement is unpredictable — sometimes it lands in the wrong position or doesn't appear to move at all.

---

## How Verse Numbers Work

### Schema (`frontend/src/Editor/textSchema.ts:263-291`)

Bible verse text is represented as regular text nodes decorated with a `verse` **mark**. The mark stores `book`, `chapter`, and `verse` attributes:

```ts
verse: {
  attrs: { book: {}, chapter: {}, verse: {} },
  toDOM: (mark) => ["span", {
    "data-verse-book": mark.attrs.book,
    "data-verse-chapter": mark.attrs.chapter,
    "data-verse-verse": mark.attrs.verse,
  }, 0],
}
```

The verse mark is applied to each word's text node, so a single verse's text may span multiple text nodes (e.g., if some words also have bold/color marks).

### Document Structure

```
section
  └─ bibleText (attrs: { verses: "Genesis 1:1" })
       └─ chunk (attrs: { level: 0 })
            └─ text "In the beginning..." [verse mark: Gen/1/1]
            └─ text " "
            └─ text "God created..." [verse mark: Gen/1/1]
            ...
```

### Decoration Plugin (`frontend/src/Editor/Editor.tsx:1023-1049`)

The superscript numbers are NOT part of the document content. They are **ProseMirror widget decorations** created by `verseReferencePlugin`:

```ts
let verseReferencePlugin = new Plugin({
  props: {
    decorations(state: EditorState) {
      return DecorationSet.create(state.doc, getDecorations(state));
    },
    // also handles ArrowLeft key to skip over widgets
  },
});
```

### Decoration Computation (`Editor.tsx:982-1021`)

`getDecorations()` walks every text node in the document. When it encounters a text node with a `verse` mark whose key (`sectionIndex-book-chapter-verse`) differs from the previous node's, it creates a widget decoration at that position:

```ts
decorations.push(
  Decoration.widget(position, verseRefWidget(verse.attrs, position), {
    key: currentVerseKey,      // e.g. "0-Genesis-1-1"
    ignoreSelection: true,
    side: -1,                  // widget appears before the position
  })
);
```

### Widget DOM Element (`Editor.tsx:957-980`)

The widget factory creates a `<span>` element:

```ts
const verseRefWidget = (verse, position) => (view: EditorView) => {
  const elem = document.createElement("span");
  elem.onclick = (e) => {
    e.preventDefault();
    const transaction = view.state.tr.setSelection(
      TextSelection.create(view.state.doc, position)
    );
    view.dispatch(transaction);
    view.focus();
  };
  elem.className = "verseRef";
  elem.contentEditable = "true";
  if (verse.verse === 1) {
    elem.innerHTML = verse.chapter + ":" + verse.verse;
  } else {
    elem.innerHTML = verse.verse;
  }
  return elem;
};
```

### Styling (`backend/static/styles/editor/editor.css:735-741`)

```css
.verseRef {
  font-size: x-small;
  vertical-align: super;
  padding-right: 3px;
  color: grey;
  cursor: pointer;
}
```

---

## Observed Behavior

The cursor briefly appears at the **correct** position (the start of the clicked verse), then almost immediately jumps to a **wrong** position. This "correct then jumps" behavior is consistent with two competing selection transactions:

1. The widget's `onclick` fires first, dispatching a transaction that sets the cursor correctly.
2. A second selection update fires shortly after and overrides it.

The most likely cause of the second update is ProseMirror's `selectionchange` listener. ProseMirror registers a DOM `selectionchange` handler that continuously syncs the browser's native caret position back into ProseMirror's state. Because the widget has `contentEditable = "true"` (Problem 3 below), the browser's native click-to-caret logic places a caret **inside** the widget's DOM. When ProseMirror's `selectionchange` handler fires (asynchronously, after `onclick`), it reads that DOM caret position, maps it back to a document position, and dispatches a new transaction — overwriting the correct selection that `onclick` just set.

Timeline of a single click:
```
mousedown  → ProseMirror captures initial click context
mouseup    → browser finalizes native caret (inside the contentEditable widget)
click      → widget's onclick fires, dispatches correct selection → cursor appears at right spot
~0-5ms later:
selectionchange → ProseMirror reads DOM caret (inside widget), maps to wrong doc position → cursor jumps
```

This explains why the cursor visibly lands correctly and then jumps — it is **not** just going to the wrong place, it is going to the right place and being immediately overridden.

Additionally, the cursor is sometimes observed landing on the **left side** of the superscript text itself, then sliding to the right. This is the browser's native caret placement at work — because `contentEditable = "true"` is set on the widget `<span>`, the browser treats it as an editable text node and places a blinking caret inside it. The caret position (left vs right of the superscript characters) depends on exactly where the user clicked within the widget. This caret is entirely outside ProseMirror's document model and has no meaningful document position, but ProseMirror's `selectionchange` handler still tries to map it, producing the subsequent jump.

### Assessment

The current implementation is a hacky workaround rather than a correct integration with ProseMirror's event model. The widget relies on `onclick` firing and "winning" a race against ProseMirror's own event handlers, but nothing actually prevents ProseMirror from processing the same click. The `contentEditable = "true"` on the widget (likely added to make the caret visually appear near the verse) creates a second race with the browser's native caret placement. The result is multiple actors (widget handler, ProseMirror handler, browser native caret) all fighting to control cursor position with no coordination between them.

By contrast, `questionMarkWidget` in the same codebase handles this correctly: it uses `onmousedown` + `stopPropagation` + `stopEvent` to claim exclusive ownership of the event before any other handler sees it.

---

## Identified Problems

### Problem 1: Stale `position` Closure Due to Widget Reuse (PRIMARY)

**Location:** `Editor.tsx:957-980` (widget factory) + `Editor.tsx:1008-1014` (decoration options)

The `key` property on the decoration tells ProseMirror to reuse the existing DOM element when the decoration set is recomputed, as long as the key matches. Since `getDecorations()` is called on **every state change** (it's in `props.decorations`), ProseMirror diffs the old and new decoration sets. If a widget's key matches one from the previous set, ProseMirror keeps the old DOM element **without calling the widget factory again**.

This means the `onclick` handler retains the `position` value from when the widget was **first created**. If the document changes (e.g., the user types text earlier in the document, shifting all subsequent positions), the captured `position` becomes stale and points to the wrong location.

**Example scenario:**
1. Document loads. Verse Gen 1:2 starts at position 50. Widget captures `position = 50`.
2. User types 10 characters earlier in the document. Gen 1:2 is now at position 60.
3. `getDecorations()` runs, creates a new decoration at position 60, but with the same `key`.
4. ProseMirror sees matching key, reuses the old DOM element (with `onclick` capturing `position = 50`).
5. User clicks the superscript "2". Cursor goes to position 50 (wrong — that's now in the middle of verse 1).

### Problem 2: Missing `stopEvent` — ProseMirror Fights the Click Handler

**Location:** `Editor.tsx:1008-1014`

The `questionMarkWidget` (line 910-914) uses `stopEvent` to prevent ProseMirror from processing click events on the widget:

```ts
// questionMarkWidget has this:
stopEvent: (e: Event) => {
  return e.type === "click";
},
```

The `verseRefWidget` does **NOT** have `stopEvent`. This means when a user clicks a verse number:

1. The widget's `onclick` fires first, dispatching a transaction to set the selection.
2. The click event bubbles up to ProseMirror's editor root.
3. ProseMirror's internal `handleClick` logic processes the same click, potentially dispatching a **second** transaction that overrides the selection to wherever ProseMirror thinks the click landed in the document.

Since the verse widget is a tiny superscript element inserted between text nodes, ProseMirror's click-to-position mapping may resolve to an adjacent text position, overriding the programmatic selection.

### Problem 3: `contentEditable = "true"` on the Widget

**Location:** `Editor.tsx:973`

```ts
elem.contentEditable = "true";
```

The verse widget is set as `contentEditable = "true"`. This tells the browser it's an editable element, so:

- The browser's native caret placement tries to position a cursor **inside** the widget.
- This conflicts with ProseMirror's document model — widget decorations are not part of the document, so a browser caret inside the widget has no corresponding ProseMirror position.
- The result is undefined behavior: sometimes the browser "wins" and places the caret in the widget (invisible to ProseMirror), sometimes ProseMirror's handler runs and moves it somewhere unexpected.

This is likely why the behavior feels "unpredictable" — it depends on a race between the browser's native cursor placement and ProseMirror's event handling.

### Problem 4: `e.preventDefault()` Without `e.stopPropagation()`

**Location:** `Editor.tsx:959-963`

```ts
elem.onclick = (e) => {
  e.preventDefault();
  // ... set selection ...
};
```

`preventDefault()` prevents the browser's default action but does **not** stop the event from propagating to parent elements. ProseMirror's click handler on the editor root still sees this event. Adding `e.stopPropagation()` would prevent ProseMirror's handler from interfering, but this alone wouldn't fix Problem 1 (stale positions).

---

## How These Interact

The typical failure sequence:

1. User edits the document (typing, pasting, etc.)
2. Verse positions shift, but widget DOM elements are reused (Problem 1)
3. User clicks a superscript verse number
4. Browser tries to place caret inside the `contentEditable` widget (Problem 3)
5. Widget's `onclick` fires, dispatching a selection to a stale position (Problem 1)
6. Event bubbles to ProseMirror, which dispatches its own selection (Problem 2)
7. Final cursor position is whatever the last dispatch set — unpredictable

---

## Comparison: How `questionMarkWidget` Avoids This

The question mark widget (`Editor.tsx:854-884`) is similar but avoids some of these issues:

| Aspect | `verseRefWidget` | `questionMarkWidget` |
|--------|------------------|----------------------|
| `stopEvent` | **No** | Yes — stops `click` events |
| `contentEditable` | **`"true"`** | Not set (default: inherits from parent) |
| `stopPropagation` | **No** | Uses `e.stopPropagation()` in `onmousedown` |
| Event used | `onclick` | `onmousedown` (fires before click) |
| Purpose on click | Set cursor position | Open question popup |

The question mark widget uses `onmousedown` + `stopPropagation` + `stopEvent`, which prevents ProseMirror from ever seeing the click. The verse ref widget uses none of these.

---

## Standard Fix

### The `getPos` Callback — ProseMirror's Built-In Solution

The current code uses a **non-standard pattern**. ProseMirror's `Decoration.widget` factory function provides a `getPos` callback as its **second parameter** — the official, built-in mechanism for getting a widget's current document position at any point in time:

```ts
// ProseMirror API signature:
Decoration.widget(
  pos: number,
  toDOM: (view: EditorView, getPos: () => number | undefined) => DOMNode,
  spec?: { key, side, stopEvent, ignoreSelection, ... }
)
```

The current code **ignores `getPos` entirely** and instead captures a stale `position` in a closure:

```ts
// CURRENT (broken) — position is stale after any document edit:
const verseRefWidget = (verse, position) => (view: EditorView) => { ... };

// STANDARD — getPos always returns the widget's current position:
const verseRefWidget = (verse) => (view: EditorView, getPos: () => number | undefined) => { ... };
```

Source: [ProseMirror Reference Manual — Decoration.widget](https://prosemirror.net/docs/ref/#view.Decoration%5Ewidget)

### Combined Fix (All Four Problems)

Apply all changes together — they are small, independent, and each addresses a specific problem:

**`verseRefWidget` (Editor.tsx:957-980):**
```ts
const verseRefWidget = (verse) => (view: EditorView, getPos: () => number | undefined) => {
  const elem = document.createElement("span");
  elem.onmousedown = (e) => {                              // [Fix 4] onmousedown, not onclick
    e.preventDefault();
    e.stopPropagation();                                    // [Fix 4] stop ProseMirror from seeing this
    const pos = getPos();                                   // [Fix 1] always-current position via ProseMirror API
    if (pos != null) {
      const tr = view.state.tr.setSelection(
        TextSelection.create(view.state.doc, pos)
      );
      view.dispatch(tr);
      view.focus();
    }
  };
  elem.ondblclick = (e) => {
    e.preventDefault();
    e.stopPropagation();
    let book = verse.book.replace(" ", "_").toLowerCase();
    let url = "https://biblehub.com/" + book + "/" + verse.chapter + "-" + verse.verse + ".htm";
    window.open(url, "_blank").focus();
    view.focus();
  };
  elem.className = "verseRef";
                                                            // [Fix 3] contentEditable removed entirely
  if (verse.verse === 1) {
    elem.innerHTML = verse.chapter + ":" + verse.verse;
  } else {
    elem.innerHTML = verse.verse;
  }
  return elem;
};
```

**Decoration creation in `getDecorations` (Editor.tsx:1008-1014):**
```ts
decorations.push(
  Decoration.widget(position, verseRefWidget(verse.attrs), {  // position no longer passed to widget
    key: currentVerseKey,
    ignoreSelection: true,
    side: -1,
    stopEvent: (e: Event) => e.type === "mousedown" || e.type === "click",  // [Fix 2]
  })
);
```

### What Each Change Does

| Change | Problem Fixed | Why |
|--------|--------------|-----|
| Use `getPos()` instead of closed-over `position` | Stale position (Problem 1) | `getPos` is ProseMirror's official API — always returns the widget's current document position, even after edits shift it |
| Add `stopEvent` for `mousedown` and `click` | Event conflict (Problem 2) | Tells ProseMirror to ignore these events on the widget, preventing its internal handlers from overriding our selection |
| Remove `contentEditable = "true"` | Browser caret conflict (Problem 3) | Widget is a read-only label. Removing this stops the browser from placing a native caret inside it, eliminating the `selectionchange` race |
| Switch `onclick` → `onmousedown` + `stopPropagation` | Event timing (Problem 4) | `mousedown` fires before ProseMirror's own `mousedown` handler. `stopPropagation` prevents the event from reaching ProseMirror at all. This is the same pattern used by `questionMarkWidget` in this codebase |

### Why This Is Stable

- **`getPos`** is ProseMirror's own API for this exact use case. It is maintained as part of the public API and documented in the reference manual. It works correctly with widget `key`-based reuse — even when the DOM element is reused across state changes, `getPos` returns the updated position.
- **`stopEvent` + `onmousedown` + `stopPropagation`** gives the widget exclusive ownership of the event. No race conditions, no competing handlers. This is the same pattern used by `questionMarkWidget` in this codebase (lines 878-882, 910-914).
- **Removing `contentEditable`** eliminates an entire class of browser-level caret interference. Widget decorations should not be editable — they are not part of the ProseMirror document model. The ProseMirror author [recommends against interactive decorations](https://discuss.prosemirror.net/t/widget-decoration-and-cursor-position-issue/2756) that conflict with the editor's own selection management.

### Sources

- [ProseMirror Reference Manual — Decoration.widget](https://prosemirror.net/docs/ref/#view.Decoration%5Ewidget) — documents the `getPos` callback and `stopEvent` spec option
- [How to handle events inside decorations](https://discuss.prosemirror.net/t/how-to-handle-events-inside-decorations/1083) — ProseMirror forum discussion on the standard pattern
- [Widget decoration and cursor position issue](https://discuss.prosemirror.net/t/widget-decoration-and-cursor-position-issue/2756) — marijn (ProseMirror author) on why interactive decorations should not fight the editor's selection

---

## Files Involved

| File | Lines | Role |
|------|-------|------|
| `frontend/src/Editor/textSchema.ts` | 263-291 | `verse` mark definition |
| `frontend/src/Editor/Editor.tsx` | 957-980 | `verseRefWidget` — creates the clickable superscript DOM element |
| `frontend/src/Editor/Editor.tsx` | 982-1021 | `getDecorations` — computes decoration positions |
| `frontend/src/Editor/Editor.tsx` | 1023-1049 | `verseReferencePlugin` — plugin that applies decorations + ArrowLeft handling |
| `frontend/src/Editor/editorUtils.tsx` | 122-129 | `mkVerseNode` — creates verse text nodes when inserting Bible passages |
| `backend/static/styles/editor/editor.css` | 735-741 | `.verseRef` CSS (superscript styling) |
