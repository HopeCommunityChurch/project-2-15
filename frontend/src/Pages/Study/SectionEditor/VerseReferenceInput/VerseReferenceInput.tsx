import { createSignal } from "solid-js";
import * as classes from "./styles.module.scss";

export function VerseReferenceInput({
  initialValue,
  sectionIndex,
  verseIndex,
  handleVerseReferenceChange,
  validateScriptureReference,
}) {
  // Local state for verse reference
  const [localVerseReference, setLocalVerseReference] = createSignal(initialValue);
  function handleEnterPress(e) {
    if (e.key === "Enter") {
      // Your function logic here
      validateScriptureReference(sectionIndex, verseIndex);
    }
  }
  return (
    <input
      type="text"
      value={localVerseReference()}
      placeholder="Add verses (ex: Rom 1:1-7)"
      onInput={(e) => {
        const newValue = e.currentTarget.value;
        setLocalVerseReference(newValue);
      }}
      onBlur={() => {
        handleVerseReferenceChange(sectionIndex, verseIndex, localVerseReference());
      }}
      onKeyDown={handleEnterPress}
      class={classes.verseRangeSelector}
    />
  );
}
