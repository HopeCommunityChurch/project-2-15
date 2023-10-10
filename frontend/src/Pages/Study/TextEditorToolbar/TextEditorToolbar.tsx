import { createEffect, createSignal, onMount, For, createResource, Show } from "solid-js";
import ArrowIcon from "Assets/arrow.svg";
import VerticalElipsesIcon from "Assets/vertical-elipses-icon.svg";
import * as classes from "./styles.module.scss";

import UndoIcon from "./Assets/undo-icon.svg";
import RedoIcon from "./Assets/redo-icon.svg";
import BoldIcon from "./Assets/bold-icon.svg";
import ItalicIcon from "./Assets/italic-icon.svg";
import UnderlineIcon from "./Assets/underline-icon.svg";
import TextColorIcon from "./Assets/text-color-icon.svg";
import LinkIcon from "./Assets/link-icon.svg";
import AddScripture from "../../../Assets/add-scripture.svg";
import ReferenceIcon from "./Assets/reference-icon.svg";
import RephraseIcon from "./Assets/rephrase-icon.svg";
import CommentIcon from "./Assets/comment-icon.svg";
import ParallelViewIcon from "./Assets/parallel-view-icon.svg";
import NumberedListIcon from "./Assets/numbered-list-icon.svg";
import BulletListIcon from "./Assets/bullet-list-icon.svg";
import IndentIcon from "./Assets/indent-icon.svg";
import OutdentIcon from "./Assets/outdent-icon.svg";
import ClearFormattingIcon from "./Assets/clear-formatting-icon.svg";
import QuestionIcon from "./Assets/question-icon.svg";
import * as Editor from "Editor/Editor";
import { getBiblePassge } from "./validateAndFetchVerses";
import { match } from "ts-pattern";

export function TextEditorToolbar({
  editor,
  isTopbarOpen,
  setTopbarOpen,
  isSplitScreen,
  setSplitScreen,
}) {
  const [showExtendedToolbar, setShowExtendedToolbar] = createSignal(false);
  const [windowWidth, setWindowWidth] = createSignal(window.innerWidth);

  createEffect(() => {
    const handleResize = () => {
      setWindowWidth(window.innerWidth);
    };

    window.addEventListener("resize", handleResize);

    return () => {
      window.removeEventListener("resize", handleResize);
    };
  });

  return (
    <>
      <div class={`${classes.topTextEditingToolbar} ${isTopbarOpen() ? "" : classes.collapsed}`}>
        <img
          src={UndoIcon}
          class={classes.toolbarIcon}
          onClick={(e) => {
            e.preventDefault();
            editor.undo();
          }}
        />
        <img
          src={RedoIcon}
          class={classes.toolbarIcon}
          onClick={(e) => {
            e.preventDefault();
            editor.redo();
          }}
        />
        <div class={classes.seperator} />
        <ToolbarGroup1 editor={editor} />
        <ToolbarGroup2 editor={editor} />
        <ToolbarGroup3 editor={editor} />
        <ToolbarGroup4 editor={editor} />
        <ToolbarGroup5
          editor={editor}
          isSplitScreen={isSplitScreen}
          setSplitScreen={setSplitScreen}
          windowWidth={windowWidth}
        />
        <ClearFormattingSection editor={editor} />
        <div class={classes.extendedMenuContainer}>
          <img
            src={VerticalElipsesIcon}
            class={`${classes.toolbarIcon} ${classes.extendedMenuToggle} ${
              showExtendedToolbar() ? classes.expanded : ""
            }`}
            onClick={() => setShowExtendedToolbar(!showExtendedToolbar())}
          />
          {/* Conditionally show extended toolbar */}
          <Show when={showExtendedToolbar()}>
            <div class={classes.extendedToolbar}>
              <ToolbarGroup1 editor={editor} />
              <ToolbarGroup2 editor={editor} />
              <ToolbarGroup3 editor={editor} />
              <ToolbarGroup4 editor={editor} />
              <ToolbarGroup5
                editor={editor}
                isSplitScreen={isSplitScreen}
                setSplitScreen={setSplitScreen}
                windowWidth={windowWidth}
              />
              <ClearFormattingSection editor={editor} />
            </div>
          </Show>
        </div>
        <img
          src={ArrowIcon}
          class={`${classes.toolbarIcon} ${classes.collapseTopBar}`}
          onClick={() => {
            setTopbarOpen(!isTopbarOpen());
          }}
        />
      </div>
    </>
  );
}

function ClearFormattingSection({ editor }) {
  return (
    <img
      src={ClearFormattingIcon}
      class={`${classes.toolbarIcon} ${classes.clearFormatting}`}
      onClick={(e) => {
        e.preventDefault();
        editor.clearFormatting();
      }}
    />
  );
}

function ToolbarGroup1({ editor }) {
  const [showColorPickerPopup, setShowColorPickerPopup] = createSignal(false);
  const [showHighlightColorPickerPopup, setShowHighlightColorPickerPopup] = createSignal(false);
  const [highlightFillColor, setHighlightFillColor] = createSignal("#54585D");
  const [textFillColor, setTextFillColor] = createSignal("#54585D");

  const [colorPickerPosition, setColorPickerPosition] = createSignal({ x: 0, y: 0 });

  editor.onUpdate(() => {
    editor.getCurrentTextAndHighlightColors(setHighlightFillColor, setTextFillColor);
  });

  const [highlightColorPickerPosition, setHighlightColorPickerPosition] = createSignal({
    x: 0,
    y: 0,
  });

  const toggleColorPickerPopup = (event) => {
    const rect = event.currentTarget.getBoundingClientRect();
    const x = rect.left + window.pageXOffset;
    const y = rect.bottom + window.pageYOffset;
    setColorPickerPosition({ x: x + rect.width / 2, y: y });
    setTimeout(() => {
      setShowColorPickerPopup(!showColorPickerPopup());
    }, 10);
  };

  const toggleHighlightColorPickerPopup = (event) => {
    const rect = event.currentTarget.getBoundingClientRect();
    const x = rect.left + window.pageXOffset;
    const y = rect.bottom + window.pageYOffset;
    setHighlightColorPickerPosition({ x: x + rect.width / 2, y: y });
    setTimeout(() => {
      setShowHighlightColorPickerPopup(!showHighlightColorPickerPopup());
    }, 10);
  };

  createEffect(() => {
    const handleClick = () => {
      if (showColorPickerPopup()) {
        setShowColorPickerPopup(!showColorPickerPopup());
      }
    };

    document.addEventListener("click", handleClick);

    return () => {
      document.removeEventListener("click", handleClick);
    };
  });

  createEffect(() => {
    const handleClick = () => {
      if (showHighlightColorPickerPopup()) {
        setShowHighlightColorPickerPopup(!showHighlightColorPickerPopup());
      }
    };

    document.addEventListener("click", handleClick);

    return () => {
      document.removeEventListener("click", handleClick);
    };
  });

  return (
    <div class={classes.toolbarGroup1}>
      <img
        src={BoldIcon}
        class={classes.toolbarIcon}
        onClick={(e) => {
          e.preventDefault();
          editor.toggleBold();
        }}
      />
      <img
        src={ItalicIcon}
        class={classes.toolbarIcon}
        onClick={(e) => {
          e.preventDefault();
          editor.toggleItalic();
        }}
      />
      <img
        src={UnderlineIcon}
        class={classes.toolbarIcon}
        onClick={(e) => {
          e.preventDefault();
          editor.toggleUnderline();
        }}
      />
      <svg class={classes.toolbarIcon} onClick={toggleColorPickerPopup} viewBox="0 0 30 30">
        <rect x=".0884" y="23.8309" width="29.8233" height="6.1691" fill={textFillColor()} />
        <path
          data-name="Path 6847"
          d="m11.7934,16.3587h6.3985l1.1575,3.9236h4.7398L17.5229,0h-5.0605l-6.5515,20.2804h4.7398l1.1428-3.9217Zm3.1507-10.8656h.0834l2.1188,7.1714h-4.3072l2.105-7.1714Z"
          fill="#3b3e3d"
        />
      </svg>
      {showColorPickerPopup() && (
        <div
          class={classes.colorPickerPopUp}
          style={{
            position: "absolute",
            left: `${colorPickerPosition().x}px`,
            top: `${colorPickerPosition().y - 52}px`,
            transform: "translate(-50%, 0)",
          }}
        >
          <div
            class={classes.black}
            onClick={(e) => {
              e.preventDefault();
              editor.setTextColor("#000");
            }}
          ></div>
          <div
            class={classes.gray}
            onClick={(e) => {
              e.preventDefault();
              editor.setTextColor("#91A4B1");
            }}
          ></div>
          <div
            class={classes.brown}
            onClick={(e) => {
              e.preventDefault();
              editor.setTextColor("#8B4E35");
            }}
          ></div>
          <div
            class={classes.red}
            onClick={(e) => {
              e.preventDefault();
              editor.setTextColor("#F13A35");
            }}
          ></div>
          <div
            class={classes.orange}
            onClick={(e) => {
              e.preventDefault();
              editor.setTextColor("#EB732E");
            }}
          ></div>
          <div
            class={classes.yellow}
            onClick={(e) => {
              e.preventDefault();
              editor.setTextColor("#F8AC2B");
            }}
          ></div>
          <div
            class={classes.green}
            onClick={(e) => {
              e.preventDefault();
              editor.setTextColor("#00A55B");
            }}
          ></div>
          <div
            class={classes.blue}
            onClick={(e) => {
              e.preventDefault();
              editor.setTextColor("#2D78ED");
            }}
          ></div>
        </div>
      )}
      <svg
        viewBox="0 0 30 30"
        class={classes.toolbarIcon}
        onClick={toggleHighlightColorPickerPopup}
      >
        <rect x=".0884" y="23.8309" width="29.8233" height="6.1691" fill={highlightFillColor()} />
        <path
          d="m25.931,4.4543l-3.928-3.7782c-.9636-.9269-2.4962-.8971-3.4231.0665L7.3606,12.4065c-.9268.9637-.8971,2.4963.0665,3.4232l-4.1009,4.4526h9.6924l-.0029-.0036c.6413.0046,1.283-.2386,1.7627-.7373l11.2191-11.6639c.927-.9637.8972-2.4963-.0665-3.4232Zm-7.8282,8.8959l-3.3998,3.5345c-.9268.9637-2.4595.9934-3.4231.0665l-1.1951-1.1494c-.9636-.9269-.9934-2.4595-.0665-3.4232l3.3997-3.5346c.927-.9637,2.4596-.9934,3.4233-.0665l1.1949,1.1494c.9637.9269.9934,2.4595.0665,3.4232Z"
          fill="#3b3e3d"
        />
      </svg>
      {showHighlightColorPickerPopup() && (
        <div
          class={classes.highlightColorPickerPopUp}
          style={{
            position: "absolute",
            left: `${highlightColorPickerPosition().x}px`,
            top: `${highlightColorPickerPosition().y - 52}px`,
            transform: "translate(-50%, 0)",
          }}
        >
          <div
            class={classes.empty}
            onClick={(e) => {
              e.preventDefault();
              editor.removeHighlightColor();
            }}
          ></div>
          <div
            class={classes.brown}
            onClick={(e) => {
              e.preventDefault();
              editor.setHighlightColor("#EBDED9");
            }}
          ></div>
          <div
            class={classes.red}
            onClick={(e) => {
              e.preventDefault();
              editor.setHighlightColor("#FDDAD8");
            }}
          ></div>
          <div
            class={classes.orange}
            onClick={(e) => {
              e.preventDefault();
              editor.setHighlightColor("#FCE6D6");
            }}
          ></div>
          <div
            class={classes.yellow}
            onClick={(e) => {
              e.preventDefault();
              editor.setHighlightColor("#FDF4D2");
            }}
          ></div>
          <div
            class={classes.green}
            onClick={(e) => {
              e.preventDefault();
              editor.setHighlightColor("#D2EFE0");
            }}
          ></div>
          <div
            class={classes.blue}
            onClick={(e) => {
              e.preventDefault();
              editor.setHighlightColor("#C6E6FD");
            }}
          ></div>
          <div
            class={classes.purple}
            onClick={(e) => {
              e.preventDefault();
              editor.setHighlightColor("#EFDEEC");
            }}
          ></div>
        </div>
      )}
      <div class={classes.seperator} />
    </div>
  );
}
function ToolbarGroup2({ editor }) {
  return (
    <div class={classes.toolbarGroup2}>
      <img
        src={NumberedListIcon}
        class={classes.toolbarIcon}
        onClick={(e) => {
          e.preventDefault();
          editor.toggleOrderedList();
        }}
      />
      <img src={BulletListIcon} class={classes.toolbarIcon} />
      <div class={classes.seperator} />
    </div>
  );
}
function ToolbarGroup3({ editor }) {
  let increaseLevel = (e: MouseEvent) => {
    e.preventDefault();
    e.stopPropagation();
    editor.increaseLevel();
  };
  let decreaseLevel = (e: MouseEvent) => {
    e.preventDefault();
    e.stopPropagation();
    editor.decreaseLevel();
  };
  return (
    <div class={classes.toolbarGroup3}>
      <img src={OutdentIcon} class={classes.toolbarIcon} onClick={decreaseLevel} />
      <img src={IndentIcon} class={classes.toolbarIcon} onClick={increaseLevel} />
      <div class={classes.seperator} />
    </div>
  );
}

function ToolbarGroup4({ editor }) {
  const [showAddScripturePopUp, setAddScripturePopUp] = createSignal(false);
  const [addScripturePopUpPosition, setAddScripturePopUpPosition] = createSignal({ x: 0, y: 0 });
  const [addScriptureText, setAddScriptureText] = createSignal("");
  const [addScriptureErrorMessage, setAddScriptureErrorMessage] = createSignal("");

  const toggleAddScripturePopUp = (event) => {
    const rect = event.currentTarget.getBoundingClientRect();
    const x = rect.left + window.pageXOffset;
    const y = rect.bottom + window.pageYOffset;
    setAddScripturePopUpPosition({ x: x + rect.width / 2, y: y });
    setTimeout(() => {
      setAddScripturePopUp(!showAddScripturePopUp());
    }, 10);
  };

  createEffect(() => {
    const handleClick = (event) => {
      const popupElement = document.getElementById(classes.hyperlinkPopUp); // Replace with your popup's actual ID

      if (popupElement && !popupElement.contains(event.target)) {
        // Click is outside the popup
        if (showAddScripturePopUp()) {
          setAddScripturePopUp(!showAddScripturePopUp());
        }
      }
    };
    document.addEventListener("click", handleClick);
    return () => {
      document.removeEventListener("click", handleClick);
    };
  });

  const addScripture = async (e: Event) => {
    e.preventDefault();
    e.stopPropagation();
    const result = await getBiblePassge(addScriptureText());
    if (result === null) {
      setAddScriptureErrorMessage("Verse format not recognized");
      return;
    }
    //@ts-ignore
    match(result)
      .with({ state: "error" }, () => setAddScriptureErrorMessage("Verse format not recognized"))
      .with({ state: "success" }, ({ body }) => {
        setAddScriptureErrorMessage("");
        setAddScriptureText("");
        setAddScripturePopUp(false);
        editor.addVerse(body.canonical, body.passage);
      })
      .exhaustive();
  };

  const addGeneralStudyBlock = (e: Event) => {
    e.preventDefault();
    e.stopPropagation();
    editor.addGeneralStudyBlock();
  };

  return (
    <div class={classes.toolbarGroup4}>
      {/* <img src={ReferenceIcon} class={classes.toolbarIcon} /> */}
      {/* <img src={RephraseIcon} class={classes.toolbarIcon} /> */}
      <img
        src={QuestionIcon}
        class={classes.toolbarIcon}
        onClick={(e) => {
          e.preventDefault();
          editor.addQuestion();
        }}
      />
      <img src={AddScripture} class={classes.toolbarIcon} onClick={toggleAddScripturePopUp} />
      {showAddScripturePopUp() && (
        <div
          class={classes.hyperlinkPopUp}
          id={classes.hyperlinkPopUp}
          style={{
            position: "absolute",
            left: `${addScripturePopUpPosition().x}px`,
            top: `${addScripturePopUpPosition().y - 52}px`,
            transform: "translate(-50%, 0)",
          }}
        >
          <form onSubmit={addScripture}>
            <div class={classes.allInputsContainer}>
              <div class={classes.labelInputContainer}>
                <label for="hyperlinkURL">Ref:</label>
                <input
                  autofocus={true}
                  type="text"
                  id="hyperlinkURL"
                  placeholder="Ex. John 3:16"
                  value={addScriptureText()}
                  onInput={(e) => setAddScriptureText(e.target.value)}
                />
              </div>
            </div>
            <div class={classes.applyHyperlinkButton} onClick={addScripture}>
              Insert
            </div>
          </form>
          <Show when={addScriptureErrorMessage() !== ""}>
            <p class={classes.popUpErrorMessage}>{addScriptureErrorMessage()}</p>
          </Show>
        </div>
      )}
      <button onClick={addGeneralStudyBlock}>Add Study Block</button>
      <div class={classes.seperator} />
    </div>
  );
}

function ToolbarGroup5({ editor, isSplitScreen, setSplitScreen, windowWidth }) {
  const [showHyperlinkPopUp, setHyperlinkPopUp] = createSignal(false);
  const [hyperlinkPopUpPosition, setHyperlinkPopUpPosition] = createSignal({ x: 0, y: 0 });
  const [hyperlinkText, setHyperlinkText] = createSignal("");
  const [hyperlinkURL, setHyperlinkURL] = createSignal("");

  const toggleHyperlinkPopUp = (event) => {
    const rect = event.currentTarget.getBoundingClientRect();
    const x = rect.left + window.pageXOffset;
    const y = rect.bottom + window.pageYOffset;
    setHyperlinkPopUpPosition({ x: x + rect.width / 2, y: y });
    setTimeout(() => {
      setHyperlinkPopUp(!showHyperlinkPopUp());
    }, 10);
  };

  createEffect(() => {
    const handleClick = (event) => {
      const popupElement = document.getElementById(classes.hyperlinkPopUp); // Replace with your popup's actual ID

      if (popupElement && !popupElement.contains(event.target)) {
        // Click is outside the popup
        if (showHyperlinkPopUp()) {
          setHyperlinkPopUp(!showHyperlinkPopUp());
        }
      }
    };

    document.addEventListener("click", handleClick);

    return () => {
      document.removeEventListener("click", handleClick);
    };
  });

  const applyHyperlink = (e) => {
    e.preventDefault();
    editor.insertLink(hyperlinkURL(), hyperlinkText());
    setHyperlinkText("");
    setHyperlinkURL("");
    setHyperlinkPopUp(false);
  };

  return (
    <div class={classes.toolbarGroup5}>
      <img src={LinkIcon} class={classes.toolbarIcon} onClick={toggleHyperlinkPopUp} />
      {showHyperlinkPopUp() && (
        <div
          class={classes.hyperlinkPopUp}
          id={classes.hyperlinkPopUp}
          style={{
            position: "absolute",
            left: `${hyperlinkPopUpPosition().x}px`,
            top: `${hyperlinkPopUpPosition().y - 52}px`,
            transform: "translate(-50%, 0)",
          }}
        >
          <form>
            <div class={classes.allInputsContainer}>
              <div class={classes.labelInputContainer}>
                <label for="hyperlinkText">Text</label>
                <input
                  type="text"
                  id="hyperlinkText"
                  placeholder="Link Title..."
                  value={hyperlinkText()}
                  onInput={(e) => setHyperlinkText(e.target.value)}
                />
              </div>
              <div class={classes.labelInputContainer}>
                <label for="hyperlinkURL">URL</label>
                <input
                  type="text"
                  id="hyperlinkURL"
                  placeholder="https://..."
                  value={hyperlinkURL()}
                  onInput={(e) => setHyperlinkURL(e.target.value)}
                />
              </div>
            </div>
            <div class={classes.applyHyperlinkButton} onClick={applyHyperlink}>
              Apply
            </div>
          </form>
        </div>
      )}
      {/* <img src={CommentIcon} class={classes.toolbarIcon} /> */}
      <img
        src={ParallelViewIcon}
        class={`${classes.toolbarIcon} ${isSplitScreen() ? classes.active : ""}`}
        onClick={() => {
          setSplitScreen(!isSplitScreen());
        }}
      />
      <div class={classes.seperator} />
    </div>
  );
}
