import { createEffect, createSignal, onMount, For, createResource, Show } from "solid-js";
import ArrowIcon from "Assets/arrow.svg";
import VerticalElipsesIcon from "Assets/vertical-elipses-icon.svg";
import * as classes from "./styles.module.scss";

import UndoIcon from "./Assets/undo-icon.svg";
import RedoIcon from "./Assets/redo-icon.svg";
import BoldIcon from "./Assets/bold-icon.svg";
import ItalicIcon from "./Assets/italic-icon.svg";
import UnderlineIcon from "./Assets/underline-icon.svg";
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
import AddStudyBlockIcon from "../../../Assets/add-study-block-icon.svg";
import { GroupStudyRaw } from "Types";

type Props = {
  editor : Editor.P215Editor;
  isTopbarOpen: () => boolean;
  setTopbarOpen : (arg : boolean) => void;
  activeEditor : any
  isSplitScreen : () => boolean;
  setSplitScreen : (arg : boolean) => void;
  groupStudy?: GroupStudyRaw;
};

export function TextEditorToolbar({
  editor,
  isTopbarOpen,
  setTopbarOpen,
  activeEditor,
  isSplitScreen,
  setSplitScreen,
  groupStudy,
} : Props) {
  const [showExtendedToolbar, setShowExtendedToolbar] = createSignal(false);
  const [windowWidth, setWindowWidth] = createSignal(window.innerWidth);
  const [operatingSystem, setOperatingSystem] = createSignal("Unknown");

  createEffect(() => {
    const isDesktop = window.innerWidth > 800;

    if (isDesktop) {
      const platform = navigator.platform.toLowerCase();
      let os = "Unknown OS";

      if (platform.includes("win")) os = "Windows";
      else if (platform.includes("mac")) os = "Mac";
      else if (platform.includes("linux")) os = "Linux";

      setOperatingSystem(os);
    }
  });

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
        <div class={classes.tooltipContainer}>
          <img
            src={UndoIcon}
            class={classes.toolbarIcon}
            onClick={(e) => {
              e.preventDefault();
              activeEditor().undo();
            }}
          />{" "}
          <span class={classes.tooltipText}>Undo</span>
        </div>

        <div class={classes.tooltipContainer}>
          <img
            src={RedoIcon}
            class={classes.toolbarIcon}
            onClick={(e) => {
              e.preventDefault();
              activeEditor().redo();
            }}
          />{" "}
          <span class={classes.tooltipText}>Redo</span>
        </div>

        <div class={classes.seperator} />
        <Show when={activeEditor()}>
          <ToolbarGroup1 activeEditor={activeEditor} operatingSystem={operatingSystem} />
          <ToolbarGroup3
            activeEditor={activeEditor}
            editor={editor}
            operatingSystem={operatingSystem}
          />
          <ToolbarGroup4 activeEditor={activeEditor} operatingSystem={operatingSystem} />
          { (groupStudy == null)? (<></>) :
              (<ToolbarGroup5 isSplitScreen={isSplitScreen} setSplitScreen={setSplitScreen} />)
          }
          <ClearFormattingSection activeEditor={activeEditor} />
        </Show>

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
              <Show when={activeEditor()}>
                <ToolbarGroup1 activeEditor={activeEditor} operatingSystem={operatingSystem} />
                <ToolbarGroup3
                  activeEditor={activeEditor}
                  editor={editor}
                  operatingSystem={operatingSystem}
                />
                <ToolbarGroup4 activeEditor={activeEditor} operatingSystem={operatingSystem} />
                { (groupStudy == null)? (<></>) :
                    (<ToolbarGroup5 isSplitScreen={isSplitScreen} setSplitScreen={setSplitScreen} />)
                }
                <ClearFormattingSection activeEditor={activeEditor} />
              </Show>
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

function ClearFormattingSection({ activeEditor }) {
  return (
    <div class={`${classes.tooltipContainer} ${classes.clearFormattingContainer}`}>
      <img
        src={ClearFormattingIcon}
        class={`${classes.toolbarIcon} ${classes.clearFormatting}`}
        onClick={(e) => {
          e.preventDefault();
          if (activeEditor()) {
            activeEditor().clearFormatting();
          } else {
            console.error("Editor is null");
          }
        }}
      />
      <span class={classes.tooltipText}>Clear Formatting</span>
    </div>
  );
}

function ToolbarGroup1({ activeEditor, operatingSystem }) {
  const [showColorPickerPopup, setShowColorPickerPopup] = createSignal(false);
  const [showHighlightColorPickerPopup, setShowHighlightColorPickerPopup] = createSignal(false);
  const [highlightFillColor, setHighlightFillColor] = createSignal("#54585D");
  const [textFillColor, setTextFillColor] = createSignal("#54585D");

  const [colorPickerPosition, setColorPickerPosition] = createSignal({ x: 0, y: 0 });

  // For some reason, doesn't deteck marks on qNode.editor??
  activeEditor().onUpdate(() => {
    activeEditor().getCurrentTextAndHighlightColors(setHighlightFillColor, setTextFillColor);
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
      <div class={classes.tooltipContainer}>
        <img
          src={BoldIcon}
          class={classes.toolbarIcon}
          onClick={(e) => {
            e.preventDefault();
            activeEditor().toggleBold();
          }}
        />{" "}
        <span class={classes.tooltipText}>
          Bold ({operatingSystem() === "Mac" ? "⌘" : "Ctrl"}+b)
        </span>
      </div>

      <div class={classes.tooltipContainer}>
        <img
          src={ItalicIcon}
          class={classes.toolbarIcon}
          onClick={(e) => {
            e.preventDefault();
            activeEditor().toggleItalic();
          }}
        />
        <span class={classes.tooltipText}>
          Italic ({operatingSystem() === "Mac" ? "⌘" : "Ctrl"}+i)
        </span>
      </div>

      <div class={classes.tooltipContainer}>
        <img
          src={UnderlineIcon}
          class={classes.toolbarIcon}
          onClick={(e) => {
            e.preventDefault();
            activeEditor().toggleUnderline();
          }}
        />{" "}
        <span class={classes.tooltipText}>
          Underline ({operatingSystem() === "Mac" ? "⌘" : "Ctrl"}+u)
        </span>
      </div>

      <div class={classes.tooltipContainer}>
        <svg
          class={classes.toolbarIcon}
          onMouseDown={(e) => e.preventDefault()}
          onClick={toggleColorPickerPopup}
          viewBox="0 0 30 30"
        >
          <rect x=".0884" y="23.8309" width="29.8233" height="6.1691" fill={textFillColor()} />
          <path
            data-name="Path 6847"
            d="m11.7934,16.3587h6.3985l1.1575,3.9236h4.7398L17.5229,0h-5.0605l-6.5515,20.2804h4.7398l1.1428-3.9217Zm3.1507-10.8656h.0834l2.1188,7.1714h-4.3072l2.105-7.1714Z"
            fill="#3b3e3d"
          />
        </svg>{" "}
        <span class={classes.tooltipText}>Text Color</span>
      </div>

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
              activeEditor().setTextColor("#000");
            }}
          ></div>
          <div
            class={classes.gray}
            onClick={(e) => {
              e.preventDefault();
              activeEditor().setTextColor("#91A4B1");
            }}
          ></div>
          <div
            class={classes.brown}
            onClick={(e) => {
              e.preventDefault();
              activeEditor().setTextColor("#8B4E35");
            }}
          ></div>
          <div
            class={classes.red}
            onClick={(e) => {
              e.preventDefault();
              activeEditor().setTextColor("#F13A35");
            }}
          ></div>
          <div
            class={classes.orange}
            onClick={(e) => {
              e.preventDefault();
              activeEditor().setTextColor("#EB732E");
            }}
          ></div>
          <div
            class={classes.yellow}
            onClick={(e) => {
              e.preventDefault();
              activeEditor().setTextColor("#F8AC2B");
            }}
          ></div>
          <div
            class={classes.green}
            onClick={(e) => {
              e.preventDefault();
              activeEditor().setTextColor("#00A55B");
            }}
          ></div>
          <div
            class={classes.blue}
            onClick={(e) => {
              e.preventDefault();
              activeEditor().setTextColor("#2D78ED");
            }}
          ></div>
        </div>
      )}
      <div class={classes.tooltipContainer}>
        <svg
          viewBox="0 0 30 30"
          class={classes.toolbarIcon}
          onMouseDown={(e) => e.preventDefault()}
          onClick={toggleHighlightColorPickerPopup}
        >
          <rect x=".0884" y="23.8309" width="29.8233" height="6.1691" fill={highlightFillColor()} />
          <path
            d="m25.931,4.4543l-3.928-3.7782c-.9636-.9269-2.4962-.8971-3.4231.0665L7.3606,12.4065c-.9268.9637-.8971,2.4963.0665,3.4232l-4.1009,4.4526h9.6924l-.0029-.0036c.6413.0046,1.283-.2386,1.7627-.7373l11.2191-11.6639c.927-.9637.8972-2.4963-.0665-3.4232Zm-7.8282,8.8959l-3.3998,3.5345c-.9268.9637-2.4595.9934-3.4231.0665l-1.1951-1.1494c-.9636-.9269-.9934-2.4595-.0665-3.4232l3.3997-3.5346c.927-.9637,2.4596-.9934,3.4233-.0665l1.1949,1.1494c.9637.9269.9934,2.4595.0665,3.4232Z"
            fill="#3b3e3d"
          />
        </svg>{" "}
        <span class={classes.tooltipText}>Highlight Color</span>
      </div>

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
              activeEditor().removeHighlightColor();
            }}
          ></div>
          <div
            class={classes.brown}
            onClick={(e) => {
              e.preventDefault();
              activeEditor().setHighlightColor("#EBDED9");
            }}
          ></div>
          <div
            class={classes.red}
            onClick={(e) => {
              e.preventDefault();
              activeEditor().setHighlightColor("#FDDAD8");
            }}
          ></div>
          <div
            class={classes.orange}
            onClick={(e) => {
              e.preventDefault();
              activeEditor().setHighlightColor("#FCE6D6");
            }}
          ></div>
          <div
            class={classes.yellow}
            onClick={(e) => {
              e.preventDefault();
              activeEditor().setHighlightColor("#FDF4D2");
            }}
          ></div>
          <div
            class={classes.green}
            onClick={(e) => {
              e.preventDefault();
              activeEditor().setHighlightColor("#D2EFE0");
            }}
          ></div>
          <div
            class={classes.blue}
            onClick={(e) => {
              e.preventDefault();
              activeEditor().setHighlightColor("#C6E6FD");
            }}
          ></div>
          <div
            class={classes.purple}
            onClick={(e) => {
              e.preventDefault();
              activeEditor().setHighlightColor("#EFDEEC");
            }}
          ></div>
        </div>
      )}
      <div class={classes.seperator} />
    </div>
  );
}
function ToolbarGroup2({ editor, operatingSystem }) {
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
function ToolbarGroup3({ editor, activeEditor, operatingSystem }) {
  const [isQuestionPopupEditor, setIsQuestionPopupEditor] = createSignal(false);

  createEffect(() => {
    try {
      // Attempt to access the property
      const test = activeEditor().nodeViews.questionAnswer;
      setIsQuestionPopupEditor(true); // No error, so it's a question popup editor
    } catch (error) {
      setIsQuestionPopupEditor(false); // Error occurred, not a question popup editor
    }
  });

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
      <div class={classes.tooltipContainer}>
        {isQuestionPopupEditor() ? (
          <>
            <img src={OutdentIcon} class={`${classes.toolbarIcon} ${classes.disabled}`} />
            <span class={classes.tooltipText}>Can't Outdent in popup</span>
          </>
        ) : (
          <>
            <img src={OutdentIcon} class={`${classes.toolbarIcon}`} onClick={decreaseLevel} />
            <span class={classes.tooltipText}>
              Outdent ({operatingSystem() === "Mac" ? "⌘" : "Ctrl"}+[`)
            </span>
          </>
        )}
      </div>
      <div class={classes.tooltipContainer}>
        {isQuestionPopupEditor() ? (
          <>
            <img src={IndentIcon} class={`${classes.toolbarIcon} ${classes.disabled}`} />
            <span class={classes.tooltipText}>Can't Indent in popup</span>
          </>
        ) : (
          <>
            <img src={IndentIcon} class={`${classes.toolbarIcon}`} onClick={increaseLevel} />
            <span class={classes.tooltipText}>
              Indent ({operatingSystem() === "Mac" ? "⌘" : "Ctrl"}+{`]`})
            </span>
          </>
        )}
      </div>
      <div class={classes.seperator} />
    </div>
  );
}

function ToolbarGroup4({ activeEditor, operatingSystem }) {
  const [showAddScripturePopUp, setAddScripturePopUp] = createSignal(false);
  const [addScripturePopUpPosition, setAddScripturePopUpPosition] = createSignal({ x: 0, y: 0 });
  const [addScriptureText, setAddScriptureText] = createSignal("");
  const [addScriptureErrorMessage, setAddScriptureErrorMessage] = createSignal("");
  const [isQuestionPopupEditor, setIsQuestionPopupEditor] = createSignal(false);

  createEffect(() => {
    try {
      // Attempt to access the property
      const test = activeEditor().nodeViews.questionAnswer;
      setIsQuestionPopupEditor(true); // No error, so it's a question popup editor
    } catch (error) {
      setIsQuestionPopupEditor(false); // Error occurred, not a question popup editor
    }
  });

  const getCurrentSectionHeaderText = async (editor) => {
    const { state } = editor.view;
    const { selection } = state;
    let headerText = "";

    // Traverse up the document structure from the selection to find the section
    state.doc.nodesBetween(selection.from, selection.to, (node, pos) => {
      if (node.type.name === "section") {
        // Find the sectionHeader child of this section
        node.forEach((childNode) => {
          if (childNode.type.name === "sectionHeader") {
            headerText = childNode.textContent;
          }
        });
        return false; // Stop traversing further
      }
    });

    const result = await getBiblePassge(headerText);
    if (result != null) {
      return headerText;
    }

    return "";
  };

  const toggleAddScripturePopUp = (event) => {
    const rect = event.currentTarget.getBoundingClientRect();
    const x = rect.left + window.pageXOffset;
    const y = rect.bottom + window.pageYOffset;
    setAddScripturePopUpPosition({ x: x + rect.width / 2, y: y });

    setTimeout(() => {
      // Toggle the pop-up visibility
      setAddScripturePopUp(!showAddScripturePopUp());
    }, 10);

    setTimeout(async () => {
      const currentSectionHeader = await getCurrentSectionHeaderText(activeEditor());

      // Set this text into the pop-up's input field
      setAddScriptureText(currentSectionHeader);
    }, 0);

    setTimeout(() => {
      const inputField = document.getElementById("addScriptureField");
      if (inputField) {
        inputField.focus();
      }
    }, 0);
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
        activeEditor().addVerse(body.canonical, body.passage);
      })
      .exhaustive();
  };

  const addGeneralStudyBlock = (e: Event) => {
    e.preventDefault();
    e.stopPropagation();
    activeEditor().addGeneralStudyBlock();
  };

  return (
    <div class={classes.toolbarGroup4}>
      {/* <img src={ReferenceIcon} class={classes.toolbarIcon} /> */}
      {/* <img src={RephraseIcon} class={classes.toolbarIcon} /> */}
      <div class={classes.tooltipContainer}>
        {isQuestionPopupEditor() ? (
          <>
            <img src={QuestionIcon} class={`${classes.toolbarIcon} ${classes.disabled}`} />
            <span class={classes.tooltipText}>Can't add question in popup</span>
          </>
        ) : (
          <>
            <img
              src={QuestionIcon}
              class={`${classes.toolbarIcon}`}
              onClick={(e) => {
                e.preventDefault();
                activeEditor().addQuestion();
              }}
            />
            <span class={classes.tooltipText}>
              Add Question ({operatingSystem() === "Mac" ? "⌘" : "Ctrl"}+e)
            </span>
          </>
        )}
      </div>

      <div class={classes.tooltipContainer}>
        {isQuestionPopupEditor() ? (
          <>
            <img src={AddScripture} class={`${classes.toolbarIcon} ${classes.disabled}`} />
            <span class={classes.tooltipText}>Can't insert scripture in popup</span>
          </>
        ) : (
          <>
            <img
              src={AddScripture}
              class={`${classes.toolbarIcon}`}
              onClick={toggleAddScripturePopUp}
            />
            <span class={classes.tooltipText}>Insert Scripture</span>
          </>
        )}
      </div>
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
                <label for="addScriptureField">Ref:</label>
                <input
                  autofocus={true}
                  type="text"
                  id="addScriptureField"
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
      <div class={classes.tooltipContainer}>
        {isQuestionPopupEditor() ? (
          <>
            <img src={AddStudyBlockIcon} class={`${classes.toolbarIcon} ${classes.disabled}`} />
            <span class={classes.tooltipText}>Can't add study blocks in popup</span>
          </>
        ) : (
          <>
            <img
              src={AddStudyBlockIcon}
              class={`${classes.toolbarIcon}`}
              onClick={addGeneralStudyBlock}
            />
            <span class={classes.tooltipText}>
              Add Study Block ({operatingSystem() === "Mac" ? "⌘" : "Ctrl"}+s)
            </span>
          </>
        )}
      </div>
      <div class={classes.seperator} />
    </div>
  );
}

function ToolbarGroup5({ isSplitScreen, setSplitScreen }) {
  return (
    <div class={classes.toolbarGroup5}>
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
