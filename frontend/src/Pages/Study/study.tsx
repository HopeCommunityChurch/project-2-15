// Libraries and external modules
import { createEffect, createSignal, onMount, createResource, Show } from "solid-js";
import { useParams, useNavigate } from "@solidjs/router";
import { throttle } from "@solid-primitives/scheduled";
import { match } from "ts-pattern";
import { dndzone } from "solid-dnd-directive";

// Local imports
import * as Network from "Utils/Network";
import * as classes from "./styles.module.scss";
import * as Editor from "Editor/Editor";
import { PublicUser, DocRaw } from "../../Types";
import { LoginUser, loginState } from "Pages/LoginPage/login";
import { StudyTopNav } from "./StudyTopNav/StudyTopNav";
import { TextEditorToolbar } from "./TextEditorToolbar/TextEditorToolbar";

// Icons
import BlueCheckIcon from "Assets/blue-check-icon.svg";
import DragHandleIcon from "Assets/drag-handle.svg";
import GrayCircleIcon from "Assets/gray-circle-icon.svg";
import Arrow2Icon from "Assets/arrow2.svg";
import GrayTrashIcon from "Assets/gray-trash-icon.svg";
import BluePencil from "Assets/blue-pencil.png";
import CloseXIcon from "Assets/x.svg";
import SplitScreenIcon from "Assets/split-screen.png";
import ArrowIcon from "Assets/arrow.svg";

// Network functions
async function getStudy(documentId): Promise<Network.NetworkState<DocRaw>> {
  return Network.request("/document/" + documentId);
}

export function StudyPage() {
  const nav = useNavigate();
  const documentID = useParams().documentID;
  const [result] = createResource([], () => getStudy(documentID), {
    initialValue: { state: "loading" },
  });

  createEffect(() => {});

  return (
    <>
      {
        //@ts-ignore
        match(loginState() as LoginUser)
          .with({ state: "notLoggedIn" }, () => {
            nav("/app/login?redirect=/app/study/" + documentID);
            return <> </>;
          })
          .with({ state: "loggedIn" }, ({ user }) =>
            //@ts-ignore
            match(result())
              .with({ state: "loading" }, () => <></>)
              .with({ state: "error" }, ({ body }) => <>{JSON.stringify(body)}</>)
              .with({ state: "success" }, ({ body }) => StudyLoggedIn(body, user))
              .with({ state: "notloaded" }, () => <>not loaded</>)
              .exhaustive()
          )
          .exhaustive()
      }
    </>
  );
}

function StudyLoggedIn(doc: DocRaw, currentUser: PublicUser) {
  // State and Variables
  const [isSidebarOpen, setSidebarOpen] = createSignal(false);
  const [isTopbarOpen, setTopbarOpen] = createSignal(true);
  const [isSplitScreen, setSplitScreen] = createSignal(false);
  const [splitScreenOrientation, setSplitScreenOrientation] = createSignal(true);
  const [sectionEditorMode, setSectionEditorMode] = createSignal(false);
  const [sectionTitles, setSectionTitles] = createSignal([]);
  const [dragDisabled, setDragDisabled] = createSignal(true);

  let editorRoot: HTMLDivElement;
  let editorRootSplitScreen: HTMLDivElement;
  let editor: Editor.P215Editor = new Editor.P215Editor(doc.document);
  let editorSplitScreen: Editor.P215Editor = new Editor.P215Editor(doc.document);

  //resizing height on mobile
  onMount(() => {
    // Handle dynamic viewport resizing
    const updateVH = () => {
      let vh = window.innerHeight * 0.01;
      document.documentElement.style.setProperty("--vh", `${vh}px`);
    };

    // Set initial viewport height
    updateVH();

    window.addEventListener("resize", updateVH);

    // Cleanup listeners when component is destroyed
    return () => {
      window.removeEventListener("resize", updateVH);
    };
  });

  //Resizing sidebar
  onMount(() => {
    const handleSidebarState = () => {
      if (window.innerWidth <= 750) {
        setSidebarOpen(false);
      } else {
        setSidebarOpen(true);
      }
    };

    // Set initial state based on viewport width
    handleSidebarState();

    // Add resize listener
    window.addEventListener("resize", handleSidebarState);

    const sidebar = document.querySelector(`.${classes.sidebar}`) as HTMLElement;
    let isResizing = false;
    let startWidth;
    let startX;

    // Start resizing when the user clicks on the right border
    if (sidebar) {
      document
        .querySelector(`.${classes.sidebarResizeHandle}`)
        .addEventListener("mousedown", (e: MouseEvent) => {
          isResizing = true;
          startX = e.clientX;
          startWidth = sidebar.offsetWidth;

          document.body.classList.add(classes.noSelect);
          document.addEventListener("mousemove", handleMouseMove);
          document.addEventListener("mouseup", () => {
            // Stop resizing
            isResizing = false;
            document.body.classList.remove(classes.noSelect);
            document.removeEventListener("mousemove", handleMouseMove);
          });
        });
    }

    function handleMouseMove(e) {
      if (!isResizing) return;

      const width = startWidth + (e.clientX - startX);

      // Apply the width, but within the defined min and max boundaries
      sidebar.style.width = Math.max(Math.min(width, 500), 170) + "px";
    }

    // Cleanup listener when component is destroyed
    return () => {
      window.removeEventListener("resize", handleSidebarState);
    };
  });

  onMount(() => {
    editor.addEditor(editorRoot);
    editorSplitScreen.addEditor(editorRootSplitScreen);
    updateSectionTitles(doc);
  });

  editor.onUpdate((value) => {
    updateSignal(value);
  });

  // Helper Functions
  const updateSignal = throttle(
    (change) =>
      Network.request("/document/" + doc.docId, {
        method: "PUT",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify(change),
      })
        .then((res) => {
          updateSectionTitles(res.body);
        })
        .catch((err) => {
          console.error(err);
        }),
    500
  );

  const handleScrollToSection = (sectionIndex) => {
    const element = document.getElementById(`section-${sectionIndex}`);

    if (element) {
      // Close the sidebar and then scroll if window width <= 750px
      if (window.innerWidth <= 750) {
        setSidebarOpen(false);

        // Wait 300ms and then scroll smoothly to the section
        setTimeout(() => {
          element.scrollIntoView({ behavior: "smooth" });
        }, 300);
      } else {
        // Scroll immediately if window width is greater than 750px
        element.scrollIntoView({ behavior: "smooth" });
      }
    }
  };

  const addNewSection = () => {
    editor.addSection();

    setSectionTitles((prevTitles) => [
      ...prevTitles,
      {
        title: "Untitled",
        id: prevTitles.length,
      },
    ]);
  };

  const updateSectionTitles = (studyDoc) => {
    const titlesWithId = studyDoc.document.content
      .map((section, index) => {
        let header = section.content.find((innerSection) => {
              return innerSection.type === "sectionHeader";
        });
        if(header) {
          return {
            title: header.content[0].text,
            id: index,
          };
        } else {
          return {
            title: "untitled",
            id: index,
          };
        }
      })
    setSectionTitles(titlesWithId);
  };

  function handleDndEvent(e) {
    const {
      items: newItems,
      info: { id, trigger },
    } = e.detail;

    // If the drag has ended, log the initial and final index
    if (trigger === "droppedIntoZone") {
      const finalIdx = newItems.findIndex((item) => item.id === id);
      console.log(`Moved from index ${id} to ${finalIdx}`);

      editor.moveSection(id, finalIdx);
      //TODO Jonny: add function to push update to editor
    }

    setSectionTitles(newItems);
    setDragDisabled(true);
  }

  function startDrag(e) {
    e.preventDefault(); // to prevent lag on touch devices
    setDragDisabled(false);
  }

  return (
    <div class={classes.wholePageContainer}>
      <StudyTopNav
        isSidebarOpen={isSidebarOpen}
        setSidebarOpen={setSidebarOpen}
        isTopbarOpen={isTopbarOpen}
        doc={doc}
      />
      <TextEditorToolbar
        editor={editor}
        isTopbarOpen={isTopbarOpen}
        setTopbarOpen={setTopbarOpen}
        isSplitScreen={isSplitScreen}
        setSplitScreen={setSplitScreen}
      />
      <div
        class={`${classes.pageBody} ${isTopbarOpen() ? "" : classes.collapsed}  ${
          isSidebarOpen() ? classes.sidebarCollapsed : ""
        }`}
      >
        <div class={classes.sidebarContainer}>
          <div class={`${classes.sidebar} ${isSidebarOpen() ? "" : classes.closed}`}>
            <div
              onClick={() => {
                setSidebarOpen(!isSidebarOpen());
                !isSidebarOpen() ? setSectionEditorMode(false) : null;
              }}
              class={classes.sidebarToggle}
            >
              {isSidebarOpen() ? (
                <>
                  <img src={Arrow2Icon} />
                  <p>COLLAPSE</p>
                </>
              ) : (
                <img src={Arrow2Icon} class={classes.reverse} />
              )}
            </div>
            {isSidebarOpen() ? (
              <div
                onClick={() => setSectionEditorMode(!sectionEditorMode())}
                class={classes.editSections}
              >
                <img src={BluePencil} />
                <p>{sectionEditorMode() ? "STOP EDITING" : "EDIT SECTIONS"}</p>
              </div>
            ) : null}
            <div
              class={classes.allSectionSidebarContainer}
              //@ts-ignore
              use:dndzone={{ items: sectionTitles, dragDisabled }}
              on:consider={handleDndEvent}
              on:finalize={handleDndEvent}
            >
              {sectionTitles().map(({ title }, index) => (
                <div
                  id={`sidebar-entry-${index}`}
                  class={`${classes.sectionSidebarContainer} ${
                    isSidebarOpen() ? "" : classes.closed
                  }`}
                  onClick={() => handleScrollToSection(index)}
                >
                  {isSidebarOpen() && sectionEditorMode() ? (
                    <div
                      tabindex={dragDisabled() ? 0 : -1}
                      aria-label="drag-handle"
                      class={classes.dragHandle}
                      //@ts-ignore
                      on:mousedown={startDrag}
                      on:touchstart={startDrag}
                    >
                      <img src={DragHandleIcon} />
                    </div>
                  ) : null}
                  <span class={classes.sectionSidebarStatus}>
                    <img src={BlueCheckIcon} />
                  </span>
                  {isSidebarOpen() ? (
                    <span class={classes.sectionSidebarTitle}>{title}</span>
                  ) : null}
                  {isSidebarOpen() && sectionEditorMode() ? (
                    <img
                      src={GrayTrashIcon}
                      onClick={async (e) => {
                        e.stopPropagation();
                        editor.deleteSection(index);
                      }}
                      class={classes.deleteSectionIcon}
                    />
                  ) : null}
                </div>
              ))}
            </div>

            {isSidebarOpen() && sectionEditorMode() ? (
              <div class={classes.addSectionButton} onClick={addNewSection}>
                + Add section
              </div>
            ) : null}
          </div>
          <div
            class={`${classes.sidebarResizeHandle} ${isSidebarOpen() ? "" : classes.closed}`}
          ></div>
        </div>
        <Show when={isSidebarOpen()}>
          <div
            class={classes.mobileSidebarDarkFullscreenBackground}
            onClick={() => {
              setSectionEditorMode(false);
              setSidebarOpen(false);
            }}
          ></div>
        </Show>
        <div
          class={`${classes.documentAndSplitScreenContainer} ${
            splitScreenOrientation() ? classes.vertical : classes.horizontal
          }`}
        >
          <div
            ref={editorRoot}
            class={`${classes.documentBody} ${isSidebarOpen() ? classes.sidenavOpen : ""}`}
          ></div>
          <Show when={isSplitScreen()}>
            <div ref={editorRootSplitScreen} class={classes.otherUserSplitScreenContainer}>
              <div class={classes.splitScreenSwitcher}>
                <img
                  class={classes.changeSplitScreenOrientation}
                  src={SplitScreenIcon}
                  onClick={() => {
                    setSplitScreenOrientation(!splitScreenOrientation());
                  }}
                />
                <p>
                  Scott Appleman <img src={ArrowIcon} />
                </p>
                <img
                  class={classes.closeSplitScreen}
                  src={CloseXIcon}
                  onClick={() => {
                    setSplitScreen(false);
                  }}
                />
              </div>
            </div>
          </Show>
        </div>
      </div>
    </div>
  );
}
