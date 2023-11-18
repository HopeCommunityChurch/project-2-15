// Libraries and external modules
import { createEffect, createSignal, onMount, createResource, Show, onCleanup } from "solid-js";
import { useParams, useNavigate } from "@solidjs/router";
import { throttle } from "@solid-primitives/scheduled";
import { match } from "ts-pattern";
import { dndzone } from "solid-dnd-directive";

// Local imports
import * as Network from "Utils/Network";
import * as WS from "../../WebsocketTypes";
import * as classes from "./styles.module.scss";
import * as Editor from "Editor/Editor";
import { PublicUser, DocRaw, GroupStudyRaw, DocMetaRaw } from "Types";
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
import CouldntSaveImage from "./couldnt-save-error-image.svg";

// Network functions
async function getStudy(documentId): Promise<Network.NetworkState<DocRaw>> {
  return Network.request("/document/" + documentId);
}

async function getGroupStudy(groupStudyId): Promise<Network.NetworkState<GroupStudyRaw>> {
  return Network.request("/group-study/" + groupStudyId);
}


export function StudyPage() {
  const nav = useNavigate();
  const documentID = useParams().documentID;
  const [result] = createResource([], () => getStudy(documentID), {
    initialValue: { state: "loading" },
  });
  const groupStudyId = () => {
    return match(result())
            .with({ state: "success" }, ({body}) => {
              return body.groupStudyId;
            }).otherwise( () => null);
  };
  const [groupStudyResult] = createResource(groupStudyId, (gsId) => getGroupStudy(gsId), {
    initialValue: { state: "loading" },
  });


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
              .with({ state: "success" }, ({ body }) => {
                if(body.groupStudyId) {
                  return match(groupStudyResult())
                    .with({ state: "loading" }, () => <></>)
                    .with({ state: "notloaded" }, () => <>not loaded</>)
                    .with({ state: "error" }, ({ body }) =>
                      <>{JSON.stringify(body)}</>
                    ).with({ state: "success" }, ({ body: studyGroup }) => {
                      return StudyLoggedIn(body, user, studyGroup);
                    }).exhaustive();
                } else {
                  return StudyLoggedIn(body, user, null);
                }
               })
              .with({ state: "notloaded" }, () => <>not loaded</>)
              .exhaustive()
          )
          .exhaustive()
      }
    </>
  );
}


function StudyLoggedIn(doc: DocRaw, currentUser: PublicUser, groupStudy?: GroupStudyRaw) {
  // State and Variables
  const [isSidebarOpen, setSidebarOpen] = createSignal(false);
  const [isTopbarOpen, setTopbarOpen] = createSignal(true);
  const [isSplitScreen, setSplitScreen] = createSignal(false);
  const [sectionEditorMode, setSectionEditorMode] = createSignal(false);
  const [sectionTitles, setSectionTitles] = createSignal([]);
  const [dragDisabled, setDragDisabled] = createSignal(true);
  const [saving, setSaving] = createSignal<boolean>(false);
  const [savingError, setSavingError] = createSignal<string | null>(null);


  let websocket = new WebSocket("ws:/" + location.host + "/document/realtime");
  websocket.onopen = (e) => {
    console.log('ws open', e);
    WS.sendMsg(websocket, { tag: "OpenDoc", content: doc.docId})
  };
  websocket.onclose = (e) => {
    console.log('ws close', e);
  };
  websocket.onerror = (e) => {
    console.log('ws error', e);
  };

  onCleanup( () => {
    websocket.close();
  })

  const [lastUpdate, setLastUpdate] = createSignal<string>(doc.updated);

  const [activeEditor, setActiveEditor] = createSignal(null);

  let editorRoot: HTMLDivElement;
  let editor: Editor.P215Editor = new Editor.P215Editor({
    initDoc: doc.document,
    editable: true,
    activeEditor: activeEditor,
    setActiveEditor: setActiveEditor,
    remoteThings: {
      send: (steps: any) => {
        WS.sendMsg(websocket, { tag: "Updated", content: steps})
      }
    },
  });

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

  const savingContext = () => {
    const [savingData, setSavingData] = createSignal(null);
    const throttledSave = throttle((change) => {
      setSavingData(change);
    }, 1000);

    editor.onUpdate((value) => {
      throttledSave(value);
    });

    createEffect(() => {
      const data = savingData();
      if (saving() === true) return;
      if (data == null) return;
      setSaving(true);
      setSavingData(null);
      const updated = lastUpdate();
      Network.request<DocRaw>("/document/" + doc.docId, {
        method: "PUT",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify({
          document: data,
          lastUpdated: updated,
        }),
      })
        .then((res) => {
          //@ts-ignore
          match(res)
            .with({ state: "error" }, ({ body }) =>
              //@ts-ignore
              match(body)
                .with({ error: "DocumentUpdatedNotMatch" }, () => {
                  alert("Study have been updated on the server. Refreshing to get that version.");
                  location.reload();
                })
                .otherwise(() => setSavingError(JSON.stringify(body)))
            )
            .with({ state: "success" }, ({ body }) => {
              setLastUpdate(body.updated);
              setSavingError(null);
              setTimeout(() => setSaving(false), 1000);
            })
            .exhaustive();
        })
        .catch((err) => {
          setSaving(false);
          setTimeout(() => setSaving(false), 1000);
          setSavingError(err);
        });
    });
  };

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
  };

  const updateSectionTitles = (doc) => {
    const titlesWithId = doc.content.map((section, index) => {
      let header = section.content.find((innerSection) => {
        return innerSection.type === "sectionHeader";
      });

      // Check if header exists and has content property
      if (header && header.content && header.content.length > 0 && header.content[0].text) {
        return {
          title: header.content[0].text,
          id: index,
        };
      } else {
        return {
          title: "", // Return empty string if header or its content is null, undefined, or empty
          id: index,
        };
      }
    });

    setSectionTitles(titlesWithId);
  };

  onMount(() => {
    editor.addEditor(editorRoot);
    setActiveEditor(editor);
    const [documentThingy, setDocumentThingy] = createSignal(doc.document);
    createEffect(() => {
      updateSectionTitles(documentThingy());
    });
    editor.onUpdate((value) => {
      setDocumentThingy(value);
    });
    savingContext();
  });


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

  function ErrorOverlay({ savingError }) {
    return (
      <Show when={savingError()}>
        <div class={classes.errorOverlay}>
          <div class={classes.errorMessage}>
            <img src={CouldntSaveImage} />
            <h2>Can't Save</h2>
            Looks like a Jonah situation – we're in the belly of a tech whale. Trying to be 'spat'
            out soon!
            <a class={classes.bluelink} onClick={() => location.reload()}>
              Try refreshing the page to fix
            </a>
            <div>
              If this issue persists,{" "}
              <a
                href="https://forms.gle/koJrP31Vh9TfvPcq7"
                target="_blank"
                rel="noopener noreferrer"
              >
                Let us know.
              </a>
            </div>
          </div>
        </div>
      </Show>
    );
  }

  return (
    <div class={classes.wholePageContainer}>
      <StudyTopNav
        isSidebarOpen={isSidebarOpen}
        setSidebarOpen={setSidebarOpen}
        isTopbarOpen={isTopbarOpen}
        saving={saving}
        savingError={savingError}
        setLastUpdate={setLastUpdate}
        doc={doc}
      />
      <ErrorOverlay savingError={savingError} />
      <TextEditorToolbar
        editor={editor}
        isTopbarOpen={isTopbarOpen}
        setTopbarOpen={setTopbarOpen}
        isSplitScreen={isSplitScreen}
        setSplitScreen={setSplitScreen}
        activeEditor={activeEditor}
        groupStudy={groupStudy}
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
          class={`${classes.documentAndSplitScreenContainer} ${ classes.vertical}`}
        >
          <div
            ref={editorRoot}
            class={`${classes.documentBody} ${isSidebarOpen() ? classes.sidenavOpen : ""}`}
          ></div>
          <Show when={isSplitScreen()}>
            <SplitScreen groupStudy={groupStudy} setSplitScreen={setSplitScreen} currentUser={currentUser}/>
          </Show>
        </div>
      </div>
    </div>
  );
}

type SplitProps = {
  groupStudy: GroupStudyRaw,
  setSplitScreen: (arg: boolean) => void;
  currentUser: PublicUser
};

function getInitialSplit ( groupStudy: GroupStudyRaw, currentUser: PublicUser) : DocMetaRaw {
  return groupStudy.docs.find( (doc) => {
    return null != doc.editors.find( (ed) => ed.userId != currentUser.userId);
  })
}

function SplitScreen (props : SplitProps) {
  let editorRootSplitScreen: HTMLDivElement;
  const initialDocMeta = getInitialSplit(props.groupStudy, props.currentUser);

  return (
    <div ref={editorRootSplitScreen} class={classes.otherUserSplitScreenContainer}>
      <div class={classes.splitScreenSwitcher}>
        <p>
          Scott Appleman <img src={ArrowIcon} />
        </p>
        <img
          class={classes.closeSplitScreen}
          src={CloseXIcon}
          onClick={() => {
            props.setSplitScreen(false);
          }}
        />
      </div>
    </div>
  );
}
