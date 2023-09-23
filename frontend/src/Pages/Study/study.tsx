// Libraries and external modules
import { createEffect, createSignal, onMount, createResource, Show } from "solid-js";
import { useParams, useNavigate } from "@solidjs/router";
import { throttle } from "@solid-primitives/scheduled";
import { match } from "ts-pattern";
// import { dndzone } from "solid-dnd-directive";

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

  createEffect(() => {
    console.log(result());
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
  const [sectionEditorMode, setSectionEditorMode] = createSignal(false);
  const [sectionTitles, setSectionTitles] = createSignal([]);

  let editorRoot: HTMLDivElement;
  let editor: Editor.P215Editor = new Editor.P215Editor(doc.document);

  // Effects and Mounts

  createEffect(() => {
    const isStudyPage = document.querySelector(`.${classes.documentBody}`);
    if (isStudyPage) {
      document.body.style.overflow = "hidden";
    }
  });

  onMount(() => {
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
  });

  onMount(() => {
    // Only store title strings and their order
    const titlesWithOrder = doc.document.content.map((section, index) => ({
      title: section.header,
      order: index,
    }));
    setSectionTitles(titlesWithOrder);
  });

  editor.onUpdate((value) => {
    updateSignal(value);
  });

  // Helper Functions
  const handleSidebarState = () => {
    if (window.innerWidth <= 750) {
      setSidebarOpen(false);
    } else {
      setSidebarOpen(true);
    }
  };

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
          console.log(res);
        })
        .catch((err) => {
          console.log(err);
        }),
    500
  );

  const handleScrollToSection = (sectionIndex) => {
    const element = document.getElementById(`section-${sectionIndex}`);
    if (element) {
      element.scrollIntoView({ behavior: "smooth" });
    }
    console.log(sectionIndex);
  };

  const addNewSection = () => {
    const newSection = {
      header: "Untitled",
      bibleSections: [
        {
          book: "book",
          verses: "verses",
          text: "Add Verses \n",
        },
      ],
    };

    editor.addSection(newSection);

    setSectionTitles((prevTitles) => [
      ...prevTitles,
      {
        title: newSection.header,
        order: prevTitles.length,
      },
    ]);
  };

  return (
    <>
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
      />
      <div class={`${classes.pageBody} ${isTopbarOpen() ? "" : classes.collapsed}`}>
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
            <div class={classes.allSectionSidebarContainer}>
              {sectionTitles().map(({ title }, index) => (
                <div
                  class={`${classes.sectionSidebarContainer} ${
                    isSidebarOpen() ? "" : classes.closed
                  }`}
                  onClick={() => handleScrollToSection(index)}
                >
                  {isSidebarOpen() && sectionEditorMode() ? <img src={DragHandleIcon} /> : null}
                  <span class={classes.sectionSidebarStatus}>
                    <img src={BlueCheckIcon} />
                  </span>
                  {isSidebarOpen() ? (
                    <span class={classes.sectionSidebarTitle}>{title}</span>
                  ) : null}
                  {isSidebarOpen() && sectionEditorMode() ? <img src={GrayTrashIcon} /> : null}
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
          ref={editorRoot}
          class={`${classes.documentBody} ${isSidebarOpen() ? classes.sidenavOpen : ""}`}
        ></div>
      </div>
    </>
  );
}
