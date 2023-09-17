import {
  createEffect,
  createSignal,
  onMount,
  For,
  createResource,
  Show,
  onCleanup,
} from "solid-js";
import { TextEditorToolbar } from "./TextEditorToolbar/TextEditorToolbar";
import { match } from "ts-pattern";
import { useParams } from "@solidjs/router";
import { StudyTopNav } from "./StudyTopNav/StudyTopNav";
import BlueCheckIcon from "Assets/blue-check-icon.svg";
import GrayCircleIcon from "Assets/gray-circle-icon.svg";
import Arrow2Icon from "Assets/arrow2.svg";
import * as Network from "Utils/Network";
import * as classes from "./styles.module.scss";
import { PublicUser, Study, DocRaw } from "../../Types";
import * as Editor from "Editor/Editor";
import { LoginUser, loginState } from "Pages/LoginPage/login";
import { useNavigate } from "@solidjs/router";
import { throttle } from "@solid-primitives/scheduled";

async function getStudy(documentId): Promise<Network.NetworkState<DocRaw>> {
  return Network.request("/document/" + documentId);
}

export function StudyPage() {
  const nav = useNavigate();
  const documentID = useParams().documentID;
  const [result] = createResource([], () => getStudy(documentID), { initialValue: { state: "loading" } });

  createEffect( () => {
    console.log(result());
  });

  return <>
    {
      match(loginState() as LoginUser)
      .with({ state: "notLoggedIn" }, () => {
        nav("/app/login?redirect=/app/study/"+documentID);
        return <> </>;
      })
      .with({ state: "loggedIn" }, ({ user }) =>
        match(result())
          .with({ state: "loading" }, () => <></>)
          .with({ state: "error" }, ({body}) => <>{JSON.stringify(body)}</>)
          .with({ state: "success" }, ({ body }) => (StudyLoggedIn(body, user)))
          .with({ state: "notloaded" }, () => <>not loaded</>)
          .exhaustive()
      )
      .exhaustive()
    }
  </>;
}

function StudyLoggedIn(doc : DocRaw, currentUser : PublicUser) {
  const [isSidebarOpen, setSidebarOpen] = createSignal(false);
  const [isTopbarOpen, setTopbarOpen] = createSignal(true);
  const [sections, setSections] = createSignal([]);

  createEffect(() => {
    const isStudyPage = document.querySelector(`.${classes.documentBody}`);
    if (isStudyPage) {
      document.body.style.overflow = "hidden";
    }
  });

  // Function to handle the sidebar state based on viewport width
  const handleSidebarState = () => {
    if (window.innerWidth <= 750) {
      setSidebarOpen(false);
    } else {
      setSidebarOpen(true);
    }
  };

  let editorRoot: HTMLDivElement;
  let editor: Editor.P215Editor = new Editor.P215Editor(doc.document);
  const updateSignal = throttle( (change) =>
    Network.request("/document/"+ doc.docId , {
      method: "PUT",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify(change),
    }).then( (res) => {
      console.log(res);
    }).catch( (err) => {
      console.log(err);
    })
  , 500);
  editor.onUpdate( (value) => {
    updateSignal(value);
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
      sidebar.style.width = Math.max(Math.min(width, 500), 150) + "px";
    }

    editor.addEditor(editorRoot);

    // Cleanup listener when component is destroyed
    return () => {
      window.removeEventListener("resize", handleSidebarState);
    };
  });
  const sectionTitles = [];

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
            <div onClick={() => setSidebarOpen(!isSidebarOpen())} class={classes.sidebarToggle}>
              {isSidebarOpen() ? (
                <>
                  <img src={Arrow2Icon} />
                  <p>COLLAPSE</p>
                </>
              ) : (
                <img src={Arrow2Icon} class={classes.reverse} />
              )}
            </div>
            <div class={classes.allSectionSidebarContainer}>
              {sectionTitles.map((section) => (
                <div class={classes.sectionSidebarContainer}>
                  <span class={classes.sectionSidebarStatus}>
                    {section.Status === "Completed" ? (
                      <img src={BlueCheckIcon} />
                    ) : section.Status === "Not Completed" ? (
                      <img src={GrayCircleIcon} />
                    ) : null}
                  </span>
                  {isSidebarOpen() ? (
                    <span class={classes.sectionSidebarTitle}>{section.Title}</span>
                  ) : null}
                </div>
              ))}
            </div>
          </div>
          <div
            class={`${classes.sidebarResizeHandle} ${isSidebarOpen() ? "" : classes.closed}`}
          ></div>
        </div>
        <Show when={isSidebarOpen()}>
          <div
            class={classes.mobileSidebarDarkFullscreenBackground}
            onClick={() => setSidebarOpen(false)}
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

type ViewStudyProps = {
  study: Study;
};
