import { createEffect, createSignal, onMount, For, createResource, Show } from "solid-js";
import { Button } from "../../Components/Button/Button";
import { PreLoginTopNav } from "../../Components/PreLoginTopNav/PreLoginTopNav";
import { TextEditorToolbar } from "../../Components/TextEditorToolbar/TextEditorToolbar";
import { match, P } from "ts-pattern";
import { useParams } from "@solidjs/router";
import { StudyTopNav } from "../../Components/StudyTopNav/StudyTopNav";
import BlueCheckIcon from "../../Assets/blue-check-icon.svg";
import GrayCircleIcon from "../../Assets/gray-circle-icon.svg";
import Arrow2Icon from "../../Assets/arrow2.svg";
import * as Network from "../../Utils/Network";
import * as classes from "./styles.module.scss";
import { Study } from "../../Types";

export function StudyPage() {
  const [isSidebarOpen, setSidebarOpen] = createSignal(false);
  const documentID = useParams().documentID;
  const sectionTitles = [
    { Title: "1:1–17", Status: "Completed" },
    { Title: "1:18–2:23", Status: "Completed" },
    { Title: "3:1–12", Status: "Completed" },
    { Title: "3:13–4:11", Status: "Completed" },
    { Title: "4:12–25", Status: "Completed" },
    { Title: "5–7", Status: "Not Completed" },
    { Title: "8:1–15", Status: "Not Completed" },
    { Title: "8:16––11:1", Status: "Not Completed" },
    { Title: "11:2–19", Status: "Not Completed" },
    { Title: "11:20–30", Status: "Not Completed" },
    { Title: "12", Status: "Not Completed" },
    { Title: "13:1–52", Status: "Not Completed" },
    { Title: "13:53–16:12", Status: "Not Completed" },
    { Title: "16:13–17:13", Status: "Not Completed" },
    { Title: "17:14–20:34", Status: "Not Completed" },
    { Title: "21:1–22", Status: "Not Completed" },
    { Title: "21:23–22:46", Status: "Not Completed" },
    { Title: "Long name just in case it happens need to test haha", Status: "Not Completed" },
    { Title: "24–25", Status: "Not Completed" },
    { Title: "26–27", Status: "Not Completed" },
    { Title: "28:1–8", Status: "Not Completed" },
    { Title: "28:9–10", Status: "Not Completed" },
    { Title: "28:11–15", Status: "Not Completed" },
    { Title: "28:16–20", Status: "Not Completed" },
  ];

  // Function to handle the sidebar state based on viewport width
  const handleSidebarState = () => {
    if (window.innerWidth <= 750) {
      setSidebarOpen(false);
    } else {
      setSidebarOpen(true);
    }
  };
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

    // Cleanup listener when component is destroyed
    return () => {
      window.removeEventListener("resize", handleSidebarState);
    };
  });

  return (
    <>
      <StudyTopNav isSidebarOpen={isSidebarOpen} setSidebarOpen={setSidebarOpen} />
      <TextEditorToolbar />
      <div class={classes.pageBody}>
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

        <div class={`${classes.documentBody} ${isSidebarOpen() ? classes.sidenavOpen : ""}`}>
          {documentID}
          <br />
          Insert the document processor here
        </div>
      </div>
    </>
  );
}

type ViewStudyProps = {
  study: Study;
};
