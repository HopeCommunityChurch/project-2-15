import { createEffect, createSignal, onMount, For, createResource } from "solid-js";
import { Button } from "../../Components/Button/Button";
import { PreLoginTopNav } from "../../Components/PreLoginTopNav/PreLoginTopNav";
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
  const [isSidebarClosed, setSidebarClosed] = createSignal(false);
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

  return (
    <>
      <StudyTopNav />
      <div class={classes.pageBody}>
        <div class={`${classes.sidebar} ${isSidebarClosed() ? classes.closed : ""}`}>
          <div onClick={() => setSidebarClosed(!isSidebarClosed())} class={classes.sidebarToggle}>
            {isSidebarClosed() ? (
              <img src={Arrow2Icon} class={classes.reverse} />
            ) : (
              <>
                <img src={Arrow2Icon} />
                <p>COLLAPSE</p>
              </>
            )}
          </div>
          {sectionTitles.map((section) => (
            <div class={classes.sectionSidebarContainer}>
              <span class={classes.sectionSidebarStatus}>
                {section.Status === "Completed" ? (
                  <img src={BlueCheckIcon} />
                ) : section.Status === "Not Completed" ? (
                  <img src={GrayCircleIcon} />
                ) : null}
              </span>
              {isSidebarClosed() ? null : (
                <span class={classes.sectionSidebarTitle}>{section.Title}</span>
              )}
            </div>
          ))}
        </div>

        <div class={classes.documentBody}>{documentID}</div>
      </div>
    </>
  );
}

type ViewStudyProps = {
  study: Study;
};
