import { createEffect, createSignal, onMount, For, createResource } from "solid-js";
import { Button } from "../../Components/Button/Button";
import { PreLoginTopNav } from "../../Components/PreLoginTopNav/PreLoginTopNav";
import { match, P } from "ts-pattern";
import { useParams } from "@solidjs/router";
import { StudyTopNav } from "../../Components/StudyTopNav/StudyTopNav";

import * as Network from "../../Utils/Network";
import * as classes from "./styles.module.scss";
import { Study } from "../../Types";

const [isSidebarClosed, setSidebarClosed] = createSignal(false);

export function StudyPage() {
  const params = useParams();
  const documentID = params.documentID;
  console.log(documentID);
  return (
    <>
      <StudyTopNav />
      <div class={classes.pageBody}>
        <div class={`${classes.sidebar} ${isSidebarClosed() ? classes.closed : ""}`}>
          <button onClick={() => setSidebarClosed(!isSidebarClosed())}>
            {isSidebarClosed() ? "Open" : "Close"}
          </button>
        </div>

        <div class={classes.documentBody}>{documentID}</div>
      </div>
    </>
  );
}

type ViewStudyProps = {
  study: Study;
};
