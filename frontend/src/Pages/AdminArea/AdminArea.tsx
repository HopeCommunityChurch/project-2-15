import { createEffect, createSignal, onMount, For, createResource } from "solid-js";
import { Button } from "../../Components/Button/Button";

import { DateTimeFormatter } from "@js-joda/core";
import { Locale } from "@js-joda/locale_en-us";
import { AdminTopNav } from "../../Components/AdminTopNav/AdminTopNav";
import { match, P } from "ts-pattern";
import { useNavigate } from "@solidjs/router";

import * as Network from "../../Utils/Network";
import * as classes from "./styles.module.scss";
import { PublicUser, Study, StudyRaw, toStudyFromRaw } from "../../Types";

import { LoginUser, loginState } from "../LoginPage/login";

async function getStudies(): Promise<Network.NetworkState<Array<Study>>> {
  return Network.request("/study").then((study) =>
    // This is ugly, need a better way to do this.
    Network.mapNetworkState(study, (s: Array<StudyRaw>) => s.map(toStudyFromRaw))
  );
}

export function AdminArea() {
  const nav = useNavigate();
  //@ts-ignore
  return match(loginState() as LoginUser)
    .with({ state: "notLoggedIn" }, () => {
      nav("/app/login");
      return <> </>;
    })
    .with({ state: "loggedIn" }, ({ user }) => Studies(user))
    .exhaustive();
}

function Studies(me: PublicUser) {
  const [result] = createResource([], () => getStudies(), { initialValue: { state: "loading" } });
  return (
    <>
      <AdminTopNav />
      <div class={classes.studiesPage}>
        <div class={classes.pageHeader}>
          <h1>Study Tempaltes</h1>
          <a href="/app/trash">View Trash</a>
        </div>
        <table class={classes.tableContainer}>
          <thead>
            <tr>
              <th>Study Title</th>
              <th>Shared with</th>
              <th>Last Opened by Me</th>
            </tr>
          </thead>
          <tbody>
            {
              // @ts-ignore
              match(result())
                .with({ state: "loading" }, () => <>loading</>)
                .with({ state: "error" }, ({ body }) => <>error</>)
                .with({ state: "success" }, ({ body }) => (
                  <For each={body}>{(study) => <ViewStudy study={study} me={me} />}</For>
                ))
                .with({ state: "notloaded" }, () => <>not loaded</>)
                .exhaustive()
            }
          </tbody>
        </table>
      </div>
    </>
  );
}

type ViewStudyProps = {
  study: Study;
  me: PublicUser;
};

const dtFormat = DateTimeFormatter.ofPattern("MMMM d, yyyy").withLocale(Locale.US);

function ViewStudy(props: ViewStudyProps) {
  console.log("test");
  const nav = useNavigate();
  const peoples = props.study.docs.flatMap((doc) =>
    doc.editors.filter((e) => e.userId != props.me.userId).map((e) => e.name)
  );
  let shared = "no one";
  if (peoples.length > 0) {
    // I think this should intersperse, I haven't tried it. Should probably put
    // this in a utilities file somewhere.
    shared = peoples.slice(1).reduce((prev, cur) => cur + ", " + prev, peoples[0]);
  }
  const myDoc = props.study.docs.find((doc) =>
    doc.editors.some((e) => e.userId == props.me.userId)
  );
  const updated = myDoc.updated.format(dtFormat);
  return (
    <tr class={classes.tableRow} onClick={() => (window.location.href = "/details")}>
      <td>{props.study.name}</td>
      <td class={classes.sharedWith}>{shared}</td>
      <td>{updated}</td>
    </tr>
  );
}
