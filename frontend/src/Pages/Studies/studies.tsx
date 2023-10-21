import { createEffect, createSignal, onMount, For, createResource } from "solid-js";

import { DateTimeFormatter } from "@js-joda/core";
import { Locale } from "@js-joda/locale_en-us";
import { StudiesTopNav } from "./StudiesTopNav/StudiesTopNav";
import { match, P } from "ts-pattern";
import { useNavigate } from "@solidjs/router";

import * as Network from "Utils/Network";
import * as classes from "./styles.module.scss";
import * as T from "../../Types";

import { LoginUser, loginState } from "../LoginPage/login";

async function getStudies(): Promise<Network.NetworkState<Array<T.Doc>>> {
  return Network.request("/document").then((study) =>
    // This is ugly, need a better way to do this.
    Network.mapNetworkState(study, (s: Array<T.DocRaw>) => s.map(T.toDocFromRaw))
  );
}

export function StudiesPage() {
  const nav = useNavigate();
  //@ts-ignore
  return match(loginState() as LoginUser)
    .with({ state: "notLoggedIn" }, () => {
      nav("/app/login?redirect=/app/studies");
      return <> </>;
    })
    .with({ state: "loggedIn" }, ({ user }) => Studies(user))
    .exhaustive();
}

function Studies(currentUser: T.PublicUser) {
  const [result] = createResource([], () => getStudies(), { initialValue: { state: "loading" } });
  return (
    <>
      <StudiesTopNav currentUser={currentUser} />
      <div class={classes.studiesPage}>
        <div class={classes.pageHeader}>
          <h1>My Studies</h1>
        </div>
        <table class={classes.tableContainer}>
          <thead>
            <tr>
              <th>Study Title</th>
              <th>Group Study</th>
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
                  <For each={body}>
                    {(doc) => <ViewStudy doc={doc} currentUser={currentUser} />}
                  </For>
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
  doc: T.Doc;
  currentUser: T.PublicUser;
};

const dtFormat = DateTimeFormatter.ofPattern("MMMM d, yyyy").withLocale(Locale.US);

function ViewStudy(props: ViewStudyProps) {
  const nav = useNavigate();
  let shared = "Independent";
  if (props.doc.groupStudyName) {
    shared = props.doc.groupStudyName;
  }
  const updated = props.doc.updated.format(dtFormat);
  const url = "/app/study/" + props.doc.docId;
  return (
    <tr class={classes.tableRow} onClick={() => nav(url)}>
      <td>{props.doc.name}</td>
      <td class={classes.sharedWith}>{shared}</td>
      <td>{updated}</td>
    </tr>
  );
}
