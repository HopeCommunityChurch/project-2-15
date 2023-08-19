import { createEffect, createSignal, onMount, For, createResource } from "solid-js";
import { Button } from "../../Components/Button/Button";
import { LoggedInTopNav } from "../../Components/LoggedInTopNav/LoggedInTopNav";
import { match, P } from "ts-pattern";

import * as Network from "../../Utils/Network";
import * as classes from "./styles.module.scss";
import { Study } from "../../Types";

const [loggedIn, setLogged] = createSignal(false);

export const loggedInSignal = loggedIn;

async function getStudies(): Promise<Network.NetworkState<Array<Study>>> {
  return Network.request("/study");
}

export function StudiesPage() {
  console.log("test");
  const [result] = createResource([], () => getStudies(), { initialValue: { state: "loading" } });

  return (
    <>
      <LoggedInTopNav />
      <div>
        {
          // @ts-ignore
          match(result())
            .with({ state: "loading" }, () => <>loading</>)
            .with({ state: "error" }, ({ body }) => <>error</>)
            .with({ state: "success" }, ({ body }) => (
              <div>
                <For each={body}>{(study) => <ViewStudy study={study} />}</For>
              </div>
            ))
            .with({ state: "notloaded" }, () => <>not loaded</>)
            .exhaustive()
        }
      </div>
    </>
  );
}

type ViewStudyProps = {
  study: Study;
};

function ViewStudy(props: ViewStudyProps) {
  return <div>{props.study.name}</div>;
}
