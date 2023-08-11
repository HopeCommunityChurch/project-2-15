
import { createEffect, createSignal, onMount, Switch, Match, createResource } from "solid-js";
import { Button } from "../../Components/Button/Button";
import { PreLoginTopNav } from "../../Components/PreLoginTopNav/PreLoginTopNav";
import { match, P } from 'ts-pattern';

import * as Network from "../../Utils/Network";
import * as classes from "./styles.module.scss";

const [loggedIn, setLogged] = createSignal(false);

export const loggedInSignal = loggedIn;

type Study = {
  name: string;
  sutdyId: string;
  studyTemplateId: string;
  docs: {}[];
};

export function StudiesPage() {

  console.log("test");
  const [result] = createResource(
    [],
    () => Network.request("/study"),
    { initialValue: {state: "loading"} }
  )


  return (
    <>
      <PreLoginTopNav />
      <div>
      { match(result())
          .with({state: "loading"}, () =>
            (<>loading</>)
          )
          .with({state: "error"}, ({body}) =>
            (<>error</>)
          )
          .with({state: "success"}, ({body}) =>
            (<>success {JSON.stringify(body)}</>)
          )
          .with({state: "notloaded"}, () =>
            (<>not loaded</>)
          ).exhaustive()
        }
      </div>
    </>
  );
}
