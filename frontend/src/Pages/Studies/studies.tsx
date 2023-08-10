
import { createEffect, createSignal, onMount, Switch, Match } from "solid-js";
import { Button } from "../../Components/Button/Button";
import { PreLoginTopNav } from "../../Components/PreLoginTopNav/PreLoginTopNav";

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
  const initialLoginStatue = {
    state: "loading"
  };
  const [studiesState, setStudiesState] = createSignal(initialLoginStatue);

  console.log("test");
  Network.request("/study").then( (result) => {
    setStudiesState(result);
  })


  return (
    <>
      <PreLoginTopNav />
      <div>
        <Switch fallback={<div>Hello world</div>} >
          <Match when={ studiesState().state == "loading"}>
            loading
          </Match>
          <Match when={ studiesState().state === "success"}>
            test
          </Match>
          <Match when={ studiesState().state === "error"}>
            some error
          </Match>
        </Switch>
      </div>
    </>
  );
}
