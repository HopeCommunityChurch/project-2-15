
import { createEffect, createSignal, onMount, Switch, Match } from "solid-js";
import { Button } from "../../Components/Button/Button";
import { PreLoginTopNav } from "../../Components/PreLoginTopNav/PreLoginTopNav";

import * as Network from "../../Utils/Network";
import * as classes from "./styles.module.scss";

const [loggedIn, setLogged] = createSignal(false);

export const loggedInSignal = loggedIn;


export function StudiesPage() {
  const initialLoginStatue = {
    state: "loading"
  };
  const [studiesState, setStudiesState] = createSignal(initialLoginStatue);

  onMount( () => {
    console.log("test");
  });


  return (
    <>
      <PreLoginTopNav />
      <div>
        <Switch fallback={<div>Hello world</div>} >
          <Match when={ studiesState().state == "loading"}>
            loading
          </Match>
        </Switch>
      </div>
    </>
  );
}
