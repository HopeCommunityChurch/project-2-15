import { render } from "solid-js/web";
import * as classes from "./styles.module.scss";
import { Routes, Route, Router } from "@solidjs/router";

import { LandingHomePage } from "./Pages/LandingHomePage/LandingHomePage";

export function App() {
  return (
    <div class= {classes.global}>
      <Routes>
        <Route path={["/", "/app"]} component={LandingHomePage}/>
      </Routes>
    </div>
  );
}

render(() => <Router><App></App></Router>, document.getElementById("root"));
