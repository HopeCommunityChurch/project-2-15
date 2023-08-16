import { render } from "solid-js/web";
import * as classes from "./styles.module.scss";
import { Routes, Route, Router } from "@solidjs/router";

import { LandingHomePage } from "./Pages/LandingHomePage/LandingHomePage";
import { LoginPage } from "./Pages/LoginPage/login";
import { StudiesPage } from "./Pages/Studies/studies";
import {updateLoginState} from "./Pages/LoginPage/login";

updateLoginState();

export function App() {
  return (
    <div class= {classes.global}>
      <Routes>
        <Route path={["/", "/app"]} component={LandingHomePage}/>
        <Route path={"/app/login"} component={LoginPage}/>
        <Route path={"/app/studies"} component={StudiesPage}/>
      </Routes>
    </div>
  );
}

render(() => <Router><App></App></Router>, document.getElementById("root"));
