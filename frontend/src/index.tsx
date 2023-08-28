import { render } from "solid-js/web";
import { createSignal, createEffect, onMount } from "solid-js";
import * as classes from "./styles.module.scss";
import { Routes, Route, Router } from "@solidjs/router";

import { LandingHomePage } from "./Pages/LandingHomePage/LandingHomePage";
import { LoginPage } from "./Pages/LoginPage/login";
import { SignUpPage } from "./Pages/SignUpPage/signup";
import { ResetPasswordPage } from "./Pages/ResetPasswordPage/resetpassword";
import { StudiesPage } from "./Pages/Studies/studies";
import { StudyPage } from "./Pages/Study/study";
import { AdminArea } from "./Pages/AdminArea/AdminArea";
import { Four04 } from "./Pages/Four04/Four04";
import { updateLoginState } from "./Pages/LoginPage/login";

export function App() {
  return (
    <div class={classes.global}>
      <Routes>
        <Route path={["/", "/app"]} component={LandingHomePage} />
        <Route path={"/app/login"} component={LoginPage} />
        <Route path={"/app/resetpassword"} component={ResetPasswordPage} />
        <Route path={"/app/signup"} component={SignUpPage} />
        <Route path={"/app/studies"} component={StudiesPage} />
        <Route path={"/app/admin"} component={AdminArea} />
        <Route path={"/app/study/:documentID"} component={StudyPage} />
        <Route path="*" component={Four04} />
      </Routes>
    </div>
  );
}

updateLoginState().then(() => {
  render(
    () => (
      <Router>
        <App></App>
      </Router>
    ),
    document.getElementById("root")
  );
});
