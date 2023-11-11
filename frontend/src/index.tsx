import { render } from "solid-js/web";
import { createSignal, createEffect, onMount } from "solid-js";
import * as classes from "./styles.module.scss";
import { Routes, Route, Router } from "@solidjs/router";

import { LandingHomePage } from "./Pages/LandingHomePage/LandingHomePage";
import { LoginPage } from "./Pages/LoginPage/login";
import { SignUpPage } from "./Pages/SignUpPage/signup";
import { ResetPasswordPage } from "./Pages/ResetPasswordPage/resetpassword";
import { ResetPasswordTokenPage } from "./Pages/ResetPasswordToken/resetpassword";
import { StudiesPage } from "./Pages/Studies/studies";
import { StudyPage } from "./Pages/Study/study";
import { AdminArea } from "./Pages/AdminArea/AdminArea";
import { MyAccountPage } from "./Pages/MyAccountPage/MyAccountPage";
import { Four04 } from "./Pages/Four04/Four04";
import { updateLoginState } from "./Pages/LoginPage/login";

export function App() {
  return (
    <div class={classes.global}>
      <Routes>
        <Route path={["/", "/app"]} component={LandingHomePage} />
        <Route path={"/app/login"} component={LoginPage} />
        <Route path={"/app/resetpassword"} component={ResetPasswordPage} />
        <Route path={"/app/reset_token"} component={ResetPasswordTokenPage} />
        <Route path={"/app/signup"} component={SignUpPage} />
        <Route path={"/app/studies"} component={StudiesPage} />
        <Route path={"/app/admin"} component={AdminArea} />
        <Route path={"/app/account"} component={MyAccountPage} />
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

setInterval(() => {
  const script = document.getElementById("js-source") as HTMLScriptElement;
  if (script) {
    let src = script.src;
    fetch(src, {
      method: "HEAD",
    }).then((response) => {
      let status = response.status;
      if (status === 200) {
        return;
      }
      if (status == 404) {
        alert("New version of p215 found. To ensure nothing breaks forcing an update.");
        location.reload();
      }
    });
  }
}, 60_000);
