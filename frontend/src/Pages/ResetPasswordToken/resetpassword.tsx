import { createEffect, createSignal } from "solid-js";
import { useNavigate } from "@solidjs/router";
import { A } from "@solidjs/router";
import { updateLoginState } from "../LoginPage/login";

import * as Network from "../../Utils/Network";
import * as classes from "./styles.module.scss";

import laptopMockup from "./laptop-mockup.png";
import p215Logo from "./P215.png";
import { PublicUser } from "../../Types";
import { match } from "ts-pattern";



export function ResetPasswordTokenPage() {

  const [password, setPassword] = createSignal("");
  const [password2, setPassword2] = createSignal("");
  const [showP, setShowP] = createSignal(false);

  const [signupError, setSignupError] = createSignal<null | string>(null);
  const [resetResult, setResetResult] = createSignal<
    Network.NetworkError
    | Network.NetworkNotLoaded
    | Network.NetworkSuccess<null>
    >(
    Network.notLoaded
  );
  let token = (new URLSearchParams(location.search)).get("token");

  const nav = useNavigate();

  const resetPushed = (e: Event) => {
    e.preventDefault();
    if (password() != password2()) {
      setSignupError("passwords don't match");
      return;
    }

    if (password().length < 9) {
      setSignupError("passwords must be more than 8 characters long");
      return;
    }
    setSignupError(null);

    Network.request("/auth/password_reset_token", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify({
        token: token,
        password: password(),
      }),
    }).then((result : Network.SimpleNetworkState<null>) => {
      match(result)
      .with({state: "error"}, (body) =>
        setResetResult(body)
      ).with({state: "success"}, () =>
        updateLoginState().then( () => {
          nav("/app/studies");
        })
      ).exhaustive();
    }).catch((err) => {
      console.error(err);
    });
  };

  return (
    <div class={classes.gridContainer}>
      <div class={classes.leftColumn}>
        <div class={classes.innerLeftColumn}>
          <A href="/app/">
            <img src={p215Logo} alt="Logo" />
          </A>
          <h1>Reset Password</h1>
          <p>
            Please provide the email address that you used when you signed up for your account. If
            you forgot your email, please <a>contact us</a>
          </p>
          <form onSubmit={(e) => resetPushed(e)}>
            <label for="password">Password</label>
            <input
              type={showP() ? "text" : "password"}
              id="password"
              onKeyUp={(e) => setPassword(e.currentTarget.value)}
            />
            <label for="password2">Repeat Password</label>
            <input
              type={showP() ? "text" : "password"}
              id="password2"
              onKeyUp={(e) => setPassword2(e.currentTarget.value)}
            />
            <div class={classes.formGroup}>
              <div>
                <label class={classes.checkboxContainer}>
                  View Passwords
                  <input
                    type="checkbox"
                    onChange={(e) => setShowP(e.currentTarget.checked)}
                    id="viewPassword"
                  />
                  <span class={classes.checkmark}></span>
                </label>
              </div>
            </div>
            <button onSubmit={(e) => resetPushed(e)} type="submit">
              Reset Password
            </button>
            {signupError() ? <div class={classes.errorText}>{signupError()}</div> : <> </>}
            {
              match(resetResult())
                .with({ state: "notloaded" }, () => <> </>)
                .with({ state: "error" }, ({ body }) =>
                  <div>
                    You shouldn't hit this error so here it is raw:
                    {JSON.stringify(body)}
                  </div>
                )
                .with({ state: "success" }, ({ body }) =>
                  <div>
                    Check your email for a password reset email.
                    <br />
                    If you don't see it check the spam folder.
                  </div>
                )
                .exhaustive()
            }
          </form>
        </div>
      </div>
      <div class={classes.rightColumn}>
        <div class={classes.outerContainer}>
          <div class={classes.backgroundGradient}>
            <div class={classes.backgroundImage}>
              <div class={classes.centeredDiv}>
                <h2>Making Bible Study Simpler than Ever!</h2>
                <p>
                  Take the complexity out of Bible study and delve deeper into the Word with others
                </p>
                <img src={laptopMockup} />
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
