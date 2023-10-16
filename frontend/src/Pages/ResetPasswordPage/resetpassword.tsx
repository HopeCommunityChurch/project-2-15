import { createEffect, createSignal } from "solid-js";
import { useNavigate } from "@solidjs/router";
import { A } from "@solidjs/router";

import * as Network from "../../Utils/Network";
import * as classes from "./styles.module.scss";

import laptopMockup from "./laptop-mockup.png";
import p215Logo from "./P215.png";
import { PublicUser } from "../../Types";
import { match } from "ts-pattern";



export function ResetPasswordPage() {
  const [email, setEmail] = createSignal("");
  const [resetResult, setResetResult] = createSignal<
    Network.NetworkError
    | Network.NetworkNotLoaded
    | Network.NetworkSuccess<null>
    >(
    Network.notLoaded
  );

  const nav = useNavigate();

  const resetPushed = (e: Event) => {
    e.preventDefault();
    Network.request("/auth/password_reset", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify({
        email: email(),
      }),
    }).then((result : Network.SimpleNetworkState<null>) => {
        setResetResult(result);
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
            <label for="username">Email</label>
            <input type="email" id="username" onKeyUp={(e) => setEmail(e.currentTarget.value)} />
            <p>
              Remember your account? <A href="/app/login">Login</A>
            </p>
            <button onSubmit={(e) => resetPushed(e)} type="submit">
              Reset Password
            </button>
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
