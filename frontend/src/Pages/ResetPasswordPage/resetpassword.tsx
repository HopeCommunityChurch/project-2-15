import { createEffect, createSignal } from "solid-js";
import { useNavigate } from "@solidjs/router";
import { A } from "@solidjs/router";

import * as Network from "../../Utils/Network";
import * as classes from "./styles.module.scss";

import laptopMockup from "./laptop-mockup.png";
import p215Logo from "./P215.png";
import { PublicUser } from "../../Types";
import { match } from "ts-pattern";

export type NotLoggedIn = {
  state: "notLoggedIn";
};

export type UserLoggedIn = {
  state: "loggedIn";
  user: PublicUser;
};

export type LoginUser = UserLoggedIn | NotLoggedIn;

const notLoggedIn = {
  state: "notLoggedIn",
};

const [loginStateLocal, setLoginState] = createSignal(notLoggedIn);

export const loginState = loginStateLocal;

export function updateLoginState(): void {
  Network.request<PublicUser>("/user/me", {
    method: "GET",
    headers: {
      "Content-Type": "application/json",
    },
  }).then((result) => {
    // @ts-ignore
    match(result)
      .with({ state: "success" }, ({ body }) => {
        setLoginState({
          state: "loggedIn",
          user: body,
        });
      })
      .with({ state: "error" }, ({ body }) => {
        // @ts-ignore
        match(body)
          .with({ status: 401 }, () =>
            setLoginState({
              state: "notLoggedIn",
            })
          )
          .otherwise((re) => console.error(re));
      })
      .exhaustive();
  });
}

export function ResetPasswordPage() {
  const initialLoginStatue = {
    state: "NotLoaded",
  };
  const [email, setEmail] = createSignal("");
  const [password, setPassword] = createSignal("");
  const [showP, setShowP] = createSignal(false);
  const [loginError, setLoginError] = createSignal<Network.NetworkError | Network.NetworkNotLoaded>(
    Network.notLoaded
  );
  const nav = useNavigate();

  const loginPushed = (e: Event) => {
    e.preventDefault();
    Network.request("/auth/password", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify({
        email: email(),
        password: password(),
      }),
    })
      .then((result) => {
        // @ts-ignore
        match(result)
          .with({ state: "error" }, (res) => {
            setLoginError(res);
          })
          .with({ state: "success" }, () => {
            updateLoginState();
            nav("/app/studies");
          })
          .exhaustive();
      })
      .catch((err) => {
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
          <form onSubmit={(e) => loginPushed(e)}>
            <label for="username">Email</label>
            <input type="email" id="username" onKeyUp={(e) => setEmail(e.currentTarget.value)} />

            <p>
              Remember you account? <A href="/app/login">Login</A>
            </p>
            <button onSubmit={(e) => loginPushed(e)} type="submit">
              Reset Password
            </button>
            {
              // @ts-ignore
              match(loginError())
                .with({ state: "notloaded" }, () => <> </>)
                .with({ state: "error" }, ({ body }) =>
                  // @ts-ignore
                  match(body)
                    .with({ error: "AuthError" }, () => <div>email or password wrong</div>)
                    .otherwise((err) => (
                      <div>
                        You shouldn't hit this error so here it is raw:
                        {JSON.stringify(err)}
                      </div>
                    ))
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
