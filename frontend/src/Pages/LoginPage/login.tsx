import { Show, createEffect, createSignal } from "solid-js";
import { Button } from "../../Components/Button/Button";
import { PreLoginTopNav } from "../../Components/PreLoginTopNav/PreLoginTopNav";
import { useNavigate } from "@solidjs/router";

import * as Network from "../../Utils/Network";
import * as classes from "./styles.module.scss";

import blueCircles from "./background-blue-circles.png";
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

export function LoginPage() {
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
            console.log(result);
            updateLoginState();
            nav("/app/studies");
          })
          .exhaustive();
      })
      .catch((err) => {
        console.error(err);
      });
  };

  createEffect(() => {
    console.log(loginError());
  });

  return (
    <div class={classes.gridContainer}>
      <div class={classes.leftColumn}>
        <div class={classes.innerLeftColumn}>
          <img src={p215Logo} alt="Logo" />
          <h1>Log In</h1>
          <p>Please enter your login credentials below to start using the admin console.</p>
          <form onSubmit={(e) => loginPushed(e)}>
            <label for="username">Username</label>
            <input type="text" id="username" onKeyUp={(e) => setEmail(e.currentTarget.value)} />
            <label for="password">Password</label>
            <input
              type={showP() ? "text" : "password"}
              id="password"
              onKeyUp={(e) => setPassword(e.currentTarget.value)}
            />
            <div class={classes.formGroup}>
              <div>
                <label class={classes.checkboxContainer}>
                  View Password
                  <input
                    type="checkbox"
                    onChange={(e) => setShowP(e.currentTarget.checked)}
                    id="viewPassword"
                  />
                  <span class={classes.checkmark}></span>
                </label>
              </div>

              <a href="#">Forgot Password?</a>
            </div>
            <p class={classes.dontHaveAccount}>
              Don't have an account? <a href="#">Sign up</a>
            </p>
            <button onSubmit={(e) => loginPushed(e)} type="submit">
              Log In
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
