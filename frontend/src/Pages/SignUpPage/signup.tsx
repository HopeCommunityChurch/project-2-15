import { createEffect, createSignal } from "solid-js";
import { useNavigate } from "@solidjs/router";
import { A } from "@solidjs/router";

import * as Network from "../../Utils/Network";
import * as classes from "./styles.module.scss";

import laptopMockup from "./laptop-mockup.png";
import p215Logo from "./P215.png";
import { match } from "ts-pattern";
import { updateLoginState } from "../LoginPage/login";

const churchId = "a2a712c7-5812-4371-8fa3-2edee0c541cf";

export function SignUpPage() {
  const [name, setName] = createSignal("");
  const [email, setEmail] = createSignal("");
  const [password, setPassword] = createSignal("");
  const [password2, setPassword2] = createSignal("");
  const [showP, setShowP] = createSignal(false);

  const [signupError, setSignupError] = createSignal<null | string>(null);

  const nav = useNavigate();

  const signupPushed = (e: Event) => {
    e.preventDefault();

    // Validation
    if (password() != password2()) {
      setSignupError("passwords don't match");
      return;
    }

    if (password().length < 9) {
      setSignupError("passwords must be more than 8 characters long");
      return;
    }

    if (email() == "") {
      setSignupError("empty email address");
      return;
    }

    if (name() == "") {
      setSignupError("empty name");
      return;
    }

    Network.request("/auth/register", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify({
        email: email().trim(),
        password: password(),
        churchId: churchId,
        name: name().trim(),
      }),
    })
      .then((result) => {
        // @ts-ignore
        match(result)
          .with({ state: "error" }, (res) => {
            setSignupError(res.toString());
          })
          .with({ state: "success" }, () => {
            updateLoginState().then( () => {
              nav("/app/studies");
            });
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
          <h1>Sign Up</h1>
          <p>Please enter your login credentials below to start using the admin console.</p>
          <form onSubmit={(e) => signupPushed(e)}>
            <label for="name">Name</label>
            <input type="text" id="name" onKeyUp={(e) => setName(e.currentTarget.value)} />
            <label for="email">Email</label>
            <input type="email" id="email" onKeyUp={(e) => setEmail(e.currentTarget.value)} />
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
            <p>
              Already have an account? <A href="/app/login">Login</A>
            </p>
            <button onSubmit={(e) => signupPushed(e)} type="submit">
              Sign Up
            </button>
            {signupError() ? <div class={classes.errorText}>{signupError()}</div> : <> </>}
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
