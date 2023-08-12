import { createEffect, createSignal } from "solid-js";
import { Button } from "../../Components/Button/Button";
import { PreLoginTopNav } from "../../Components/PreLoginTopNav/PreLoginTopNav";
import { useNavigate } from "@solidjs/router";

import * as Network from "../../Utils/Network";
import * as classes from "./styles.module.scss";

import blueCircles from "./background-blue-circles.png";
import laptopMockup from "./laptop-mockup2.png";
import p215Logo from "./p215.png";

const [loggedIn, setLogged] = createSignal(false);

export const loggedInSignal = loggedIn;

export function LoginPage() {
  const initialLoginStatue = {
    state: "NotLoaded",
  };
  const [login, setLogin] = createSignal(initialLoginStatue);
  const [email, setEmail] = createSignal("");
  const [password, setPassword] = createSignal("");
  const nav = useNavigate();

  const loginPushed = () => {
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
        switch (result.state) {
          case "error":
            break;
          case "success":
            console.log(result);
            setLogged(true);
            nav("/app/studies");
            break;
        }
      })
      .catch((err) => {
        console.log(err);
      });
  };

  return (
    <div class={classes.gridContainer}>
      <div class={classes.leftColumn}>
        <div class={classes.innerLeftColumn}>
          <img src={p215Logo} alt="Logo" />
          <h1>Log In</h1>
          <p>
            Welcome to SKED. Please enter your login credentials below to start using the admin
            console.
          </p>
          <form>
            <label for="username">Username</label>
            <input type="text" id="username" />
            <label for="password">Password</label>
            <input type="password" id="password" />
            <div class={classes.formGroup}>
              <div class={classes.checkboxGroup}>
                <label class={classes.checkboxContainer}>
                  View Password
                  <input type="checkbox" id="viewPassword" />
                  <span class={classes.checkmark}></span>
                </label>
              </div>

              <a href="#" class={classes.forgotLink}>
                Forgot Password?
              </a>
            </div>
            <p class={classes.dontHaveAccount}>
              Don't have an account?{" "}
              <a href="#" class={classes.signUpLink}>
                Sign up
              </a>
            </p>

            <button type="submit">Log In</button>
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
