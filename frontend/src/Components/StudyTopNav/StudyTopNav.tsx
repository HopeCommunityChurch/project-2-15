import Logo from "../../Assets/p215-circle.svg";
import { Button } from "../Button/Button";
import { createSignal, createEffect, Show, onCleanup } from "solid-js";
import { A, useNavigate } from "@solidjs/router";
import { loginState, LoginUser } from "../../Pages/LoginPage/login";

import * as classes from "./styles.module.scss";
import { match } from "ts-pattern";
import useClickOutsideClose from "../../Hooks/useOutsideClickClose";

export function StudyTopNav() {
  const [showDropdown, setShowDropdown] = createSignal(false);

  createEffect(() => {
    useClickOutsideClose(showDropdown, setShowDropdown, classes.profileDropdown);
  });

  const nav = useNavigate();

  return (
    <>
      <header class={classes.header}>
        <img class={classes.logo} src={Logo} />
        <div class={classes.studyHeaderText}>
          <p>Study Title</p>
          <div>
            <p>Study Template</p>
            <p>|</p>
            <p>Study Collaborators</p>
          </div>
        </div>
        <div>
          {loginState()}
          {
            // @ts-ignore
            match(loginState() as LoginUser)
              .with({ state: "notLoggedIn" }, () => (
                <>
                  <Button type="lightBlue" onClick={() => nav("/app/login")}>
                    Log In
                  </Button>
                  <Button type="Blue" onClick={() => nav("/app/signup")}>
                    Sign Up
                  </Button>
                </>
              ))
              .with({ state: "loggedIn" }, ({ user }) => (
                <>
                  <div
                    class={classes.profileButton}
                    onClick={() => setShowDropdown(!showDropdown())}
                  >
                    {user.name.slice(0, 2).toUpperCase()}
                  </div>
                  <div
                    class={`${classes.profileDropdown} ${
                      showDropdown() ? classes.showDropdown : ""
                    }`}
                  >
                    <ul>
                      <li>
                        <a href="/admin" class={classes.fullWidthLink}>
                          Admin Area
                        </a>
                      </li>
                      <li>
                        <a href="/logout" class={classes.fullWidthLink}>
                          Sign Out
                        </a>
                      </li>
                    </ul>
                  </div>
                </>
              ))
              .exhaustive()
          }
        </div>
      </header>

      <div class={classes.headerThingy}></div>
    </>
  );
}
