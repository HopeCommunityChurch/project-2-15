import Logo from "./P215.png";
import { Button } from "../../Components/Button/Button";
import { createSignal, createEffect, Show } from "solid-js";
import { A, useNavigate } from "@solidjs/router";
import { loginState, LoginUser } from "../../Pages/LoginPage/login";

import * as classes from "./styles.module.scss";
import { match } from "ts-pattern";

export function PreLoginTopNav() {
  const [showNavbar, setShowNavbar] = createSignal(false);

  const handleShowNavbar = () => {
    setShowNavbar((n) => !n);
  };

  const nav = useNavigate();

  return (
    <>
      <header class={classes.header}>
        <img class={classes.logo} src={Logo} />
        <nav class={classes.menu}>
          <ul>
            <li>
              <a href="https://experiencethehope.com/teaching" target="_blank">
                Teachings
              </a>
            </li>
            <li>
              <a href="https://experiencethehope.com/equipping" target="_blank">
                Equipping
              </a>
            </li>
            <li>
              <a href="https://messaging.subsplash.com/25FXCW/auth" target="_blank">
                Messaging
              </a>
            </li>
          </ul>
        </nav>
        <div class={classes.buttons}>
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
                  <Button type="Blue" onClick={() => nav("/app/studies")}>
                    Go to App
                  </Button>
                </>
              ))
              .exhaustive()
          }
        </div>
        <div class={classes.hamburger} onClick={handleShowNavbar}>
          <Hamburger />
        </div>
        <div class={`${classes.mobileNav} ${showNavbar() ? classes.showNavbar : ""}`}>
          <ul>
            <li>
              <a href="https://experiencethehope.com/teaching" target="_blank">
                Teachings
              </a>
            </li>
            <li>
              <a href="https://experiencethehope.com/equipping" target="_blank">
                Equipping
              </a>
            </li>
            <li>
              <a href="https://messaging.subsplash.com/25FXCW/auth" target="_blank">
                Messaging
              </a>
            </li>
            {
              // @ts-ignore
              match(loginState() as LoginUser)
                .with({ state: "notLoggedIn" }, () => (
                  <>
                    <li>
                      <A href="/app/login">
                        <Button type="lightBlue">Log In</Button>
                      </A>
                    </li>
                    <li>
                      <A href="/app/signup">
                        <Button type="Blue">Sign Up</Button>
                      </A>
                    </li>
                  </>
                ))
                .with({ state: "loggedIn" }, ({ user }) => (
                  <>
                    <li>
                      <A href="/app/studies">
                        <Button type="Blue">Go to App</Button>
                      </A>
                    </li>
                  </>
                ))
                .exhaustive()
            }
          </ul>
        </div>
      </header>
      <div class={classes.headerThingy}></div>
    </>
  );
}

const Hamburger = () => (
  <svg xmlns="http://www.w3.org/2000/svg" width="52" height="24" viewBox="0 0 52 24">
    <g id="Group_9" data-name="Group 9" transform="translate(-294 -47)">
      <rect
        id="Rectangle_3"
        data-name="Rectangle 3"
        width="42"
        height="4"
        rx="2"
        transform="translate(304 47)"
        fill="#574c4c"
      />
      <rect
        id="Rectangle_5"
        data-name="Rectangle 5"
        width="42"
        height="4"
        rx="2"
        transform="translate(304 67)"
        fill="#574c4c"
      />
      <rect
        id="Rectangle_4"
        data-name="Rectangle 4"
        width="52"
        height="4"
        rx="2"
        transform="translate(294 57)"
        fill="#574c4c"
      />
    </g>
  </svg>
);
