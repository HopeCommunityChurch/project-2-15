import React, { useState } from "react";
import Logo from "../../Components/PreLoginTopNav/BibleOneLogoSVG";
import { Button } from "../../Components/Button/Button";

import * as classes from "./styles.module.scss";

export function PreLoginTopNav() {
  const [showNavbar, setShowNavbar] = React.useState(false);

  const handleShowNavbar = () => {
    setShowNavbar(!showNavbar);
  };
  return (
    <header className={classes.header}>
      <Logo className={classes.logo} />
      <nav className={classes.menu}>
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
      <div className={classes.buttons}>
        <Button type="lightBlue">Log In</Button>
        <Button type="Blue">Sign Up</Button>
      </div>
      <div className={classes.hamburger} onClick={handleShowNavbar}>
        <Hamburger />
      </div>
      <div className={`${classes.mobileNav} ${showNavbar ? classes.showNavbar : ""}`}>
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
          <li>
            <a href="#">
              <Button type="lightBlue">Log In</Button>
            </a>
          </li>
          <li>
            <a href="#">
              <Button type="Blue">Sign Up</Button>
            </a>
          </li>
        </ul>
      </div>
    </header>
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
