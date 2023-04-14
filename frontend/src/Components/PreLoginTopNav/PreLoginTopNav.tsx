import React from "react";
import Logo from "../../Components/PreLoginTopNav/BibleOneLogoSVG";
import { Button } from "../../Components/Button/Button";

import * as classes from "./styles.module.scss";

export function PreLoginTopNav() {
  return (
    <header className={classes.header}>
      <Logo className={classes.logo} />
      <nav className={classes.menu}>
        <ul>
          <li>
            <a href="#">Teachings</a>
          </li>
          <li>
            <a href="#">Equipping</a>
          </li>
          <li>
            <a href="#">Give</a>
          </li>
        </ul>
      </nav>
      <div className={classes.buttons}>
        <Button type="lightBlue">Log In</Button>
        <Button type="Blue">Sign Up</Button>
      </div>
    </header>
  );
}
