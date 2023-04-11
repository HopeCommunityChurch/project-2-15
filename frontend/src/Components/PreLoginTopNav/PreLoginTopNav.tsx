import React from "react";
import Logo from "../../Components/PreLoginTopNav/BibleOneLogoSVG";

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
        <button>Login</button>
        <button>Sign Up</button>
      </div>
    </header>
  );
}
