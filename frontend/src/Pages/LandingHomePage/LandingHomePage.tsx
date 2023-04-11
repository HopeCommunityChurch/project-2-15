import React from "react";
import { Button3D } from "../../Components/Button3D/Button3D";
import { PreLoginTopNav } from "../../Components/PreLoginTopNav/PreLoginTopNav";

import * as classes from "./styles.module.scss";

export function LandingHomePage() {
  return (
    <div className={classes.grid}>
      <PreLoginTopNav />
      <div className={classes.mainContent}>Body</div>
      <footer>Footer</footer>
    </div>
  );
}
