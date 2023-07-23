import ReactDOM from "react-dom/client";
import React from "react";
import * as classes from "./styles.module.scss";

import { Button3D } from "./Components/Button3D/Button3D";
import { LandingHomePage } from "./Pages/LandingHomePage/LandingHomePage";

export function App() {
  return (
    <div>
      <LandingHomePage className={classes.global}></LandingHomePage>
    </div>
  );
}

const root = ReactDOM.createRoot(document.getElementById("root"));
root.render(<App></App>);
