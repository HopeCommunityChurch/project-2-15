import React, { ReactNode } from "react";
import * as classes from "./styles.module.scss";
import Arrow from "./arrow-icon.svg";

interface Button3DProps {
  children: ReactNode;
}

export function Button3D({ children }) {
  return (
    <div>
      <button className={classes.buttonContainer}>
        {children}
        <img src={Arrow} />
      </button>
    </div>
  );
}
