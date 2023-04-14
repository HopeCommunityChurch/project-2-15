import React from "react";
import * as classes from "./styles.module.scss";

interface ButtonProps {
  type: "lightBlue" | "Blue" | "default";
  children?: React.ReactNode;
}

export function Button(props: ButtonProps) {
  return (
    <button
      className={
        props.type === "lightBlue"
          ? classes.lightBlue
          : props.type === "Blue"
          ? classes.blue
          : classes.button
      }
    >
      {props.children}
    </button>
  );
}
