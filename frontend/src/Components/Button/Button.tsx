import * as classes from "./styles.module.scss";

interface ButtonProps {
  type: "lightBlue" | "Blue" | "default";
  children?: any;
  onClick?: () => void
}

export function Button(props: ButtonProps) {
  return (
    <button
      class={
        props.type === "lightBlue"
          ? classes.lightBlue
          : props.type === "Blue"
          ? classes.blue
          : classes.button
      }
      onClick={props.onClick}
    >
      {props.children}
    </button>
  );
}
