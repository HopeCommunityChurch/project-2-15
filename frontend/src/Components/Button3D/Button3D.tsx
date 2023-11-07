import * as classes from "./styles.module.scss";
import Arrow from "./arrow-icon.svg";


export function Button3D({ children, onclick }) {
  return (
    <div>
      <button class={classes.buttonContainer} onclick={onclick}>
        {children}
        <img src={Arrow} />
      </button>
    </div>
  );
}
