import * as classes from "./styles.module.scss";
import Arrow from "./arrow-icon.svg";


export function Button3D({ children }) {
  return (
    <div>
      <button class={classes.buttonContainer}>
        {children}
        <img src={Arrow} />
      </button>
    </div>
  );
}
