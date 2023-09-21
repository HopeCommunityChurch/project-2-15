import { createEffect, createSignal } from "solid-js";
import * as classes from "./styles.module.scss";

export function DynamicTextArea({ scripture, onScriptureChange }) {
  let scriptureTextRef;
  const [localScripture, setLocalScripture] = createSignal(scripture);

  function adjustHeight() {
    if (scriptureTextRef) {
      const element = scriptureTextRef;
      element.style.height = "auto";
      element.style.height = `${Math.min(element.scrollHeight, 200)}px`;
    }
  }

  createEffect(adjustHeight);

  return (
    <textarea
      ref={scriptureTextRef}
      value={localScripture()}
      onInput={(e) => {
        const newValue = e.currentTarget.value;
        setLocalScripture(newValue);
        adjustHeight();
      }}
      onBlur={() => {
        onScriptureChange(localScripture());
      }}
      class={classes.scriptureText}
    />
  );
}
