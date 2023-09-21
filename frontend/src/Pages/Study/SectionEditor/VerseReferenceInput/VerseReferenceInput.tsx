import { createSignal } from "solid-js";
import * as classes from "./styles.module.scss";

export function VerseReferenceInput({
  initialValue,
  sectionIndex,
  verseIndex,
  handleVerseReferenceChange,
}) {
  // Local state for verse reference
  const [localVerseReference, setLocalVerseReference] = createSignal(initialValue);

  return (
    <input
      type="text"
      value={localVerseReference()}
      onInput={(e) => {
        const newValue = e.currentTarget.value;
        setLocalVerseReference(newValue);
      }}
      onBlur={() => {
        handleVerseReferenceChange(sectionIndex, verseIndex, localVerseReference());
      }}
      class={classes.verseRangeSelector}
    />
  );
}
