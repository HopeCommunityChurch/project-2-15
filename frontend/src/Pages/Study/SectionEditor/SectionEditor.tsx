import { createEffect, createSignal, Show } from "solid-js";
import { useNavigate } from "@solidjs/router";
import { A } from "@solidjs/router";
import CloseXIcon from "Assets/x.svg";

import * as classes from "./styles.module.scss";

export function SectionEditor({ showSectionEditor, setShowSectionEditor }) {
  return (
    <Show when={showSectionEditor()}>
      <div class={classes.modalBackground} onClick={() => setShowSectionEditor(false)}>
        <div class={classes.ModalBody} onClick={(e) => e.stopPropagation()}>
          <h3>Edit Sections</h3>
          <img
            src={CloseXIcon}
            alt="Close Modal"
            class={classes.closeModalIcon}
            onClick={() => setShowSectionEditor(false)}
          />
        </div>
      </div>
    </Show>
  );
}
