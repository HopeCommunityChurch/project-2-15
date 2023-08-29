import { createEffect, createSignal, onMount, For, createResource, Show } from "solid-js";
import ArrowIcon from "../../Assets/arrow.svg";
import * as classes from "./styles.module.scss";

import UndoIcon from "./undo-icon.svg";
import RedoIcon from "./redo-icon.svg";
import BoldIcon from "./bold-icon.svg";
import ItalicIcon from "./italic-icon.svg";
import UnderlineIcon from "./underline-icon.svg";
import TextColorIcon from "./text-color-icon.svg";
import HilightIcon from "./hilight-icon.svg";
import LinkIcon from "./link-icon.svg";
import ReferenceIcon from "./reference-icon.svg";
import RephraseIcon from "./rephrase-icon.svg";
import CommentIcon from "./comment-icon.svg";
import ParallelViewIcon from "./parallel-view-icon.svg";
import NumberedListIcon from "./numbered-list-icon.svg";
import BulletListIcon from "./bullet-list-icon.svg";
import IndentIcon from "./indent-icon.svg";
import OutdentIcon from "./outdent-icon.svg";
import ClearFormattingIcon from "./clear-formatting-icon.svg";
import QuestionIcon from "./question-icon.svg";

export function TextEditorToolbar() {
  return (
    <div class={classes.topTextEditingToolbar}>
      <button class={classes.toobarTextDropdown}>
        <p>100%</p>
        <img src={ArrowIcon} />
      </button>
      <button class={classes.toobarTextDropdown}>
        <p>Normal Text</p>
        <img src={ArrowIcon} />
      </button>
      <div class={classes.seperator} />
      <img src={UndoIcon} class={classes.toolbarIcon} />
      <img src={RedoIcon} class={classes.toolbarIcon} />
      <div class={classes.seperator} />
      <img src={BoldIcon} class={classes.toolbarIcon} />
      <img src={ItalicIcon} class={classes.toolbarIcon} />
      <img src={UnderlineIcon} class={classes.toolbarIcon} />
      <img src={TextColorIcon} class={classes.toolbarIcon} />
      <img src={HilightIcon} class={classes.toolbarIcon} />
      <div class={classes.seperator} />
      <img src={LinkIcon} class={classes.toolbarIcon} />
      <img src={CommentIcon} class={classes.toolbarIcon} />
      <div class={classes.seperator} />
      <img src={NumberedListIcon} class={classes.toolbarIcon} />
      <img src={BulletListIcon} class={classes.toolbarIcon} />
      <img src={IndentIcon} class={classes.toolbarIcon} />
      <img src={OutdentIcon} class={classes.toolbarIcon} />
      <img src={ClearFormattingIcon} class={classes.toolbarIcon} />
      <div class={classes.seperator} />
      <img src={ReferenceIcon} class={classes.toolbarIcon} />
      <img src={RephraseIcon} class={classes.toolbarIcon} />
      <img src={QuestionIcon} class={classes.toolbarIcon} />
      <img src={ParallelViewIcon} class={classes.toolbarIcon} />
      <img src={ArrowIcon} class={`${classes.toolbarIcon} ${classes.collapseTopBar}`} />
    </div>
  );
}
