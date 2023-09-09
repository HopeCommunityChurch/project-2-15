import { createEffect, createSignal, onMount, For, createResource, Show } from "solid-js";
import ArrowIcon from "../../Assets/arrow.svg";
import VerticalElipsesIcon from "../../Assets/vertical-elipses-icon.svg";
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
import * as Editor from "../../Editor/Editor";
import EnterIcon from "./enter-icon.svg";

type TextEditorToolbarProps = {
  editor: Editor.P215Editor;
  isTopbarOpen: () => boolean;
  setTopbarOpen: (value: boolean) => void;
};

export function TextEditorToolbar(props: TextEditorToolbarProps) {
  const [showExtendedToolbar, setShowExtendedToolbar] = createSignal(false);

  return (
    <>
      <div
        class={`${classes.topTextEditingToolbar} ${props.isTopbarOpen() ? "" : classes.collapsed}`}
      >
        <img src={UndoIcon} class={classes.toolbarIcon} />
        <img src={RedoIcon} class={classes.toolbarIcon} />
        <div class={classes.seperator} />
        <button class={classes.toobarTextDropdown}>
          <p>100%</p>
          <img src={ArrowIcon} />
        </button>
        <div class={classes.seperator} />
        <ToolbarGroup1 />
        <ToolbarGroup2 />
        <ToolbarGroup3 />
        <ToolbarGroup4 />
        <ToolbarGroup5 editor={props.editor} />
        <ToolbarGroup6 />
        <ClearFormattingSection />
        <div class={classes.extendedMenuContainer}>
          <img
            src={VerticalElipsesIcon}
            class={`${classes.toolbarIcon} ${classes.extendedMenuToggle} ${
              showExtendedToolbar() ? classes.expanded : ""
            }`}
            onClick={() => setShowExtendedToolbar(!showExtendedToolbar())}
          />
          {/* Conditionally show extended toolbar */}
          <Show when={showExtendedToolbar()}>
            <div class={classes.extendedToolbar}>
              <ToolbarGroup1 />
              <ToolbarGroup2 />
              <ToolbarGroup3 />
              <ToolbarGroup4 />
              <ToolbarGroup5 editor={props.editor} />
              <ToolbarGroup6 />
              <ClearFormattingSection />
            </div>
          </Show>
        </div>
        <img
          src={ArrowIcon}
          class={`${classes.toolbarIcon} ${classes.collapseTopBar}`}
          onClick={() => {
            props.setTopbarOpen(!props.isTopbarOpen());
          }}
        />
      </div>
    </>
  );
}

function ClearFormattingSection() {
  return (
    <img src={ClearFormattingIcon} class={`${classes.toolbarIcon} ${classes.clearFormatting}`} />
  );
}
function ToolbarGroup1() {
  return (
    <div class={classes.toolbarGroup1}>
      <button class={classes.toobarTextDropdown}>
        <p>Normal Text</p>
        <img src={ArrowIcon} />
      </button>
      <div class={classes.seperator} />
    </div>
  );
}
function ToolbarGroup2() {
  return (
    <div class={classes.toolbarGroup2}>
      <img src={BoldIcon} class={classes.toolbarIcon} />
      <img src={ItalicIcon} class={classes.toolbarIcon} />
      <img src={UnderlineIcon} class={classes.toolbarIcon} />
      <img src={TextColorIcon} class={classes.toolbarIcon} />
      <img src={HilightIcon} class={classes.toolbarIcon} />
      <div class={classes.seperator} />
    </div>
  );
}
function ToolbarGroup3() {
  return (
    <div class={classes.toolbarGroup3}>
      <img src={NumberedListIcon} class={classes.toolbarIcon} />
      <img src={BulletListIcon} class={classes.toolbarIcon} />
      <div class={classes.seperator} />
    </div>
  );
}
function ToolbarGroup4() {
  return (
    <div class={classes.toolbarGroup4}>
      <img src={OutdentIcon} class={classes.toolbarIcon} />
      <img src={IndentIcon} class={classes.toolbarIcon} />
      <div class={classes.seperator} />
    </div>
  );
}

type GroupProp = {
  editor: Editor.P215Editor;
};

function ToolbarGroup5(props: GroupProp) {
  let addQestionClick = (e: MouseEvent) => {
    e.preventDefault();
    props.editor.addQuestion();
  };
  return (
    <div class={classes.toolbarGroup5}>
      <img src={ReferenceIcon} class={classes.toolbarIcon} />
      <img src={RephraseIcon} class={classes.toolbarIcon} />
      <img src={QuestionIcon} class={classes.toolbarIcon} onClick={addQestionClick} />
      <div class={classes.seperator} />
    </div>
  );
}
function ToolbarGroup6() {
  return (
    <div class={classes.toolbarGroup6}>
      <img src={LinkIcon} class={classes.toolbarIcon} />
      <img src={CommentIcon} class={classes.toolbarIcon} />
      <img src={ParallelViewIcon} class={classes.toolbarIcon} />
      <div class={classes.seperator} />
    </div>
  );
}
