import { createEffect, createSignal, onMount, For, createResource, Show } from "solid-js";
import ArrowIcon from "Assets/arrow.svg";
import VerticalElipsesIcon from "Assets/vertical-elipses-icon.svg";
import * as classes from "./styles.module.scss";

import UndoIcon from "./Assets/undo-icon.svg";
import RedoIcon from "./Assets/redo-icon.svg";
import BoldIcon from "./Assets/bold-icon.svg";
import ItalicIcon from "./Assets/italic-icon.svg";
import UnderlineIcon from "./Assets/underline-icon.svg";
import TextColorIcon from "./Assets/text-color-icon.svg";
import HilightIcon from "./Assets/hilight-icon.svg";
import LinkIcon from "./Assets/link-icon.svg";
import ReferenceIcon from "./Assets/reference-icon.svg";
import RephraseIcon from "./Assets/rephrase-icon.svg";
import CommentIcon from "./Assets/comment-icon.svg";
import ParallelViewIcon from "./Assets/parallel-view-icon.svg";
import NumberedListIcon from "./Assets/numbered-list-icon.svg";
import BulletListIcon from "./Assets/bullet-list-icon.svg";
import IndentIcon from "./Assets/indent-icon.svg";
import OutdentIcon from "./Assets/outdent-icon.svg";
import ClearFormattingIcon from "./Assets/clear-formatting-icon.svg";
import QuestionIcon from "./Assets/question-icon.svg";
import * as Editor from "Editor/Editor";

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
        <ToolbarGroup4 editor={props.editor} />
        <ToolbarGroup5 editor={props.editor} />
        <ToolbarGroup6 editor={props.editor} />
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
              <ToolbarGroup4 editor={props.editor} />
              <ToolbarGroup5 editor={props.editor} />
              <ToolbarGroup6 editor={props.editor} />
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
function ToolbarGroup4(props: GroupProp) {
  let increaseLevel = (e : MouseEvent) => {
    e.preventDefault();
    props.editor.increaseLevel();
  }
  let decreaseLevel = (e : MouseEvent) => {
    e.preventDefault();
    props.editor.decreaseLevel();
  }
  return (
    <div class={classes.toolbarGroup4}>
      <img src={OutdentIcon} class={classes.toolbarIcon} onClick={decreaseLevel}/>
      <img src={IndentIcon} class={classes.toolbarIcon} onClick={increaseLevel} />
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

function ToolbarGroup6(props : GroupProp) {
  function addSection(e : MouseEvent) {
    e.preventDefault();
    const book = prompt("Book");
    const verses = prompt("Verses");
    const text = prompt("text");
    props.editor.addSection({
      header: book + " " + verses,
      bibleSections: [ {
        book: book,
        verses: verses,
        text: text,
      }],
    });
  }
  return (
    <div class={classes.toolbarGroup6}>
      <img src={LinkIcon} class={classes.toolbarIcon} />
      <img src={CommentIcon} class={classes.toolbarIcon} />
      <img src={ParallelViewIcon} class={classes.toolbarIcon} />
      <button onClick={addSection}>add section </button>
      <div class={classes.seperator} />
    </div>
  );
}
