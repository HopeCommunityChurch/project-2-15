// Libraries and external modules
import {
  createEffect,
  createSignal,
  onMount,
  createResource,
  Show,
  onCleanup,
  from,
} from "solid-js";
import { useParams, useNavigate } from "@solidjs/router";
import { throttle } from "@solid-primitives/scheduled";
import { match } from "ts-pattern";
import { dndzone } from "solid-dnd-directive";

// Local imports
import * as Network from "Utils/Network";
import * as classes from "./styles.module.scss";
import * as Editor from "Editor/Editor";
import { LoginUser, loginState } from "Pages/LoginPage/login";

// Icons
import DragHandleIcon from "Assets/drag-handle.svg";
import GrayCircleIcon from "Assets/gray-circle-icon.svg";
import Arrow2Icon from "Assets/arrow2.svg";
import GrayTrashIcon from "Assets/gray-trash-icon.svg";
import CloseXIcon from "Assets/x.svg";
import ArrowIcon from "Assets/arrow.svg";

type StudyBlockEditorProps = {
  selectedStudyBlockArea: any;
  setSelectedStudyBlockArea: any;
};

export function StudyBlockEditor(props: StudyBlockEditorProps) {
  // State and Variables
  const [editableStudyBlocks, setEditableStudyBlocks] = createSignal<EditableStudyBlocks[] | null>(
    null
  );

  interface EditableStudyBlocks {
    title: string;
    bodyText: string;
    id: string;
    required: boolean;
  }

  function extractContentFromNodes(nodes: any[]): EditableStudyBlocks[] {
    const extractedContent: EditableStudyBlocks[] = [];

    nodes.forEach((node) => {
      if (node.type.name === "generalStudyBlock") {
        // Extract the title from the studyBlockData
        const headerContent = JSON.parse(JSON.stringify(node)).content.find(
          (node) => node.type === "generalStudyBlockHeader"
        );
        const extractedTitle = headerContent?.content[0]?.text || "";

        let extractedDescription = JSON.parse(JSON.stringify(node))
          .content.find((item) => item.type === "generalStudyBlockBody")
          .content.map((item) => {
            if (item.content) {
              return item.content
                .filter((contentItem) => contentItem.type === "text" && contentItem.text)
                .map((contentItem) => contentItem.text)
                .join(" ");
            }
            return "";
          })
          .join(" ")
          .trim();

        extractedContent.push({
          title: extractedTitle,
          bodyText: extractedDescription,
          id: node.attrs.id,
          required: false,
        });
      }

      if (node.type.name === "questions") {
        extractedContent.push({
          title: "Questions",
          bodyText: "",
          id: "Questions",
          required: true,
        });
      }
    });
    setEditableStudyBlocks((prevBlocks) => [...(prevBlocks || []), ...extractedContent]);
    return extractedContent;
  }

  const moveItemUp = (index) => {
    // if (index === 0) return; // First item cannot move up
    // const newItems = [...selectedStudyItems()];
    // const temp = newItems[index];
    // newItems[index] = newItems[index - 1];
    // newItems[index - 1] = temp;
    // setSelectedStudyItems(newItems);
  };

  const moveItemDown = (index) => {
    // if (index === selectedStudyItems().length - 1) return; // Last item cannot move down
    // const newItems = [...selectedStudyItems()];
    // const temp = newItems[index];
    // newItems[index] = newItems[index + 1];
    // newItems[index + 1] = temp;
    // setSelectedStudyItems(newItems);
  };

  async function saveUpdatedStudyBlock() {
    const studyBlocks = editableStudyBlocks();

    for (const item of studyBlocks) {
      // Skip the element with id "Questions"
      if (item.id === "Questions") {
        console.log("Skipping 'Questions' item...");
        continue;
      }

      // Find the matching tr element in the DOM
      const trElement = document.querySelector(`[data-id="${item.id}"]`);
      if (!trElement) {
        console.log(`No matching tr element found for id ${item.id}`);
        continue;
      }

      // Find the studyBlockHeaderDiv inside the tr element
      const headerDiv = trElement.querySelector("div");
      if (!headerDiv) {
        console.log(`No first div found inside tr element`);
        continue;
      }

      // Update the text inside the div
      console.log(`Updating text for div inside tr element with id ${item.id}`);
      headerDiv.textContent = item.title;

      // Pause for 1 second before the next iteration
      await new Promise((resolve) => setTimeout(resolve, 10));
    }
    props.setSelectedStudyBlockArea(null);
    setEditableStudyBlocks(null);
  }

  return (
    <Show when={props.selectedStudyBlockArea() !== null}>
      <div
        class={classes.modalBackground}
        onClick={() => {
          props.setSelectedStudyBlockArea(null);
          setEditableStudyBlocks(null);
        }}
      >
        <div class={classes.editStudyBlockModal} onClick={(e) => e.stopPropagation()}>
          <h3>Edit Study Block</h3>
          <button type="submit" class={classes.saveButton} onClick={saveUpdatedStudyBlock}>
            Save
          </button>

          <img
            src={CloseXIcon}
            alt="Close Modal"
            class={classes.closeModalIcon}
            onClick={() => {
              props.setSelectedStudyBlockArea(null);
              setEditableStudyBlocks(null);
            }}
          />
          <table class={classes.editableStudyBlocksTable}>
            <tbody>
              {props.selectedStudyBlockArea()?.studyBlocks?.map((block, index) => {
                const blockContent = extractContentFromNodes([block])[0];

                return (
                  <tr class={classes.editableStudyBlock}>
                    <td class={classes.studyBlockGrayBackground}>
                      <div class={classes.upAndDownArrows}>
                        <img
                          class={classes.arrowUp}
                          src={ArrowIcon}
                          onClick={() => moveItemUp(index)}
                        />
                        <img
                          src={ArrowIcon}
                          class={classes.arrowDown}
                          onClick={() => moveItemDown(index)}
                        />
                      </div>
                      <div class={classes.studyBlockTitleAndDescription}>
                        <input
                          class={classes.studyBlockTitle}
                          type="text"
                          placeholder="Add title..."
                          value={blockContent.title || ""}
                          onChange={(e) => {
                            setEditableStudyBlocks((prevBlocks) => {
                              if (!prevBlocks) return null;

                              return prevBlocks.map((block) =>
                                block.id === blockContent.id
                                  ? { ...block, title: e.target.value }
                                  : block
                              );
                            });
                          }}
                        />
                        <input
                          class={classes.studyBlockDescription}
                          type="text"
                          placeholder="Add description..."
                        />
                      </div>
                    </td>
                    <td class={classes.studyBlockContentPreview}>
                      <p>{blockContent.bodyText || ""}</p>
                    </td>
                    <td class={classes.studyBlockDeleteBlock}>
                      <img
                        src={GrayTrashIcon}
                        onClick={async (e) => {
                          console.log("delete");
                        }}
                      />
                    </td>
                  </tr>
                );
              })}
            </tbody>
          </table>
        </div>
      </div>
    </Show>
  );
}
