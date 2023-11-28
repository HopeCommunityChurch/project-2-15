import * as classes from "./styles.module.scss";
import { Schema } from "prosemirror-model";

export const textSchema = new Schema({
  nodes: {
    doc: {
      content: "section*",
    },
    paragraph: {
      content: "text*",
      group: "richText",
      toDOM: () => {
        return ["p", 0];
      },
    },
    link: {
      attrs: {
        href: {},
        title: { default: null },
      },
      inclusive: false,
      parseDOM: [
        {
          tag: "a[href]",
          getAttrs(dom: any) {
            return {
              href: dom.getAttribute("href"),
              title: dom.getAttribute("title"),
            };
          },
        },
      ],
      toDOM(node: any) {
        const { href, title } = node.attrs;
        return ["a", { href, title }, 0];
      },
    },
    section: {
      content: "sectionChild*",
      isolating: true,
      defining: true,
      toDOM(node) {
        return ["div", { class: classes.section }, 0];
      },
    },
    sectionHeader: {
      content: "text*",
      group: "sectionChild",
      isolating: true,
      defining: true,
      toDOM: () => {
        return ["h2", 0];
      },
    },
    bibleText: {
      content: "chunk*",
      group: "sectionChild",
      isolating: true,
      defining: true,
      attrs: { verses: { default: "Genesis 1:1" } },
      toDOM: () => {
        return [
          "div",
          {
            class: classes.bibleText,
          },
          0,
        ];
      },
    },
    studyBlocks: {
      content: "studyElement*",
      group: "sectionChild",
      isolating: true,
      defining: true,
    },
    questionDoc: {
      content: "question",
    },
    questions: {
      content: "question*",
      group: "studyElement",
      isolating: true,
      defining: true,
    },
    question: {
      content: "(questionAnswer | questionText )*",
      attrs: { questionId: { default: null } },
      isolating: true,
      defining: true,
      draggable: true,
      toDOM: () => {
        return ["question", { class: classes.question }, 0];
      },
    },
    generalStudyBlock: {
      content: "generalStudyBlockChildren*",
      group: "studyElement",
      isolating: true,
      defining: true,
      toDOM: () => {
        return ["tr", 0];
      },
    },
    generalStudyBlockHeader: {
      content: "text*",
      group: "generalStudyBlockChildren",
      isolating: true,
      defining: true,
    },
    generalStudyBlockBody: {
      content: "richText*",
      group: "generalStudyBlockChildren",
      isolating: true,
      defining: true,
      toDOM: () => {
        return ["td", 0];
      },
    },
    questionText: {
      isolating: true,
      defining: true,
      content: "richText*",
      toDOM: () => {
        return ["questionText", 0];
      },
    },
    questionAnswer: {
      content: "richText*",
      draggable: true,
      toDOM: () => {
        return ["questionAnswer", 0];
      },
    },
    chunkComment: {
      content: "text*",
      inline: true,
      atom: true,
      draggable: false,
      defining: true,
      selectable: false,
      attrs: { referenceId: { default: null }, color: { default: "red" } },
      toDOM: () => ["chunkComment", 0],
      parseDOM: [{ tag: "chunkComment" }],
    },
    chunk: {
      content: "(chunkComment | text)*",
      attrs: { level: { default: 0 } },
      toDOM: (node) => {
        return [
          "div",
          {
            class: classes.chunk,
            level: node.attrs.level,
            "data-indent-level": node.attrs.level,
          },
          0,
        ];
      },
      parseDOM: [
        {
          tag: `div.${classes.chunk}`,
          getAttrs(domNode) {
            const dom = domNode as HTMLElement;
            const level = dom.getAttribute("data-indent-level");
            return { level: level ? parseInt(level, 10) : 0 };
          },
        },
      ],
    },
    text: { inline: true },
  },
  marks: {
    strong: {
      parseDOM: [{ tag: "strong" }],
      toDOM: () => ["strong"],
    },
    em: {
      parseDOM: [{ tag: "em" }],
      toDOM: () => ["em"],
    },
    underline: {
      parseDOM: [{ tag: "u" }],
      toDOM: () => ["u"],
    },
    textColor: {
      attrs: { color: {} },
      inline: true,
      parseDOM: [
        {
          style: "color",
          getAttrs: (value) => {
            return { color: value };
          },
        },
      ],
      toDOM: (mark) => {
        return ["span", { style: `color: ${mark.attrs.color};` }, 0];
      },
    },
    highlightColor: {
      attrs: { color: {} },
      inline: true,
      parseDOM: [
        {
          style: "background-color",
          getAttrs: (value) => {
            return { color: value };
          },
        },
      ],
      toDOM: (mark) => {
        return ["span", { style: `background-color: ${mark.attrs.color};` }, 0];
      },
    },
    referenceTo: {
      attrs: { referenceId: {} },
      excludes: "",
      toDOM: (mark) => {
        return [
          "span",
          {
            "data-type": "reference",
            class: classes.reference,
            referenceId: mark.attrs.referenceId,
          },
          0,
        ];
      },
    },
    questionReference: {
      attrs: { questionId: {} },
      excludes: "",
      toDOM: (mark) => [
        "questionRef",
        {
          class: classes.questionRef,
          questionId: mark.attrs.questionId,
        },
        0,
      ],
    },
    verse: {
      attrs: { book: {}, chapter: {}, verse: {} },
      toDOM: (mark) => {
        return [
          "span",
          {
            "data-verse-book": mark.attrs.book,
            "data-verse-chapter": mark.attrs.chapter,
            "data-verse-verse": mark.attrs.verse,
          },
          0,
        ];
      },
      parseDOM: [
        {
          tag: "span[data-verse-book][data-verse-chapter][data-verse-verse]",
          getAttrs: (dom) => {
            if (dom instanceof HTMLElement) {
              return {
                book: dom.getAttribute("data-verse-book"),
                chapter: dom.getAttribute("data-verse-chapter"),
                verse: dom.getAttribute("data-verse-verse"),
              };
            }
            return {};
          },
        },
      ],
    },
  },
});
