import { Instant, LocalDateTime } from "@js-joda/core";

export type UUID = string & { readonly __tag: unique symbol };

export type UserId = UUID & { readonly __tag: unique symbol };

export type PublicUser = {
  userId: UserId;
  name: string;
  image?: string;
};

export type DocId = UUID & { readonly __tag: unique symbol };
export type StudyId = UUID & { readonly __tag: unique symbol };

export type DocMetaRaw = {
  created: string;
  updated: string;
  docId: DocId;
  name: string;
  studyId: StudyId;
  editors: Array<PublicUser>;
};

export type StudyTemplateId = UUID & { readonly __tag: unique symbol };

export type StudyRaw = {
  name: string;
  studyId: StudyId;
  studyTemplateId: StudyTemplateId;
  docs: Array<DocMetaRaw>;
};

export type DocMeta = {
  created: LocalDateTime;
  updated: LocalDateTime;
  docId: DocId;
  name: string;
  studyId: StudyId;
  editors: Array<PublicUser>;
};

export type Study = {
  name: string;
  studyId: StudyId;
  studyTemplateId: StudyTemplateId;
  docs: Array<DocMeta>;
};

export function toDocFromRaw(doc: DocMetaRaw): DocMeta {
  return {
    editors: doc.editors,
    docId: doc.docId,
    studyId: doc.studyId,
    name: doc.name,
    created: LocalDateTime.ofInstant(Instant.parse(doc.created)),
    updated: LocalDateTime.ofInstant(Instant.parse(doc.updated)),
  };
}

export function toStudyFromRaw(study: StudyRaw): Study {
  return {
    name: study.name,
    studyId: study.studyId,
    studyTemplateId: study.studyTemplateId,
    docs: study.docs.map(toDocFromRaw),
  };
}

export type Doc = {
  docId : DocId;
  document: any;
  editor: Array<PublicUser>;
  name: string;
  study: StudyId;
};
