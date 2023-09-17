import { Instant, LocalDateTime } from "@js-joda/core";

export type UUID = string & { readonly __tag: unique symbol };

export type UserId = UUID & { readonly __tag: unique symbol };

export type PublicUser = {
  userId: UserId;
  name: string;
  image?: string;
};

export type DocId = UUID & { readonly __tag: unique symbol };
export type GroupStudyId = UUID & { readonly __tag: unique symbol };

export type DocMetaRaw = {
  created: string;
  updated: string;
  docId: DocId;
  name: string;
  studyId?: GroupStudyId;
  editors: Array<PublicUser>;
};

export type StudyTemplateId = UUID & { readonly __tag: unique symbol };

export type GroupStudyRaw = {
  name: string;
  studyId: GroupStudyId;
  studyTemplateId: StudyTemplateId;
  docs: Array<DocMetaRaw>;
  owners: Array<PublicUser>;
};

export type DocMeta = {
  created: LocalDateTime;
  updated: LocalDateTime;
  docId: DocId;
  name: string;
  studyId: GroupStudyId;
  editors: Array<PublicUser>;
};

export type GroupStudy = {
  name: string;
  studyId: GroupStudyId;
  studyTemplateId: StudyTemplateId;
  docs: Array<DocMeta>;
  owners: Array<PublicUser>;
};

// export function toDocFromRaw(doc: DocMetaRaw): DocMeta {
//   return {
//     editors: doc.editors,
//     docId: doc.docId,
//     studyId: doc.studyId,
//     name: doc.name,
//     created: LocalDateTime.ofInstant(Instant.parse(doc.created)),
//     updated: LocalDateTime.ofInstant(Instant.parse(doc.updated)),
//   };
// }

// export function toStudyFromRaw(study: GroupStudyRaw): GroupStudy {
//   return {
//     name: study.name,
//     studyId: study.studyId,
//     studyTemplateId: study.studyTemplateId,
//     docs: study.docs.map(toDocFromRaw),
//     owners: study.owners
//   };
// }

export type DocRaw = {
  docId : DocId;
  document: any;
  editors: Array<PublicUser>;
  name: string;
  groupStudyId?: GroupStudyId;
  studyTemplateId?: StudyTemplateId;
  created: string;
  updated: string;
};

export type Doc = {
  docId : DocId;
  document: any;
  editors: Array<PublicUser>;
  name: string;
  groupStudyId?: GroupStudyId;
  studyTemplateId?: StudyTemplateId;
  created: LocalDateTime;
  updated: LocalDateTime;
};

export function toDocFromRaw(doc: DocRaw): Doc {
  return {
    editors: doc.editors,
    docId: doc.docId,
    groupStudyId: doc.groupStudyId,
    document: doc.document,
    name: doc.name,
    created: LocalDateTime.ofInstant(Instant.parse(doc.created)),
    updated: LocalDateTime.ofInstant(Instant.parse(doc.updated)),
  };
}
