
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


export type LastUpdate = {
  computerId: string,
  time: string,
  version: number,
};

export type DocRaw = {
  docId : DocId;
  document: any;
  editors: Array<PublicUser>;
  name: string;
  groupStudyId?: GroupStudyId;
  groupStudyName?: string;
  studyTemplateId?: StudyTemplateId;
  created: string;
  updated: string;
  version: number;
  lastUpdate?: LastUpdate
};


export type Verse = {
  book: string;
  chapter: number;
  verse: number;
  passage: string;
};



export type ESVResponse = {
  canonical: string;
  passage: Array<Verse>;
};
