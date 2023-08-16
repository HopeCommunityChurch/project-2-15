export type UUID = string & { readonly __tag: unique symbol }

export type UserId = UUID & { readonly __tag: unique symbol }

export type PublicUser = {
  userId: UserId;
  name: string;
  image?: string;
}

export type DocId = UUID & { readonly __tag: unique symbol }
export type StudyId = UUID & { readonly __tag: unique symbol }

export type Doc = {
  created: string;
  docId: DocId;
  name: string;
  studyId: StudyId;
  editors: Array<PublicUser>;

};

export type StudyTemplateId = UUID & { readonly __tag: unique symbol }

export type Study = {
  name: string;
  sutdyId: StudyId;
  studyTemplateId: StudyTemplateId;
  docs: Array<Doc>;
};
