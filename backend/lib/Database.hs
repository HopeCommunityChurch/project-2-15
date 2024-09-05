module Database where

import Data.Aeson (Object)
import Database.Beam (
  Beamable,
  C,
  Database,
  DatabaseEntity,
  DatabaseSettings,
  EntityModification,
  FieldModification,
  Table (..),
  TableEntity,
  TableField,
  defaultDbSettings,
  fieldNamed,
  modifyEntityName,
  modifyTableFields,
  withDbModification,
 )
import Database.Beam.Postgres (PgJSONB(..))
import Password (PasswordHash)
import Types qualified as T

type TableMod table =
  forall be .
    EntityModification (DatabaseEntity be Db) be (TableEntity table)


unPgJSONB :: PgJSONB a -> a
unPgJSONB (PgJSONB a) = a


modifyTable
  :: Beamable tbl
  => Text
  -> tbl (FieldModification (TableField tbl))
  -> EntityModification (DatabaseEntity be db) be (TableEntity tbl)
modifyTable tblName fFeilds =
  modifyEntityName (const tblName) <> modifyTableFields fFeilds


data UserT f = MkUserT
  { userId :: C f T.UserId
  , email :: C f T.Email
  , name :: C f Text
  , image :: C f (Maybe Text)
  , churchId :: C f T.ChurchId
  , created :: C f UTCTime
  }
  deriving (Generic)
  deriving anyclass (Beamable)

instance Table UserT where
  data PrimaryKey UserT f = UserKey (C f T.UserId)
    deriving Generic
    deriving anyclass (Beamable)
  primaryKey = UserKey <$> (.userId)

userTable :: TableMod UserT
userTable =
  modifyTable
    "user"
    MkUserT
      { userId = fieldNamed "userId"
      , email = fieldNamed "email"
      , name = fieldNamed "name"
      , image = fieldNamed "image"
      , churchId = fieldNamed "churchId"
      , created = fieldNamed "created"
      }


data UserPasswordT f = MkUserPasswordT
  { userId :: C f T.UserId
  , password :: C f PasswordHash
  }
  deriving (Generic)
  deriving anyclass (Beamable)

instance Table UserPasswordT where
  data PrimaryKey UserPasswordT f = UserPasswordKey (C f T.UserId)
    deriving Generic
    deriving anyclass (Beamable)
  primaryKey = UserPasswordKey <$> (.userId)

userPasswordTable :: TableMod UserPasswordT
userPasswordTable =
  modifyTable
    "user_password"
    MkUserPasswordT
      { userId = fieldNamed "userId"
      , password = fieldNamed "password"
      }


data UserFeatureT f = MkUserFeatureT
  { userId :: C f T.UserId
  , feature :: C f T.Feature
  }
  deriving (Generic)
  deriving anyclass (Beamable)

instance Table UserFeatureT where
  data PrimaryKey UserFeatureT f =
      UserFeatureKey
        (C f T.UserId)
        (C f T.Feature)
    deriving Generic
    deriving anyclass (Beamable)
  primaryKey = UserFeatureKey <$> (.userId) <*> (.feature)

userFeatureTable :: TableMod UserFeatureT
userFeatureTable =
  modifyTable
    "user_feature"
    MkUserFeatureT
      { userId = fieldNamed "userId"
      , feature = fieldNamed "feature"
      }


data UserPasswordResetT f = MkUserPasswordResetT
  { userId :: C f T.UserId
  , token :: C f T.PasswordResetToken
  , usedAt :: C f (Maybe UTCTime)
  , expiresAt :: C f UTCTime
  , created :: C f UTCTime
  }
  deriving (Generic)
  deriving anyclass (Beamable)

instance Table UserPasswordResetT where
  data PrimaryKey UserPasswordResetT f =
    UserPasswordResetKey (C f T.PasswordResetToken)
    deriving Generic
    deriving anyclass (Beamable)
  primaryKey = UserPasswordResetKey <$> (.token)

userPasswordResetTable :: TableMod UserPasswordResetT
userPasswordResetTable =
  modifyTable
    "user_password_reset"
    MkUserPasswordResetT
      { userId = fieldNamed "userId"
      , token = fieldNamed "token"
      , usedAt = fieldNamed "usedAt"
      , expiresAt = fieldNamed "expiresAt"
      , created = fieldNamed "created"
      }



data UserSessionT f = MkUserSessionT
  { userId :: C f T.UserId
  , token :: C f T.CookieToken
  , expires :: C f UTCTime
  , created :: C f UTCTime
  }
  deriving (Generic)
  deriving anyclass (Beamable)

instance Table UserSessionT where
  data PrimaryKey UserSessionT f = UserSessionKey (C f T.CookieToken)
    deriving Generic
    deriving anyclass (Beamable)
  primaryKey = UserSessionKey <$> (.token)

userSessionTable :: TableMod UserSessionT
userSessionTable =
  modifyTable
    "user_session"
    MkUserSessionT
      { userId = fieldNamed "userId"
      , token = fieldNamed "token"
      , expires = fieldNamed "expires"
      , created = fieldNamed "created"
      }


data ChurchT f = MkChurchT
  { churchId :: C f T.ChurchId
  , name :: C f Text
  , created :: C f UTCTime
  }
  deriving (Generic)
  deriving anyclass (Beamable)

instance Table ChurchT where
  data PrimaryKey ChurchT f = ChurchKey (C f T.ChurchId)
    deriving Generic
    deriving anyclass (Beamable)
  primaryKey = ChurchKey <$> (.churchId)

churchTable :: TableMod ChurchT
churchTable =
  modifyTable
    "church"
    MkChurchT
      { churchId = fieldNamed "churchId"
      , name = fieldNamed "name"
      , created = fieldNamed "created"
      }


data ChurchElderT f = MkChurchElderT
  { churchId :: C f T.ChurchId
  , userId :: C f T.UserId
  , created :: C f UTCTime
  }
  deriving (Generic)
  deriving anyclass (Beamable)

instance Table ChurchElderT where
  data PrimaryKey ChurchElderT f = ChurchElderKey (C f T.ChurchId) (C f T.UserId)
    deriving Generic
    deriving anyclass (Beamable)
  primaryKey = ChurchElderKey <$> (.churchId) <*> (.userId)

churchElderTable :: TableMod ChurchElderT
churchElderTable =
  modifyTable
    "church_elder"
    MkChurchElderT
      { churchId = fieldNamed "churchId"
      , userId = fieldNamed "userId"
      , created = fieldNamed "created"
      }



data StudyTemplateT f = MkStudyTemplateT
  { studyTemplateId :: C f T.StudyTemplateId
  , name :: C f Text
  , document :: C f (PgJSONB Object)
  , churchId :: C f T.ChurchId
  , created :: C f UTCTime
  }
  deriving (Generic)
  deriving anyclass (Beamable)

instance Table StudyTemplateT where
  data PrimaryKey StudyTemplateT f = StudyTemplateKey (C f T.StudyTemplateId)
    deriving Generic
    deriving anyclass (Beamable)
  primaryKey = StudyTemplateKey <$> (.studyTemplateId)

studyTemplateTable :: TableMod StudyTemplateT
studyTemplateTable =
  modifyTable
    "study_template"
    MkStudyTemplateT
      { studyTemplateId = fieldNamed "studyTemplateId"
      , name = fieldNamed "name"
      , document = fieldNamed "document"
      , churchId = fieldNamed "churchId"
      , created = fieldNamed "created"
      }



data GroupStudyT f = MkGroupStudyT
  { groupStudyId :: C f T.GroupStudyId
  , studyTemplateId :: C f (Maybe T.StudyTemplateId)
  , name :: C f Text
  , created :: C f UTCTime
  }
  deriving (Generic)
  deriving anyclass (Beamable)

instance Table GroupStudyT where
  data PrimaryKey GroupStudyT f = GroupStudyKey (C f T.GroupStudyId)
    deriving Generic
    deriving anyclass (Beamable)
  primaryKey = GroupStudyKey <$> (.groupStudyId)

groupStudyTable :: TableMod GroupStudyT
groupStudyTable =
  modifyTable
    "group_study"
    MkGroupStudyT
      { groupStudyId = fieldNamed "groupStudyId"
      , studyTemplateId = fieldNamed "studyTemplateId"
      , name = fieldNamed "name"
      , created = fieldNamed "created"
      }


data GroupStudyShareT f = MkGroupStudyShareT
  { groupStudyId :: C f T.GroupStudyId
  , shareToken :: C f T.ShareToken
  , email :: C f T.Email
  , shareAsOwner :: C f Bool
  , expiresAt :: C f UTCTime
  , usedAt :: C f (Maybe UTCTime)
  , message :: C f (Maybe Text)
  , rejected :: C f Bool
  , created :: C f UTCTime
  }
  deriving (Generic)
  deriving anyclass (Beamable)

instance Table GroupStudyShareT where
  data PrimaryKey GroupStudyShareT f =
    GroupStudyShareKey
      (C f T.ShareToken)
    deriving Generic
    deriving anyclass (Beamable)
  primaryKey = GroupStudyShareKey <$>  (.shareToken)

groupStudyShareTable :: TableMod GroupStudyShareT
groupStudyShareTable =
  modifyTable
    "group_study_share"
    MkGroupStudyShareT
      { groupStudyId = fieldNamed "groupStudyId"
      , shareToken = fieldNamed "shareToken"
      , email = fieldNamed "email"
      , shareAsOwner = fieldNamed "shareAsOwner"
      , expiresAt = fieldNamed "expiresAt"
      , usedAt = fieldNamed "usedAt"
      , message = fieldNamed "message"
      , rejected = fieldNamed "rejected"
      , created = fieldNamed "created"
      }


data GroupStudyOwnerT f = MkGroupStudyOwnerT
  { groupStudyId :: C f T.GroupStudyId
  , userId :: C f T.UserId
  }
  deriving (Generic)
  deriving anyclass (Beamable)

instance Table GroupStudyOwnerT where
  data PrimaryKey GroupStudyOwnerT f =
    GroupStudyOwnerKey
      (C f T.GroupStudyId)
      (C f T.UserId)
    deriving Generic
    deriving anyclass (Beamable)
  primaryKey = GroupStudyOwnerKey <$> (.groupStudyId) <*> (.userId)

groupStudyOwnerTable :: TableMod GroupStudyOwnerT
groupStudyOwnerTable =
  modifyTable
    "group_study_owner"
    MkGroupStudyOwnerT
      { groupStudyId = fieldNamed "groupStudyId"
      , userId = fieldNamed "userId"
      }


data DocumentT f = MkDocumentT
  { docId :: C f T.DocId
  , groupStudyId :: C f (Maybe T.GroupStudyId)
  , studyTemplateId :: C f (Maybe T.StudyTemplateId)
  , name :: C f Text
  , document :: C f (PgJSONB Object)
  , isDeleted :: C f Bool
  , created :: C f UTCTime
  , updated :: C f UTCTime
  }
  deriving (Generic)
  deriving anyclass (Beamable)

instance Table DocumentT where
  data PrimaryKey DocumentT f = DocumentKey (C f T.DocId)
    deriving Generic
    deriving anyclass (Beamable)
  primaryKey = DocumentKey <$> (.docId)

documentTable :: TableMod DocumentT
documentTable =
  modifyTable
    "document"
    MkDocumentT
      { docId = fieldNamed "docId"
      , groupStudyId = fieldNamed "groupStudyId"
      , studyTemplateId = fieldNamed "studyTemplateId"
      , name = fieldNamed "name"
      , document = fieldNamed "document"
      , isDeleted = fieldNamed "isDeleted"
      , updated = fieldNamed "updated"
      , created = fieldNamed "created"
      }


data DocumentSaveT f = MkDocumentSaveT
  { docId :: C f T.DocId
  , userId :: C f T.UserId
  , computerId :: C f T.ComputerId
  , time :: C f UTCTime
  }
  deriving (Generic)
  deriving anyclass (Beamable)

instance Table DocumentSaveT where
  data PrimaryKey DocumentSaveT f = DocumentSaveKey (C f T.DocId)
    deriving Generic
    deriving anyclass (Beamable)
  primaryKey = DocumentSaveKey <$> (.docId)

documentSaveTable :: TableMod DocumentSaveT
documentSaveTable =
  modifyTable
    "document_save"
    MkDocumentSaveT
      { docId = fieldNamed "docId"
      , userId = fieldNamed "userId"
      , computerId = fieldNamed "computerId"
      , time = fieldNamed "time"
      }



data DocumentEditorT f = MkDocumentEditorT
  { docId :: C f T.DocId
  , userId :: C f T.UserId
  }
  deriving (Generic)
  deriving anyclass (Beamable)

instance Table DocumentEditorT where
  data PrimaryKey DocumentEditorT f = DocumentEditorKey (C f T.DocId) (C f T.UserId)
    deriving Generic
    deriving anyclass (Beamable)
  primaryKey = DocumentEditorKey <$> (.docId) <*> (.userId)

documentEditorTable :: TableMod DocumentEditorT
documentEditorTable =
  modifyTable
    "document_editor"
    MkDocumentEditorT
      { docId = fieldNamed "docId"
      , userId = fieldNamed "userId"
      }



data Db f = MkDb
  { user :: f (TableEntity UserT)
  , userPassword :: f (TableEntity UserPasswordT)
  , userFeature :: f (TableEntity UserFeatureT)
  , userPasswordReset :: f (TableEntity UserPasswordResetT)
  , userSession :: f (TableEntity UserSessionT)
  , church :: f (TableEntity ChurchT)
  , churchElder :: f (TableEntity ChurchElderT)
  , studyTemplate :: f (TableEntity StudyTemplateT)
  , groupStudy :: f (TableEntity GroupStudyT)
  , groupStudyOwner :: f (TableEntity GroupStudyOwnerT)
  , groupStudyShare :: f (TableEntity GroupStudyShareT)
  , document :: f (TableEntity DocumentT)
  , documentSave :: f (TableEntity DocumentSaveT)
  , documentEditor :: f (TableEntity DocumentEditorT)
  }
  deriving (Generic)

instance Database be Db

db :: DatabaseSettings be Db
db = defaultDbSettings `withDbModification`
        MkDb
          { user = userTable
          , userPassword = userPasswordTable
          , userFeature = userFeatureTable
          , userPasswordReset = userPasswordResetTable
          , userSession = userSessionTable
          , church = churchTable
          , churchElder = churchElderTable
          , studyTemplate = studyTemplateTable
          , groupStudy = groupStudyTable
          , groupStudyOwner = groupStudyOwnerTable
          , groupStudyShare = groupStudyShareTable
          , document = documentTable
          , documentSave = documentSaveTable
          , documentEditor = documentEditorTable
          }
