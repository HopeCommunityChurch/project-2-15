module Database where

import Data.Aeson (Value)
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
  , document :: C f (PgJSONB Value)
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



data StudyT f = MkStudyT
  { studyId :: C f T.StudyId
  , studyTemplateId :: C f (Maybe T.StudyTemplateId)
  , name :: C f Text
  , created :: C f UTCTime
  }
  deriving (Generic)
  deriving anyclass (Beamable)

instance Table StudyT where
  data PrimaryKey StudyT f = StudyKey (C f T.StudyId)
    deriving Generic
    deriving anyclass (Beamable)
  primaryKey = StudyKey <$> (.studyId)

studyTable :: TableMod StudyT
studyTable =
  modifyTable
    "study"
    MkStudyT
      { studyId = fieldNamed "studyId"
      , studyTemplateId = fieldNamed "studyTemplateId"
      , name = fieldNamed "name"
      , created = fieldNamed "created"
      }


data DocumentT f = MkDocumentT
  { docId :: C f T.DocId
  , studyId :: C f T.StudyId
  , name :: C f Text
  , document :: C f (PgJSONB Value)
  , created :: C f UTCTime
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
      , studyId = fieldNamed "studyId"
      , name = fieldNamed "name"
      , document = fieldNamed "document"
      , created = fieldNamed "created"
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
  , userSession :: f (TableEntity UserSessionT)
  , church :: f (TableEntity ChurchT)
  , churchElder :: f (TableEntity ChurchElderT)
  , studyTemplate :: f (TableEntity StudyTemplateT)
  , study :: f (TableEntity StudyT)
  , document :: f (TableEntity DocumentT)
  , documentEditor :: f (TableEntity DocumentEditorT)
  }
  deriving (Generic)

instance Database be Db

db :: DatabaseSettings be Db
db = defaultDbSettings `withDbModification`
        MkDb
          { user = userTable
          , userPassword = userPasswordTable
          , userSession = userSessionTable
          , church = churchTable
          , churchElder = churchElderTable
          , studyTemplate = studyTemplateTable
          , study = studyTable
          , document = documentTable
          , documentEditor = documentEditorTable
          }
