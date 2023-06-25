module Database where

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
import Types qualified as T

type TableMod table =
  forall be .
    EntityModification (DatabaseEntity be Db) be (TableEntity table)


modifyTable
  :: Beamable tbl
  => (Text -> Text)
  -> tbl (FieldModification (TableField tbl))
  -> EntityModification (DatabaseEntity be db) be (TableEntity tbl)
modifyTable tblName fFeilds = modifyEntityName tblName <> modifyTableFields fFeilds


data UserT f = MkUserT
  { userId :: C f T.UserId
  , email :: C f T.Email
  , name :: C f Text
  , image :: C f Text
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
    (const "users")
    MkUserT
      { userId = fieldNamed "userId"
      , email = fieldNamed "email"
      , name = fieldNamed "name"
      , image = fieldNamed "image"
      , churchId = fieldNamed "churchId"
      , created = fieldNamed "created"
      }


data GroupT f = MkGroupT
  { groupId :: C f T.GroupId
  , name :: C f Text
  , churchId :: C f T.ChurchId
  , created :: C f UTCTime
  }
  deriving (Generic)
  deriving anyclass (Beamable)

instance Table GroupT where
  data PrimaryKey GroupT f = GroupKey (C f T.GroupId)
    deriving Generic
    deriving anyclass (Beamable)
  primaryKey = GroupKey <$> (.groupId)

groupTable :: TableMod GroupT
groupTable =
  modifyTable
    (const "groups")
    MkGroupT
      { groupId = fieldNamed "groupId"
      , name = fieldNamed "name"
      , churchId = fieldNamed "churchId"
      , created = fieldNamed "created"
      }


data UserGroupT f = MkUserGroupT
  { groupId :: C f T.GroupId
  , userId :: C f T.UserId
  , isLeader :: C f Bool
  , created :: C f UTCTime
  }
  deriving (Generic)
  deriving anyclass (Beamable)

instance Table UserGroupT where
  data PrimaryKey UserGroupT f = UserGroupKey (C f T.GroupId) (C f T.UserId)
    deriving Generic
    deriving anyclass (Beamable)
  primaryKey = UserGroupKey <$> (.groupId) <*> (.userId)

userGroupTable :: TableMod UserGroupT
userGroupTable =
  modifyTable
    (const "users_groups")
    MkUserGroupT
      { groupId = fieldNamed "groupId"
      , userId = fieldNamed "userId"
      , isLeader = fieldNamed "isLeader"
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
    (const "church")
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
    (const "church_elder")
    MkChurchElderT
      { churchId = fieldNamed "churchId"
      , userId = fieldNamed "userId"
      , created = fieldNamed "created"
      }


data StudyT f = MkStudyT
  { studyId :: C f T.StudyId
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
    (const "study")
    MkStudyT
      { studyId = fieldNamed "studyId"
      , name = fieldNamed "name"
      , created = fieldNamed "created"
      }


data DocumentT f = MkDocumentT
  { docId :: C f T.DocId
  , studyId :: C f T.StudyId
  , name :: C f Text
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
    (const "study")
    MkDocumentT
      { docId = fieldNamed "docId"
      , studyId = fieldNamed "studyId"
      , name = fieldNamed "name"
      , created = fieldNamed "created"
      }



data Db f = MkDb
  { user :: f (TableEntity UserT)
  , group :: f (TableEntity GroupT)
  , userGroup :: f (TableEntity UserGroupT)
  , church :: f (TableEntity ChurchT)
  , study :: f (TableEntity StudyT)
  }
  deriving (Generic)

instance Database be Db

db :: DatabaseSettings be Db
db = defaultDbSettings `withDbModification`
        MkDb
          { user = userTable
          , group = groupTable
          , userGroup = userGroupTable
          , church = churchTable
          , study = studyTable
          }
