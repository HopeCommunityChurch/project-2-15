module Entity.GroupStudy where

import Data.Aeson (Object)
import Database qualified as Db
import Database.Beam (
  Beamable,
  C,
  Nullable,
  all_,
  orderBy_,
  desc_,
  default_,
  exists_,
  guard_,
  in_,
  insert,
  insertExpressions,
  insertValues,
  limit_,
  runInsert,
  val_,
  (==.),
 )
import Database.Beam.Backend.SQL.BeamExtensions (runInsertReturningList)
import Database.Beam.Postgres (PgJSONB (..))
import Database.Beam.Query qualified as BQ
import DbHelper (MonadDb, asJust_, jsonArraryOf, jsonBuildObject, runBeam)
import Entity qualified as E
import Entity.AuthUser
import Entity.User
import LateralLeftJoin
import Types qualified as T
import Unsafe.Coerce (unsafeCoerce)


data GetDocMeta = MkGetDocMeta
  { docId :: T.DocId
  , groupStudyId :: T.GroupStudyId
  , name :: Text
  , editors :: List GetUser
  , created :: UTCTime
  , updated :: UTCTime
  }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON, ToSchema)


instance E.Entity GetDocMeta where
  data DbEntity GetDocMeta f = MkDbGetDocMeta
    { docId :: C f T.DocId
    , groupStudyId :: C f T.GroupStudyId
    , name :: C f Text
    , editors :: C f (PgJSONB (Vector GetUser'))
    , updated :: C f UTCTime
    , created :: C f UTCTime
    }
    deriving anyclass (Beamable)
    deriving (Generic)

  type EntityDatabase GetDocMeta = Db.Db

  toEntity MkDbGetDocMeta{..} =
    MkGetDocMeta
      docId
      groupStudyId
      name
      (fmap E.toEntity (toList (Db.unPgJSONB editors)))
      updated
      created

  -- Should only be used in GetGroupStudy where access controls will exists
  queryEntity _ = do
    doc <- all_ Db.db.document

    let editors = jsonArraryOf $ do
                    de <- all_ Db.db.documentEditor
                    guard_ $ de.docId ==. doc.docId
                    user <- E.queryEntityBy @GetUser Nothing de.userId
                    pure $ jsonBuildObject user

    groupStudyId <- asJust_ doc.groupStudyId

    pure $
      MkDbGetDocMeta
        doc.docId
        groupStudyId
        doc.name
        editors
        doc.updated
        doc.created

type GetDocMeta' = DbEntity GetDocMeta Identity

instance FromJSON GetDocMeta'

instance E.GuardValue GetDocMeta T.GroupStudyId where
  guardValues ids doc =
    guard_ $ doc.groupStudyId `in_` ids


data GetGroupStudy = MkGetGroupStudy
  { groupStudyId :: T.GroupStudyId
  , studyTemplateId :: Maybe T.StudyTemplateId
  , name :: Text
  , docs :: List GetDocMeta
  , lastUpdate :: Maybe T.UserId
  , owners :: List GetUser
  }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON, ToSchema)


instance E.Entity GetGroupStudy where
  data DbEntity GetGroupStudy f = MkDbGetGroupStudy
    { groupStudyId :: C f T.GroupStudyId
    , studyTemplateId :: C f (Maybe T.StudyTemplateId)
    , name :: C f Text
    , docs :: C f (PgJSONB (Vector GetDocMeta'))
    , lastUpdated :: C f (Maybe T.UserId)
    , owners :: C f (PgJSONB (Vector GetUser'))
    }
    deriving anyclass (Beamable)
    deriving (Generic)

  type EntityDatabase GetGroupStudy = Db.Db

  toEntity MkDbGetGroupStudy{..} =
    MkGetGroupStudy
      groupStudyId
      studyTemplateId
      name
      (fmap E.toEntity (toList (Db.unPgJSONB docs)))
      lastUpdated
      (fmap E.toEntity (toList (Db.unPgJSONB owners)))

  queryEntity mAuthUser = do
    study <- all_ Db.db.groupStudy

    -- Make sure the study user is in the study
    for_ mAuthUser $ \ authUser -> guard_ $ exists_ $ do
      doc2 <- all_ Db.db.document
      groupStudyId <- asJust_ doc2.groupStudyId
      guard_ $ study.groupStudyId ==. groupStudyId
      user <- all_ Db.db.documentEditor
      guard_ $ user.docId ==. doc2.docId
      guard_ $ val_ authUser.userId ==. user.userId
      pure doc2.docId

    let owners = jsonArraryOf $ do
                    gso <- all_ Db.db.groupStudyOwner
                    guard_ $ gso.groupStudyId ==. study.groupStudyId
                    user <- E.queryEntityBy @GetUser Nothing gso.userId
                    pure $ jsonBuildObject user

    let docs = jsonArraryOf $ do
                    doc <- E.queryEntityBy @GetDocMeta Nothing study.groupStudyId
                    pure $ jsonBuildObject doc


    lastUpdated <- lateralLeft_ study
                    $ \ _ ->
                      limit_ 1
                      $ orderBy_ (desc_ . (.updated) . fst)
                      $ do
                      doc <- all_ Db.db.document
                      groupStudyId <- asJust_ doc.groupStudyId
                      guard_ $ unsafeBreakScope study.groupStudyId ==. groupStudyId

                      owners' <- all_ Db.db.groupStudyOwner
                      guard_ $ owners'.groupStudyId ==. unsafeBreakScope study.groupStudyId

                      docEditor <- all_ Db.db.documentEditor
                      guard_ $ doc.docId ==. docEditor.docId
                      guard_ $ owners'.userId ==. docEditor.userId
                      pure (doc, owners')

    pure $
      MkDbGetGroupStudy
        study.groupStudyId
        study.studyTemplateId
        study.name
        docs
        (snd lastUpdated).userId
        owners

unsafeBreakScope :: BQ.QGenExpr ctx be s a -> BQ.QGenExpr ctx be s2 a
unsafeBreakScope = unsafeCoerce

type GetGroupStudy' = DbEntity GetGroupStudy Identity

instance FromJSON GetGroupStudy'

instance E.GuardValue GetGroupStudy T.GroupStudyId where
  guardValues ids study =
    guard_ $ study.groupStudyId `in_` ids

instance E.EntityWithId GetGroupStudy where
  type EntityId GetGroupStudy = T.GroupStudyId
  entityId = (.groupStudyId)


data CrStudy = CrStudy
  { name :: Text
  , studyTemplateId :: Maybe T.StudyTemplateId
  , document :: Object
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)


addStudy
  :: MonadDb env m
  => T.UserId
  -> CrStudy
  -> m T.GroupStudyId
addStudy userId crStudy = do
  now <- getCurrentTime
  [study] <-
    runBeam
      $ runInsertReturningList
      $ insert Db.db.groupStudy
      $ insertExpressions
        [ Db.MkGroupStudyT
          default_
          (val_ crStudy.studyTemplateId)
          (val_ crStudy.name)
          (val_ now)
        ]

  runBeam
    $ runInsert
    $ insert Db.db.groupStudyOwner
    $ insertValues
      [ Db.MkGroupStudyOwnerT
        study.groupStudyId
        userId
      ]

  pure study.groupStudyId
