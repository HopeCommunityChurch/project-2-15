module Entity.Study where

import Data.Aeson (Object)
import Database qualified as Db
import Database.Beam (
  Beamable,
  C,
  all_,
  default_,
  exists_,
  guard_,
  in_,
  insert,
  insertExpressions,
  runInsert,
  insertValues,
  val_,
  (==.),
 )
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning (runInsertReturningList))
import Database.Beam.Postgres (PgJSONB(..))
import DbHelper (MonadDb, jsonArraryOf, jsonBuildObject, runBeam)
import Entity qualified as E
import Entity.AuthUser
import Entity.User
import Types qualified as T


data GetDocMeta = MkGetDocMeta
  { docId :: T.DocId
  , studyId :: T.StudyId
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
    , studyId :: C f T.StudyId
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
      studyId
      name
      (fmap E.toEntity (toList (Db.unPgJSONB editors)))
      updated
      created

  -- Should only be used in GetStudy where access controls will exists
  queryEntity _ = do
    doc <- all_ Db.db.document

    let editors = jsonArraryOf $ do
                    de <- all_ Db.db.documentEditor
                    guard_ $ de.docId ==. doc.docId
                    user <- E.queryEntityBy @GetUser Nothing de.userId
                    pure $ jsonBuildObject user

    pure $
      MkDbGetDocMeta
        doc.docId
        doc.studyId
        doc.name
        editors
        doc.updated
        doc.created

type GetDocMeta' = DbEntity GetDocMeta Identity

instance FromJSON GetDocMeta'

instance E.GuardValue GetDocMeta T.StudyId where
  guardValues ids doc =
    guard_ $ doc.studyId `in_` ids


data GetStudy = MkGetStudy
  { studyId :: T.StudyId
  , studyTemplateId :: Maybe T.StudyTemplateId
  , name :: Text
  , docs :: List GetDocMeta
  }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON, ToSchema)


instance E.Entity GetStudy where
  data DbEntity GetStudy f = MkDbGetStudy
    { studyId :: C f T.StudyId
    , studyTemplateId :: C f (Maybe T.StudyTemplateId)
    , name :: C f Text
    , docs :: C f (PgJSONB (Vector GetDocMeta'))
    }
    deriving anyclass (Beamable)
    deriving (Generic)

  type EntityDatabase GetStudy = Db.Db

  toEntity MkDbGetStudy{..} =
    MkGetStudy
      studyId
      studyTemplateId
      name
      (fmap E.toEntity (toList (Db.unPgJSONB docs)))

  queryEntity mAuthUser = do
    study <- all_ Db.db.study

    -- Make sure the study user is in the study
    for_ mAuthUser $ \ authUser -> guard_ $ exists_ $ do
      doc2 <- all_ Db.db.document
      guard_ $ study.studyId ==. doc2.studyId
      user <- all_ Db.db.documentEditor
      guard_ $ user.docId ==. doc2.docId
      guard_ $ val_ authUser.userId ==. user.userId
      pure doc2.docId

    let docs = jsonArraryOf $ do
                    doc <- E.queryEntityBy @GetDocMeta Nothing study.studyId
                    pure $ jsonBuildObject doc

    pure $
      MkDbGetStudy
        study.studyId
        study.studyTemplateId
        study.name
        docs


type GetStudy' = DbEntity GetStudy Identity

instance FromJSON GetStudy'

instance E.GuardValue GetStudy T.StudyId where
  guardValues ids study =
    guard_ $ study.studyId `in_` ids

instance E.EntityWithId GetStudy where
  type EntityId GetStudy = T.StudyId
  entityId = (.studyId)


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
  -> m T.StudyId
addStudy userId crStudy = do
  now <- getCurrentTime
  [study] <-
    runBeam
      $ runInsertReturningList
      $ insert Db.db.study
      $ insertExpressions
        [ Db.MkStudyT
          default_
          (val_ crStudy.studyTemplateId)
          (val_ crStudy.name)
          (val_ now)
        ]
  let crDoc = CrDoc study.studyId study.name crStudy.document userId
  void $ crDocument crDoc
  pure study.studyId



data CrDoc = CrDoc
  { studyId :: T.StudyId
  , name :: Text
  , document :: Object
  , editor :: T.UserId
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)


crDocument
  :: MonadDb env m
  => CrDoc
  -> m T.DocId
crDocument crDoc = do
  now <- getCurrentTime
  [doc] <-
    runBeam
      $ runInsertReturningList
      $ insert Db.db.document
      $ insertExpressions
        [ Db.MkDocumentT
          default_
          (val_ crDoc.studyId)
          (val_ crDoc.name)
          (val_ (PgJSONB crDoc.document))
          (val_ now)
          (val_ now)
        ]
  runBeam
    $ runInsert
    $ insert Db.db.documentEditor
    $ insertValues
      [ Db.MkDocumentEditorT
        doc.docId
        crDoc.editor
      ]
  pure doc.docId

