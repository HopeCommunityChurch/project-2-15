module Entity.Document where

import Data.Aeson (Object)
import Database qualified as Db
import Database.Beam (
  Beamable,
  C,
  all_,
  exists_,
  guard_,
  in_,
  runUpdate,
  update,
  insertExpressions,
  runInsert,
  default_,
  insert,
  insertValues,
  val_,
  (<-.),
  (==.),
 )
import Database.Beam.Postgres (PgJSONB (..))
import DbHelper (MonadDb, jsonArraryOf, jsonBuildObject, runBeam)
import Entity qualified as E
import Entity.AuthUser
import Entity.User qualified as User
import Types qualified as T
import Database.Beam.Backend.SQL.BeamExtensions (runInsertReturningList)


data GetDoc = MkGetDoc
  { docId :: T.DocId
  , groupStudyId :: Maybe T.GroupStudyId
  , studyTemplateId :: Maybe T.StudyTemplateId
  , name :: Text
  , document :: Object
  , editors :: List User.GetUser
  , updated :: UTCTime
  , created :: UTCTime
  }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON, ToSchema)



instance E.Entity GetDoc where
  data DbEntity GetDoc f = MkDbGetDoc
    { docId :: C f T.DocId
    , groupStudyId :: C f (Maybe T.GroupStudyId)
    , studyTemplateId :: C f (Maybe T.StudyTemplateId)
    , name :: C f Text
    , document :: C f (PgJSONB Object)
    , editors :: C f (PgJSONB (Vector User.GetUser'))
    , update :: C f UTCTime
    , created :: C f UTCTime
    }
    deriving anyclass (Beamable)
    deriving (Generic)

  type EntityDatabase GetDoc = Db.Db

  toEntity MkDbGetDoc{..} =
    MkGetDoc
      docId
      groupStudyId
      studyTemplateId
      name
      (Db.unPgJSONB document)
      (fmap E.toEntity (toList (Db.unPgJSONB editors)))
      update
      created

  queryEntity mAuthUser = do
    doc <- all_ Db.db.document

    for_ mAuthUser $ \ authUser -> guard_ $ exists_ $ do
      doc2 <- all_ Db.db.document
      guard_ $ doc.groupStudyId ==. doc2.groupStudyId
      user <- all_ Db.db.documentEditor
      guard_ $ user.docId ==. doc2.docId
      guard_ $ val_ authUser.userId ==. user.userId
      pure doc2.docId

    let editors = jsonArraryOf $ do
                    de <- all_ Db.db.documentEditor
                    guard_ $ de.docId ==. doc.docId
                    user <- E.queryEntityBy @User.GetUser Nothing de.userId
                    pure $ jsonBuildObject user


    pure $
      MkDbGetDoc
        doc.docId
        doc.groupStudyId
        doc.studyTemplateId
        doc.name
        doc.document
        editors
        doc.updated
        doc.created

instance E.GuardValue GetDoc T.DocId where
  guardValues ids doc =
    guard_ $ doc.docId `in_` ids

instance E.EntityWithId GetDoc where
  type EntityId GetDoc = T.DocId
  entityId = (.docId)




updateDocument
  :: MonadDb env m
  => T.DocId
  -> Object
  -> m ()
updateDocument docId document = do
  now <- getCurrentTime
  runBeam
    $ runUpdate
    $ update
      Db.db.document
      (\ r ->
           (r.document <-. val_ (PgJSONB document))
        <> (r.updated <-. val_ now)
      )
      (\ r -> r.docId ==. val_ docId)


data CrDoc = CrDoc
  { studyTemplateId :: Maybe T.StudyTemplateId
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
          default_
          (val_ crDoc.studyTemplateId)
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

