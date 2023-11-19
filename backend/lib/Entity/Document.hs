module Entity.Document where

import Data.Aeson (Object)
import Database qualified as Db
import Database.Beam (
  Beamable,
  C,
  all_,
  default_,
  desc_,
  exists_,
  guard_,
  in_,
  insert,
  insertExpressions,
  insertValues,
  just_,
  leftJoin_',
  not_,
  orderBy_,
  runInsert,
  runSelectReturningList,
  runSelectReturningOne,
  runUpdate,
  select,
  update,
  val_,
  guard_',
  (<-.),
  (==.),
  (==?.),
 )
import Database.Beam.Backend.SQL.BeamExtensions (runInsertReturningList)
import Database.Beam.Postgres (PgJSONB (..))
import DbHelper (MonadDb, jsonArraryOf, jsonBuildObject, runBeam)
import Entity qualified as E
import Entity.AuthUser
import Entity.User qualified as User
import Types qualified as T


data GetDoc = MkGetDoc
  { docId :: T.DocId
  , groupStudyId :: Maybe T.GroupStudyId
  , studyTemplateId :: Maybe T.StudyTemplateId
  , groupStudyName :: Maybe Text
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
    , studyTemplateName :: C f (Maybe Text)
    , name :: C f Text
    , document :: C f (PgJSONB Object)
    , editors :: C f (PgJSONB (Vector User.GetUser'))
    , updated :: C f UTCTime
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
      studyTemplateName
      name
      (Db.unPgJSONB document)
      (fmap E.toEntity (toList (Db.unPgJSONB editors)))
      updated
      created

  queryEntity mAuthUser = do
    doc <- all_ Db.db.document

    guard_ $ not_ doc.isDeleted

    for_ mAuthUser $ \ authUser -> guard_ $ exists_ $ do
      user <- all_ Db.db.documentEditor
      guard_ $ user.docId ==. doc.docId
      guard_ $ val_ authUser.userId ==. user.userId
      pure user.docId

    let editors = jsonArraryOf $ do
                    de <- all_ Db.db.documentEditor
                    guard_ $ de.docId ==. doc.docId
                    user <- E.queryEntityBy @User.GetUser Nothing de.userId
                    pure $ jsonBuildObject user

    sg <- leftJoin_'
            (all_ Db.db.groupStudy)
            (\ r -> just_ r.groupStudyId ==?. doc.groupStudyId)


    pure $
      MkDbGetDoc
        doc.docId
        doc.groupStudyId
        doc.studyTemplateId
        sg.name
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



getDocInStudyGroup
  :: MonadDb env m
  => AuthUser
  -> T.DocId
  -> m (Maybe GetDoc)
getDocInStudyGroup authUser docId =
  fmap (fmap E.toEntity)
  $ runBeam
  $ runSelectReturningOne
  $ select
  $ do
    doc <- E.queryEntityBy @GetDoc Nothing (val_ docId)

    guard_ $ exists_ $ do
      doc2 <- E.queryEntity @GetDoc (Just authUser)
      guard_' $ doc2.groupStudyId ==?. doc.groupStudyId
      pure doc2.docId

    pure doc



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


updateDocMeta
  :: MonadDb env m
  => T.DocId
  -> Text
  -> m ()
updateDocMeta docId name = do
  now <- getCurrentTime
  runBeam
    $ runUpdate
    $ update
      Db.db.document
      (\ r ->
           (r.name <-. val_ name)
        <> (r.updated <-. val_ now)
      )
      (\ r -> r.docId ==. val_ docId)


deleteDocument
  :: MonadDb env m
  => T.DocId
  -> m ()
deleteDocument docId = do
  runBeam
    $ runUpdate
    $ update
      Db.db.document
      (\ r -> r.isDeleted <-. val_ True)
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
          (val_ False)
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


getAllDocs
  :: MonadDb env m
  => AuthUser
  -> m [GetDoc]
getAllDocs user =
  fmap (fmap E.toEntity)
  $ runBeam
  $ runSelectReturningList
  $ select
  $ orderBy_ (desc_ . (.updated))
  $ do
    E.queryEntity @GetDoc (Just user)
