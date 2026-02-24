module Entity.Document where

import Data.Aeson (Object, Value)
import Database qualified as Db
import Database.Beam (
  Beamable,
  C,
  all_,
  asc_,
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
  limit_,
  (<-.),
  (==.),
  (==?.),
  (>.)
  , (<=.)
  )
import Database.Beam.Backend.SQL.BeamExtensions (runInsertReturningList)
import Database.Beam.Postgres (PgJSONB (..))
import DbHelper (MonadDb, jsonArraryOf, jsonBuildObject, runBeam, withRawConnection)
import Database.PostgreSQL.Simple qualified as PgS
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Aeson qualified as Aeson
import Entity qualified as E
import Entity.AuthUser
import Entity.User qualified as User
import Types qualified as T

data LastUpdate = MkLastUpdate
  { computerId :: T.ComputerId
  , time :: UTCTime
  }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON, ToSchema)

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
  , version :: Int32
  , lastUpdate :: Maybe LastUpdate
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
    , version :: C f Int32
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
      version
      Nothing

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
        doc.version

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
  -> T.UserId
  -> T.ComputerId
  -> Object
  -> m UTCTime
updateDocument docId userId computerId document = do
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
  runBeam
    $ runInsert
    $ insert Db.db.documentSave
    $ insertValues
      [ Db.MkDocumentSaveT
        docId
        userId
        computerId
        now
      ]
  pure now


addToGroup
  :: MonadDb env m
  => T.DocId
  -> T.GroupStudyId
  -> m ()
addToGroup docId groupId =
  runBeam
    $ runUpdate
    $ update
      Db.db.document
      (\ r -> r.groupStudyId <-. val_ (Just groupId))
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
          (val_ (0 :: Int32))
        ]
  runBeam
    $ runInsert
    $ insert Db.db.documentEditor
    $ insertValues
      [ Db.MkDocumentEditorT
        doc.docId
        crDoc.editor
      ]
  insertSnapshot doc.docId 0 crDoc.document
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


getLastUpdate
  :: MonadDb env m
  => T.DocId
  -> m (Maybe LastUpdate)
getLastUpdate docId =
  fmap (fmap (\ s -> MkLastUpdate s.computerId s.time))
  $ runBeam
  $ runSelectReturningOne
  $ select
  $ limit_ 1
  $ orderBy_ (desc_ . (.time))
  $ do
    save <- all_ Db.db.documentSave
    guard_ $ save.docId ==. val_ docId
    pure save


getDocVersion
  :: MonadDb env m
  => T.DocId
  -> m Int32
getDocVersion docId = do
  mVersion <-
    runBeam
    $ runSelectReturningOne
    $ select
    $ do
      doc <- all_ Db.db.document
      guard_ $ doc.docId ==. val_ docId
      pure doc.version
  pure $ fromMaybe 0 mVersion


updateDocVersion
  :: MonadDb env m
  => T.DocId
  -> Int32
  -> m ()
updateDocVersion docId newVersion =
  runBeam
    $ runUpdate
    $ update
      Db.db.document
      (\ r -> r.version <-. val_ newVersion)
      (\ r -> r.docId ==. val_ docId)


insertSteps
  :: MonadDb env m
  => T.DocId
  -> Int32         -- ^ current version (steps are inserted at startVersion+1 .. startVersion+N)
  -> T.UserId
  -> T.ComputerId  -- ^ stores the full tab-session clientId
  -> [Value]
  -> m ()
insertSteps _      _            _      _          []    = pure ()
insertSteps docId  startVersion userId clientId   steps = do
  now <- getCurrentTime
  let rows = zipWith
               (\ i s -> Db.MkDocumentStepT docId (startVersion + i) (PgJSONB s) userId clientId now)
               [1..]
               steps
  runBeam
    $ runInsert
    $ insert Db.db.documentStep
    $ insertValues rows


-- | Returns (version, step JSON, clientId) for all steps after fromVersion, ordered ascending.
getStepsSince
  :: MonadDb env m
  => T.DocId
  -> Int32
  -> m [(Int32, Value, T.ComputerId)]
getStepsSince docId fromVersion =
  fmap (fmap (\ r -> (r.version, Db.unPgJSONB r.step, r.computerId)))
  $ runBeam
  $ runSelectReturningList
  $ select
  $ orderBy_ (asc_ . (.version))
  $ do
    s <- all_ Db.db.documentStep
    guard_ $ s.docId ==. val_ docId
    guard_ $ s.version >. val_ fromVersion
    pure s


insertSnapshot
  :: MonadDb env m
  => T.DocId
  -> Int32
  -> Object
  -> m ()
insertSnapshot docId atVer docObj = do
  now <- getCurrentTime
  runBeam
    $ runInsert
    $ insert Db.db.documentSnapshot
    $ insertValues
      [ Db.MkDocumentSnapshotT docId atVer (PgJSONB docObj) now ]


-- | Like insertSnapshot but uses ON CONFLICT DO NOTHING, so it is safe to
-- call concurrently (e.g. from handleOpenDoc when two users open the same
-- legacy document at the same time).
insertSnapshotIfAbsent
  :: MonadDb env m
  => T.DocId
  -> Int32
  -> Object
  -> m ()
insertSnapshotIfAbsent docId atVer docObj =
  withRawConnection $ \conn -> liftIO $ do
    now <- getCurrentTime
    void $ PgS.execute conn
      "INSERT INTO document_snapshot (\"docId\", \"atVersion\", document, \"createdAt\") \
      \VALUES (?, ?, ?::jsonb, ?) \
      \ON CONFLICT (\"docId\", \"atVersion\") DO NOTHING"
      (docId, atVer, TLE.decodeUtf8 (Aeson.encode docObj), now)


getLatestSnapshotBefore
  :: MonadDb env m
  => T.DocId
  -> Int32
  -> m (Maybe (Int32, Object))
getLatestSnapshotBefore docId atMost =
  fmap (fmap (\ r -> (r.atVersion, Db.unPgJSONB r.document)))
  $ runBeam
  $ runSelectReturningOne
  $ select
  $ limit_ 1
  $ orderBy_ (desc_ . (.atVersion))
  $ do
    snap <- all_ Db.db.documentSnapshot
    guard_ $ snap.docId ==. val_ docId
    guard_ $ snap.atVersion <=. val_ atMost
    pure snap


-- | Returns the document's current version and content from the documents
-- table. Used as a fallback baseline for history reconstruction when no
-- collab snapshot has been recorded yet (e.g. legacy documents).
getDocBase
  :: MonadDb env m
  => T.DocId
  -> m (Maybe (Int32, Object))
getDocBase docId =
  fmap (fmap (\ r -> (r.version, Db.unPgJSONB r.document)))
  $ runBeam
  $ runSelectReturningOne
  $ select
  $ do
    doc <- all_ Db.db.document
    guard_ $ doc.docId ==. val_ docId
    pure doc


-- | Called from handleSave; takes a snapshot when version is a multiple of 50.
maybeTakeSnapshot
  :: MonadDb env m
  => T.DocId
  -> Object
  -> m ()
maybeTakeSnapshot docId docObj = do
  v <- getDocVersion docId
  when (v > 0 && v `mod` 50 == 0) $
    insertSnapshot docId v docObj
