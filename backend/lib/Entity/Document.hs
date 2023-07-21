module Entity.Document where

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
  val_,
  (==.), runInsert, insertValues,
 )
import Database.Beam.Backend.SQL.BeamExtensions (
  MonadBeamInsertReturning (runInsertReturningList),
 )
import Database.Beam.Postgres (PgJSONB (..))
import DbHelper (MonadDb, jsonArraryOf, jsonBuildObject, runBeam)
import Entity qualified as E
import Entity.AuthUser
import Entity.User qualified as User
import Types qualified as T


data GetDoc = MkGetDoc
  { docId :: T.DocId
  , study :: T.StudyId
  , name :: Text
  , document :: Object
  , editors :: List User.GetUser
  , created :: UTCTime
  }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON, ToSchema)



instance E.Entity GetDoc where
  data DbEntity GetDoc f = MkDbGetDoc
    { docId :: C f T.DocId
    , studyId :: C f T.StudyId
    , name :: C f Text
    , document :: C f (PgJSONB Object)
    , editors :: C f (PgJSONB (Vector User.GetUser'))
    , created :: C f UTCTime
    }
    deriving anyclass (Beamable)
    deriving (Generic)

  type EntityDatabase GetDoc = Db.Db

  toEntity MkDbGetDoc{..} =
    MkGetDoc
      docId
      studyId
      name
      (Db.unPgJSONB document)
      (fmap E.toEntity (toList (Db.unPgJSONB editors)))
      created

  queryEntity mAuthUser = do
    doc <- all_ Db.db.document

    for_ mAuthUser $ \ authUser -> guard_ $ exists_ $ do
      doc2 <- all_ Db.db.document
      guard_ $ doc.studyId ==. doc2.studyId
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
        doc.studyId
        doc.name
        doc.document
        editors
        doc.created

instance E.GuardValue GetDoc T.DocId where
  guardValues ids doc =
    guard_ $ doc.docId `in_` ids

instance E.EntityWithId GetDoc where
  type EntityId GetDoc = T.DocId
  entityId = (.docId)


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

