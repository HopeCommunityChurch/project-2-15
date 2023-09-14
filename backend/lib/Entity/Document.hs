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
  val_,
  (<-.),
  (==.),
 )
import Database.Beam.Postgres (PgJSONB (..))
import DbHelper (MonadDb, jsonArraryOf, jsonBuildObject, runBeam)
import Entity qualified as E
import Entity.AuthUser
import Entity.User qualified as User
import Entity.Study qualified as Study
import Types qualified as T


data GetDoc = MkGetDoc
  { docId :: T.DocId
  , studyId :: T.StudyId
  , study :: Study.GetStudy
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
    , studyId :: C f T.StudyId
    , name :: C f Text
    , document :: C f (PgJSONB Object)
    , editors :: C f (PgJSONB (Vector User.GetUser'))
    , study :: C f (PgJSONB Study.GetStudy')
    , update :: C f UTCTime
    , created :: C f UTCTime
    }
    deriving anyclass (Beamable)
    deriving (Generic)

  type EntityDatabase GetDoc = Db.Db

  toEntity MkDbGetDoc{..} =
    MkGetDoc
      docId
      studyId
      (E.toEntity (Db.unPgJSONB study))
      name
      (Db.unPgJSONB document)
      (fmap E.toEntity (toList (Db.unPgJSONB editors)))
      update
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
    study <- E.queryEntityBy @Study.GetStudy Nothing doc.studyId


    pure $
      MkDbGetDoc
        doc.docId
        doc.studyId
        doc.name
        doc.document
        editors
        (jsonBuildObject study)
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
