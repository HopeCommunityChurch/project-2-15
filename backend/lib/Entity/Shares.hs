module Entity.Shares where

import Data.Aeson (Object)
import Data.Time.Lens qualified as TL
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
  insertValues,
  isNothing_,
  just_,
  leftJoin_',
  runInsert,
  runSelectReturningOne,
  runUpdate,
  select,
  update,
  val_,
  (<-.),
  (==.),
  (==?.),
  (>=.),
 )
import Database.Beam.Backend.SQL.BeamExtensions (runInsertReturningList)
import Database.Beam.Postgres (PgJSONB (..))
import DbHelper (HasDbConn, MonadDb, asJust_, jsonArraryOf, jsonBuildObject, runBeam, withTransaction)
import Entity qualified as E
import Entity.AuthUser
import Entity.User
import Types qualified as T



data ShareUnit = MkShareUnit
  { email :: T.Email
  , asOwner :: Bool
  , message :: Maybe Text
  }
  deriving (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)


addShares
  :: MonadDb env m
  => T.GroupStudyId
  -> [ShareUnit]
  -> m [(ShareUnit, T.ShareToken)]
addShares gsId shares = do
  st <- forM shares $ \ email -> do
    token <- T.genShareToken
    pure (email, token)
  now <- getCurrentTime

  runBeam
    $ runInsert
    $ insert Db.db.groupStudyShare
    $ insertValues
    $ st <&>
      (\ (MkShareUnit{email, asOwner, message}, token) ->
        Db.MkGroupStudyShareT
          gsId
          token
          email
          asOwner
          (now & (TL.flexDT . TL.days) +~ 14)
          Nothing
          message
          now
      )
  pure st


data GetMyShareData = MkGetMyShareData
  { token :: T.ShareToken
  , groupStudyId :: T.GroupStudyId
  , groupStudyName :: Text
  , studyTemplateId :: Maybe T.StudyTemplateId
  , studyTemplateName :: Maybe Text
  }
  deriving (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)


instance E.Entity GetMyShareData where
  data DbEntity GetMyShareData f = MkDbGetMyShareData
    { token :: C f T.ShareToken
    , groupStudyId :: C f T.GroupStudyId
    , groupStudyName :: C f Text
    , studyTemplateId :: C f (Maybe T.StudyTemplateId)
    , studyTemplateName :: C f (Maybe Text)
    , created :: C f UTCTime
    }
    deriving anyclass (Beamable)
    deriving (Generic)

  type EntityDatabase GetMyShareData = Db.Db

  toEntity MkDbGetMyShareData{..} =
    MkGetMyShareData
      token
      groupStudyId
      groupStudyName
      studyTemplateId
      studyTemplateName

  queryEntity mAuthUser = do
    share <- all_ Db.db.groupStudyShare
    gs <- all_ Db.db.groupStudy
    guard_ $ share.groupStudyId ==. gs.groupStudyId
    template <- leftJoin_'
            (all_ Db.db.studyTemplate)
            (\ r -> just_ r.studyTemplateId ==?. gs.studyTemplateId)

    for_ mAuthUser $ \ user ->
      guard_ $ share.email ==. val_ user.email


    pure $
      MkDbGetMyShareData
        share.shareToken
        gs.groupStudyId
        gs.name
        template.studyTemplateId
        template.name
        share.created



acceptShare
  :: MonadDb env m
  => T.UserId
  -> T.ShareToken
  -> T.DocId
  -> m Bool
acceptShare userId token docId = withTransaction $ do
  now <- getCurrentTime
  mgsId <- runBeam
            $ runSelectReturningOne
            $ select
            $ do
              share <- all_ Db.db.groupStudyShare
              guard_ $ isNothing_ share.usedAt
              guard_ $ share.expiresAt >=. val_ now
              guard_ $ share.shareToken ==. val_ token
              pure (share.groupStudyId, share.shareAsOwner)
  result <- forM mgsId $ \ (gsId, owner) -> do
    runBeam
      $ runUpdate
      $ update Db.db.groupStudyShare
        ( \ r -> r.usedAt <-. val_ (Just now))
        ( \ r -> r.shareToken ==. val_ token)
    runBeam
      $ runUpdate
      $ update Db.db.document
        ( \ r -> r.groupStudyId <-. val_ (Just gsId))
        ( \ r -> r.docId ==. val_ docId)
    when owner $
      runBeam
      $ runInsert
      $ insert Db.db.groupStudyOwner
      $ insertValues
        [ Db.MkGroupStudyOwnerT
          gsId
          userId
        ]
  pure $ isJust result
