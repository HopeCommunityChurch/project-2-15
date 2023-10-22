module Entity.Shares where

import Data.Time.Lens qualified as TL
import Database qualified as Db
import Database.Beam (
  Beamable,
  C,
  all_,
  not_,
  delete,
  guard_,
  in_,
  insert,
  insertValues,
  isNothing_,
  just_,
  leftJoin_',
  runDelete,
  runInsert,
  runSelectReturningList,
  runSelectReturningOne,
  runUpdate,
  select,
  update,
  val_,
  (&&.),
  (<-.),
  (==.),
  (==?.),
  (>=.),
 )
import DbHelper (MonadDb, runBeam, withTransaction)
import Entity qualified as E
import Entity.AuthUser
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
    $ runDelete
    $ delete Db.db.groupStudyShare
      (\ r -> r.email `in_` fmap (val_ . (.email)) shares
            &&. r.groupStudyId ==. val_ gsId
      )

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
          False
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
    , usedAt :: C f (Maybe UTCTime)
    , expiresAt :: C f UTCTime
    , rejected :: C f Bool
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
        share.usedAt
        share.expiresAt
        share.rejected
        share.created


getSharesForUser
  :: MonadDb env m
  => AuthUser
  -> m [GetMyShareData]
getSharesForUser auth = do
  now <- getCurrentTime
  fmap (fmap E.toEntity)
    $ runBeam
    $ runSelectReturningList
    $ select
    $ do
      share <- E.queryEntity @GetMyShareData (Just auth)
      guard_ $ isNothing_ share.usedAt
      guard_ $ share.expiresAt >=. val_ now
      guard_ $ not_ share.rejected
      pure share


getShareFromToken
  :: MonadDb env m
  => T.ShareToken
  -> m (Maybe GetMyShareData)
getShareFromToken token = do
  now <- getCurrentTime
  fmap (fmap E.toEntity)
    $ runBeam
    $ runSelectReturningOne
    $ select
    $ do
      share <- E.queryEntity @GetMyShareData Nothing
      guard_ $ isNothing_ share.usedAt
      guard_ $ share.expiresAt >=. val_ now
      guard_ $ share.token ==. val_ token
      guard_ $ not_ share.rejected
      pure share


rejectToken
  :: MonadDb env m
  => T.ShareToken
  -> m ()
rejectToken token =
  runBeam
    $ runUpdate
    $ update Db.db.groupStudyShare
      ( \ r -> r.rejected <-. val_ True)
      ( \ r -> r.shareToken ==. val_ token)


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
              guard_ $ not_ share.rejected
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


data GetShareData = MkGetShareData
  { email :: T.Email
  , expiresAt :: UTCTime
  , rejected :: Bool
  , created :: UTCTime
  }
  deriving (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)


getGroupShareData
  :: MonadDb env m
  => T.GroupStudyId
  -> m [GetShareData]
getGroupShareData gsId =
  fmap (fmap (\(email, ex, r, cr) -> MkGetShareData email ex r cr))
  $ runBeam
  $ runSelectReturningList
  $ select
  $ do
    share <- all_ Db.db.groupStudyShare
    guard_ $ share.groupStudyId ==. val_ gsId
    guard_ $ isNothing_ share.usedAt
    pure (share.email, share.expiresAt, share.rejected, share.created)


deleteShare
  :: MonadDb env m
  => T.GroupStudyId
  -> T.Email
  -> m ()
deleteShare gsId email =
  runBeam
    $ runDelete
    $ delete Db.db.groupStudyShare
      (\ r -> r.email ==. val_ email &&. r.groupStudyId ==. val_ gsId)

