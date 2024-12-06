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
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamUpdateReturning(runUpdateReturningList))



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


expandShareExpire
  :: MonadDb env m
  => T.ShareToken
  -> m ShareUnit
expandShareExpire token = do
  now <- getCurrentTime
  [result] <-
    runBeam
      $ runUpdateReturningList
      $ update
          Db.db.groupStudyShare
          (\ r ->
            (r.expiresAt <-. val_ (now & (TL.flexDT . TL.days) +~ 14))
            <> (r.rejected <-. val_ False)
          )
          (\ r -> r.shareToken ==. val_ token)
  pure (MkShareUnit result.email result.shareAsOwner result.message)


data GetMyShareData = MkGetMyShareData
  { token :: T.ShareToken
  , email :: T.Email
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
    , email :: C f T.Email
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
      email
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
        share.email
        gs.groupStudyId
        gs.name
        template.studyTemplateId
        template.name
        share.usedAt
        share.expiresAt
        share.rejected
        share.created


instance E.GuardValue GetMyShareData T.GroupStudyId where
  guardValues ids doc =
    guard_ $ doc.groupStudyId `in_` ids


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


deleteToken
  :: MonadDb env m
  => T.ShareToken
  -> m ()
deleteToken token =
  runBeam
    $ runDelete
    $ delete Db.db.groupStudyShare
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
  , token :: T.ShareToken
  , expiresAt :: UTCTime
  , isExpired :: Bool
  , rejected :: Bool
  , created :: UTCTime
  }
  deriving (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)


getGroupShareData
  :: MonadDb env m
  => T.GroupStudyId
  -> m [GetShareData]
getGroupShareData gsId = do
  now <- getCurrentTime
  fmap
    (fmap
      (\(ex, token, email, r, cr) ->
        MkGetShareData
          email
          token
          ex
          (ex < now)
          r
          cr
      )
    )
    $ runBeam
    $ runSelectReturningList
    $ select
    $ do
      share <- all_ Db.db.groupStudyShare
      guard_ $ share.groupStudyId ==. val_ gsId
      guard_ $ isNothing_ share.usedAt
      pure (share.expiresAt, share.shareToken, share.email, share.rejected, share.created)


getGroupShareDataByToken
  :: MonadDb env m
  => T.ShareToken
  -> m (Maybe GetShareData)
getGroupShareDataByToken shareToken = do
  now <- getCurrentTime
  fmap
    (fmap
      (\(ex, token, email, r, cr) ->
        MkGetShareData
          email
          token
          ex
          (ex < now)
          r
          cr
      )
    )
    $ runBeam
    $ runSelectReturningOne
    $ select
    $ do
      share <- all_ Db.db.groupStudyShare
      guard_ $ share.shareToken ==. val_ shareToken
      guard_ $ isNothing_ share.usedAt
      pure (share.expiresAt, share.shareToken, share.email, share.rejected, share.created)


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

