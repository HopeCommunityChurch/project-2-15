module Entity.User where

import Data.Time.Clock (diffUTCTime)
import Data.Time.Lens qualified as TL
import Database qualified as Db
import Database.Beam (
  Beamable,
  C,
  all_,
  default_,
  delete,
  guard_,
  in_,
  insert,
  insertExpressions,
  insertValues,
  runDelete,
  runInsert,
  runSelectReturningOne,
  select,
  val_,
  (==.),
  (&&.),
  (<-.),
  (>=.), runUpdate, update, SqlDeconstructMaybe (isNothing_),
 )
import Database.Beam.Backend.SQL.BeamExtensions (runInsertReturningList)
import DbHelper (MonadDb, runBeam)
import Entity qualified as E
import Entity.AuthUser
import Password (NewPassword, getHash)
import Types qualified as T


data GetUser = MkGetUser
  { userId :: T.UserId
  , name :: Text
  , image :: Maybe Text
  , email :: T.Email
  }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON, ToSchema)



instance E.Entity GetUser where
  data DbEntity GetUser f = MkDbGetUser
    { userId :: C f T.UserId
    , name :: C f Text
    , image :: C f (Maybe Text)
    , email :: C f T.Email
    }
    deriving anyclass (Beamable)
    deriving (Generic)

  type EntityDatabase GetUser = Db.Db

  toEntity MkDbGetUser{..} =
    MkGetUser
      userId
      name
      image
      email

  queryEntity mAuthUser = do
    user <- all_ Db.db.user
    for_ mAuthUser $ \ authUser ->
      guard_ $ user.churchId ==. val_ authUser.churchId

    pure $
      MkDbGetUser
        user.userId
        user.name
        user.image
        user.email

type GetUser' = DbEntity GetUser Identity

instance FromJSON GetUser'

instance E.GuardValue GetUser T.UserId where
  guardValues ids user =
    guard_ $ user.userId `in_` ids


data NewUser = MkNewUser
  { email :: T.Email
  , name :: Text
  , password :: NewPassword
  , churchId :: T.ChurchId
  }
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)


createUser
  :: MonadDb env m
  => NewUser
  -> m T.UserId
createUser newUser = do
  [user] <-
    runBeam
      $ runInsertReturningList
      $ insert Db.db.user
      $ insertExpressions
        [ Db.MkUserT
          default_
          (val_ newUser.email)
          (val_ newUser.name)
          (val_ Nothing)
          (val_ newUser.churchId)
          (val_ False)
          default_
        ]
  hash <- liftIO $ getHash newUser.password
  runBeam
    $ runInsert
    $ insert Db.db.userPassword
    $ insertValues
      [ Db.MkUserPasswordT
        (user.userId)
        hash
      ]
  pure user.userId


data UpdateUser = MkUpdateUser
  { email :: T.Email
  , name :: Text
  }
  deriving (Show, Generic)


updateUser
  :: MonadDb env m
  => T.UserId
  -> UpdateUser
  -> m ()
updateUser userId up =
  runBeam
  $ runUpdate
  $ update
      Db.db.user
      (\ user -> (user.email <-. val_ up.email)
              <>  (user.name <-. val_ up.name)
      )
      (\ user -> user.userId ==. val_ userId)


passwordResetToken
  :: MonadDb env m
  => T.Email
  -> m (Maybe T.PasswordResetToken)
passwordResetToken email = do
  mUser <- getUser
  forM mUser $ \ user -> do
    token <- T.genPasswordResetToken
    insertToken user.userId token
    pure token
  where
    getUser =
      runBeam
        $ runSelectReturningOne
        $ select
        $ do
          user <- all_ Db.db.user
          guard_ $ user.email ==. val_ email
          pure user

    insertToken userId token = do
      now <- getCurrentTime
      let expiresAt = now & (TL.flexDT . TL.minutes) +~ 10
      runBeam
        $ runInsert
        $ insert Db.db.userPasswordReset
        $ insertValues
          [ Db.MkUserPasswordResetT
            userId
            token
            Nothing
            expiresAt
            now
          ]

getUserFromResetToken
  :: MonadDb env m
  => T.PasswordResetToken
  -> m (Maybe T.UserId)
getUserFromResetToken token = do
  now <- getCurrentTime
  userId <- runBeam
    $ runSelectReturningOne
    $ select
    $ do
      reset <- all_ Db.db.userPasswordReset
      guard_ $ reset.token ==. val_ token
      guard_ $ reset.expiresAt >=. val_ now
      guard_ $ isNothing_ $ reset.usedAt
      pure reset.userId


  pure userId


invalidateResetToken
  :: MonadDb env m
  => T.PasswordResetToken
  -> m ()
invalidateResetToken token = do
  now <- getCurrentTime
  runBeam
    $ runUpdate
    $ update
        Db.db.userPasswordReset
        (\ r -> r.usedAt <-. val_ (Just now))
        (\ r -> r.token ==. val_ token)


updatePassword
  :: MonadDb env m
  => T.UserId
  -> NewPassword
  -> m ()
updatePassword userId pwd = do
  hash <- liftIO $ getHash pwd
  runBeam
    $ runUpdate
    $ update
        Db.db.userPassword
        (\ r -> r.password <-. val_ hash)
        (\ r -> r.userId ==. val_ userId)


createVerificationToken
  :: MonadDb env m
  => T.UserId
  -> T.Email
  -> m T.EmailVerificationToken
createVerificationToken userId email = do
  runBeam
    $ runDelete
    $ delete
        Db.db.userEmailVerification
        (\ v -> v.userId ==. val_ userId &&. isNothing_ v.usedAt)
  now <- getCurrentTime
  token <- T.genEmailVerificationToken
  let expiresAt = now & (TL.flexDT . TL.hours) +~ 24
  runBeam
    $ runInsert
    $ insert Db.db.userEmailVerification
    $ insertValues
      [ Db.MkUserEmailVerificationT
          token
          userId
          email
          expiresAt
          Nothing
          now
          now
      ]
  pure token


verifyEmailToken
  :: MonadDb env m
  => T.EmailVerificationToken
  -> m (Maybe T.UserId)
verifyEmailToken token = do
  now <- getCurrentTime
  mVerification <- runBeam
    $ runSelectReturningOne
    $ select
    $ do
      v <- all_ Db.db.userEmailVerification
      guard_ $ v.token ==. val_ token
      guard_ $ v.expiresAt >=. val_ now
      guard_ $ isNothing_ v.usedAt
      pure v
  forM mVerification $ \ v -> do
    runBeam
      $ runUpdate
      $ update
          Db.db.user
          (\ u -> (u.email <-. val_ v.email)
               <> (u.emailVerified <-. val_ True)
          )
          (\ u -> u.userId ==. val_ v.userId)
    runBeam
      $ runUpdate
      $ update
          Db.db.userEmailVerification
          (\ r -> r.usedAt <-. val_ (Just now))
          (\ r -> r.token ==. val_ token)
    pure v.userId


checkVerificationRateLimit
  :: MonadDb env m
  => T.UserId
  -> m (Maybe Int)
checkVerificationRateLimit userId = do
  now <- getCurrentTime
  mVerification <- runBeam
    $ runSelectReturningOne
    $ select
    $ do
      v <- all_ Db.db.userEmailVerification
      guard_ $ v.userId ==. val_ userId
      guard_ $ isNothing_ v.usedAt
      pure v
  pure $ do
    v <- mVerification
    let cooldownSeconds = 300 :: Int
        elapsed = round (diffUTCTime now v.sentAt) :: Int
        remaining = cooldownSeconds - elapsed
    if remaining > 0 then Just remaining else Nothing


updateVerificationSentAt
  :: MonadDb env m
  => T.UserId
  -> m ()
updateVerificationSentAt userId = do
  now <- getCurrentTime
  runBeam
    $ runUpdate
    $ update
        Db.db.userEmailVerification
        (\ v -> v.sentAt <-. val_ now)
        (\ v -> v.userId ==. val_ userId &&. isNothing_ v.usedAt)


getUnverifiedUserId
  :: MonadDb env m
  => T.Email
  -> m (Maybe T.UserId)
getUnverifiedUserId email =
  runBeam
  $ runSelectReturningOne
  $ select
  $ do
    user <- all_ Db.db.user
    guard_ $ user.email ==. val_ email
    guard_ $ user.emailVerified ==. val_ False
    pure user.userId

