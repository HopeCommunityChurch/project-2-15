module Entity.User where

import Data.Time.Lens qualified as TL
import Database qualified as Db
import Database.Beam (
  Beamable,
  C,
  all_,
  default_,
  guard_,
  in_,
  insert,
  insertExpressions,
  insertValues,
  runInsert,
  runSelectReturningOne,
  select,
  val_,
  (==.),
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
  }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON, ToSchema)



instance E.Entity GetUser where
  data DbEntity GetUser f = MkDbGetUser
    { userId :: C f T.UserId
    , name :: C f Text
    , image :: C f (Maybe Text)
    }
    deriving anyclass (Beamable)
    deriving (Generic)

  type EntityDatabase GetUser = Db.Db

  toEntity MkDbGetUser{..} =
    MkGetUser
      userId
      name
      image

  queryEntity mAuthUser = do
    user <- all_ Db.db.user
    for_ mAuthUser $ \ authUser ->
      guard_ $ user.churchId ==. val_ authUser.churchId

    pure $
      MkDbGetUser
        user.userId
        user.name
        user.image

type GetUser' = DbEntity GetUser Identity

instance FromJSON GetUser'

instance E.GuardValue GetUser T.UserId where
  guardValues ids user =
    guard_ $ user.userId `in_` ids


data NewUser = NewUser
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

