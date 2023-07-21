module Entity.User where

import qualified Types as T
import qualified Database as Db
import qualified Entity as E
import DbHelper (MonadDb, runBeam)
import Database.Beam (C, Beamable, all_, guard_, (==.), insertExpressions, val_, in_, val_, insert, default_, runInsert, insertValues)
import Entity.AuthUser
import Password (NewPassword, getHash)
import Database.Beam.Backend.SQL.BeamExtensions (runInsertReturningList)


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

