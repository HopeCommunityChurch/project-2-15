module Entity.User where

import qualified Types as T
import qualified Database as Db
import qualified Entity as E
import Database.Beam (C, Beamable, all_, guard_, (==.), val_, in_, val_)
import Entity.AuthUser


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

instance E.GuardValue GetUser T.UserId where
  guardValues ids user =
    guard_ $ user.userId `in_` ids



