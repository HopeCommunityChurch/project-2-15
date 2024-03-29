module Entity.AuthUser where

import Database qualified as Db
import Database.Beam (
  Beamable,
  C,
  all_,
  exists_,
  guard_,
  in_,
  (==.),
 )
import Entity qualified as E
import Types qualified as T
import Data.Text qualified as T


data AuthUser = MkAuthUser
  { userId :: T.UserId
  , email :: T.Email
  , name :: Text
  , nameShort :: Text
  , isElder :: Bool
  , churchId :: T.ChurchId
  }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON, ToSchema)

type instance E.DbUser Db.Db = AuthUser



instance E.Entity AuthUser where
  data DbEntity AuthUser f = MkDbAuthUser
    { userId :: C f T.UserId
    , email :: C f T.Email
    , name :: C f Text
    , isElder :: C f Bool
    , churchId :: C f T.ChurchId
    }
    deriving anyclass (Beamable)
    deriving (Generic)

  type EntityDatabase AuthUser = Db.Db

  toEntity MkDbAuthUser{..} =
    MkAuthUser
      userId
      email
      name
      (T.toUpper (T.take 2 name))
      isElder
      churchId

  queryEntity _ = do
    user <- all_ Db.db.user
    let isElder = exists_ $ do
                    elder <- all_ Db.db.churchElder
                    guard_ $ elder.userId ==. user.userId
                    guard_ $ elder.churchId ==. user.churchId
                    pure elder.userId

    pure $
      MkDbAuthUser
        user.userId
        user.email
        user.name
        isElder
        user.churchId

instance E.GuardValue AuthUser T.UserId where
  guardValues ids user =
    guard_ $ user.userId `in_` ids

