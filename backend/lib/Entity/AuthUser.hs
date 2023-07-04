module Entity.AuthUser where

import qualified Types as T
import qualified Database as Db
import qualified Entity as E
import Database.Beam (C, Beamable, all_, exists_, guard_, (==.), in_)


data AuthUser = MkAuthUser
  { userId :: T.UserId
  , isElder :: Bool
  , churchId :: T.ChurchId
  }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON, ToSchema)

type instance E.DbUser Db.Db = AuthUser



instance E.Entity AuthUser where
  data DbEntity AuthUser f = MkDbAuthUser
    { userId :: C f T.UserId
    , isElder :: C f Bool
    , churchId :: C f T.ChurchId
    }
    deriving anyclass (Beamable)
    deriving (Generic)

  type EntityDatabase AuthUser = Db.Db

  toEntity MkDbAuthUser{..} =
    MkAuthUser
      userId
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
        isElder
        user.churchId

instance E.GuardValue AuthUser T.UserId where
  guardValues ids user =
    guard_ $ user.userId `in_` ids
