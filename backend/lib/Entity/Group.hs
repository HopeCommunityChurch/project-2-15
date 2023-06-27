module Entity.Group where

import qualified Types as T
import qualified Entity.User as User

data GetGroup = MkGetGroup
  { groupId :: T.GroupId
  , name :: Text
  , leaders :: List User.GetUser
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)


-- TODO: define how to get from the database
