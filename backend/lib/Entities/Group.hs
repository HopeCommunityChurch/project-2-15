module Entities.Group where

import qualified Types as T
import qualified Entities.User as User

data Group = MkGroup
  { groupId :: T.GroupId
  , name :: Text
  , leaders :: List User.GetUser
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)


-- TODO: define how to get from the database
