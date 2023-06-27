module Entity.UserAssignable where

import qualified Entity.User as User
import qualified Entity.Group as Group


data GetUserAssignable
  = GetUser User.GetUser
  | GetGroup Group.GetGroup
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)



-- TODO: define how to get from the database
