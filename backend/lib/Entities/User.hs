module Entities.User where

import qualified Types as T

data GetUser = GetUser
  { userId :: T.UserId
  , name :: Text
  , image :: Maybe Text
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)


-- TODO: define how to get from the database
