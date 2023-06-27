module Entity.User where

import qualified Types as T

data GetUser = GetUser
  { testuserId :: T.UserId
  , testname :: Text
  , testimage :: Maybe Text
  }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON, ToSchema)


-- TODO: define how to get from the database
