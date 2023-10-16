module EnvFields where

import GHC.Records (HasField)

data EnvType
  = Prod
  | Dev Text
  deriving (Show, Generic, Eq)
  deriving anyclass (FromJSON, ToJSON)

type HasEnvType env = HasField "envType" env EnvType

type HasUrl env = HasField "url" env Text
