module EnvFields where

import GHC.Records (HasField)

data EnvType
  = Prod
  | Dev Text
  deriving (Show, Generic)

type HasEnvType env = HasField "envType" env EnvType
