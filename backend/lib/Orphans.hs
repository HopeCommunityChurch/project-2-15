{-# OPTIONS_GHC -fno-warn-orphans #-}

module Orphans where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.CaseInsensitive qualified as CI
import Data.OpenApi (ToSchema (..))
import Web.Scotty (Parsable(..))
import Data.UUID (UUID)
import Data.UUID qualified as UUID

instance FromJSON (CI.CI Text) where
  parseJSON = fmap CI.mk . parseJSON

instance ToJSON (CI.CI Text) where
  toJSON = toJSON . CI.original

instance ToSchema (CI.CI Text) where
  declareNamedSchema _ =
    declareNamedSchema (Proxy @Text)


instance Parsable UUID where
  parseParam txt =
    case UUID.fromText (toStrict txt) of
      Just uuid -> pure uuid
      Nothing -> Left "not a valid uuid"
