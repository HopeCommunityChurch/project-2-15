{-# OPTIONS_GHC -fno-warn-orphans #-}

module Orphans where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.CaseInsensitive qualified as CI
import Data.OpenApi (ToSchema (..))

instance FromJSON (CI.CI Text) where
  parseJSON = fmap CI.mk . parseJSON

instance ToJSON (CI.CI Text) where
  toJSON = toJSON . CI.original

instance ToSchema (CI.CI Text) where
  declareNamedSchema _ =
    declareNamedSchema (Proxy @Text)
