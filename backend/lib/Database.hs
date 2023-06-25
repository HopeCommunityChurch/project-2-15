module Database where

import Database.Beam
  ( Beamable
  , C
  , Table(..)
  , TableEntity
  , Database
  , DatabaseSettings
  )
import qualified Types as T

data UserT f = User
  { userId    :: C f T.UserId
  , email     :: C f T.Email
  , name  :: C f Text
  , image       :: C f Text
  }
  deriving Generic
  deriving anyclass (Beamable)
