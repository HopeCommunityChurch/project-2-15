{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE UndecidableInstances #-}

module Fields.Email
  ( Email
  , UnvalidatedEmail(..)
  , mkEmail
  , mkEmail'
  , validateEmail
  , validateEmail'
  , emailText
  , unsafeMkEmail
  ) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Bifunctor as BF
import Data.CaseInsensitive qualified as CI
import Database.Beam (FromBackendRow)
import Database.Beam.Backend.SQL.SQL92 (HasSqlValueSyntax (..))
import Database.Beam.Postgres qualified as Pg
import Database.Beam.Query (HasSqlEqualityCheck)
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Orphans ()
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Instances ()
import Text.Email.Validate qualified as VEmail


newtype UnvalidatedEmail = UnvalidatedEmail Text
  deriving (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON, Arbitrary, IsString, Hashable)
  deriving anyclass (ToSchema)

instance Wrapped UnvalidatedEmail


newtype Email = Email (CI.CI Text)
  deriving (Show, Eq, Generic, Read)
  deriving newtype (ToJSON, Arbitrary, IsString, Hashable, Ord)
  deriving anyclass (ToSchema)

instance Wrapped Email


instance FromField Email where
  fromField f mdata = do
    x <- fromField f mdata
    pure $ Email x

instance FromBackendRow Pg.Postgres Email
instance HasSqlEqualityCheck Pg.Postgres Email
instance HasSqlValueSyntax be (CI.CI Text) => HasSqlValueSyntax be Email where
  sqlValueSyntax (Email b) = sqlValueSyntax b

instance FromJSON Email where
  parseJSON v@(Aeson.String str) =
    case mkEmail str of
      Right email   -> pure email
      Left errorStr -> Aeson.typeMismatch (toString errorStr) v
  parseJSON v = Aeson.typeMismatch "not a string" v



emailText :: Email -> Text
emailText (Email email) = CI.original email


-- | Make an email given some text
-- >> mkEmail "jonny@example.com"
-- Right (Email "jonny@example.com")
mkEmail :: Text -> Either Text Email
mkEmail = validateEmail . UnvalidatedEmail

mkEmail' :: Text -> Maybe Email
mkEmail' = preview _Right . validateEmail . UnvalidatedEmail

convert :: VEmail.EmailAddress -> Email
convert = Email . CI.mk . decodeUtf8 . VEmail.toByteString


validateEmail :: UnvalidatedEmail -> Either Text Email
validateEmail (UnvalidatedEmail unVEmail) =
  BF.bimap toText convert $ VEmail.validate (encodeUtf8 unVEmail)

validateEmail' :: UnvalidatedEmail -> Maybe Email
validateEmail' =  rightToMaybe . validateEmail


unsafeMkEmail :: Text -> Email
unsafeMkEmail = Email . CI.mk

