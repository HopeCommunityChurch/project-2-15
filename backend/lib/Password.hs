{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StrictData                #-}
{-# LANGUAGE UndecidableInstances      #-}
-- | Types for dealing with passwords in a safe way.

module Password
  ( Password
  , passwordFromText
  , comparePassword
  , PasswordHash(..)
  , NewPassword
  , getHash
  , newPassword
  ) where

import qualified Crypto.KDF.BCrypt                    as BCrypt
import           Crypto.Random.Types                  (MonadRandom)
import           Data.Aeson
    ( FromJSON(..)
    , ToJSON(..)
    , Value(String)
    )
import           Data.Aeson.Types                     (typeMismatch)
import           Data.OpenApi
    ( NamedSchema(..)
    , ToSchema(..)
    , toSchema
    )
import           Database.Beam
    ( FromBackendRow
    )
import           Database.Beam.Backend.SQL.SQL92      (HasSqlValueSyntax(..))
import qualified Database.Beam.Postgres               as Pg
import           Database.PostgreSQL.Simple.FromField (FromField(..))
import           Test.QuickCheck.Arbitrary            (Arbitrary(..))
import           Test.QuickCheck.Instances            ()
import           Text.Show                            (Show(..))

import           Control.Lens                         (iso)
import Web.Scotty.Trans (Parsable(..))

-- $setup
-- The code examples in this module require GHC's `OverloadedStrings`
-- extension:
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Aeson (decode)



-- | A data type of passing around password in a safe way.
--
-- Decoding a password from a JSON string
--
-- >>> decode "\"password\"" :: Maybe Password
-- Just **********
--
newtype Password = Password ( PasswordHash -> Bool )

runPassword :: Password -> PasswordHash -> Bool
runPassword (Password a) = a

passwordFromText :: Text -> Password
passwordFromText str =
    Password
    $ BCrypt.validatePassword (encodeUtf8 @Text @ByteString str) . (encodeUtf8 @Text @ByteString) . unPasswordHash

-- | Compare a password and a password hash
--
-- Comparing a password hash with a password decoded from a JSON string:
--
-- >>> let hash = PasswordHash "$2a$08$BQO/t0G9QAXJuG8z57Imde3b93iJlDXqaxDEHBrM6zMUNZuGPS7vm"
-- >>> let maybePassword = decode "\"password\"" :: Maybe Password
-- >>> fmap (\password -> comparePassword password hash) maybePassword
-- Just True
comparePassword :: Password -> PasswordHash -> Bool
comparePassword = runPassword

instance Show Password where
  show _ = "**********"

instance Parsable Password where
  parseParam txt =
    pure $ passwordFromText (toStrict txt)

instance FromJSON Password where
  parseJSON (String str) =
    pure $ passwordFromText str
  parseJSON x = typeMismatch "passwords are strings" x

-- | Only here to help documentation. Doesn't actually return the plain text
-- password
instance ToJSON Password where
  toJSON _ = String "**********"

instance ToSchema Password where
  declareNamedSchema _ =
    declareNamedSchema (Proxy @Text)


-- | Password hash from the database
newtype PasswordHash = PasswordHash Text


unPasswordHash :: PasswordHash -> Text
unPasswordHash (PasswordHash a) = a

instance Wrapped PasswordHash where
  type Unwrapped PasswordHash = Text
  _Wrapped' = iso unPasswordHash PasswordHash
instance FromBackendRow Pg.Postgres PasswordHash
instance FromField PasswordHash where
  fromField f mdata = do
      x <- fromField f mdata
      pure $ PasswordHash x
instance HasSqlValueSyntax be Text => HasSqlValueSyntax be PasswordHash where
  sqlValueSyntax = sqlValueSyntax . unPasswordHash

hashPassword :: (MonadRandom m) => Text -> m PasswordHash
hashPassword str =
  fmap (PasswordHash . (decodeUtf8 @Text @ByteString)) (BCrypt.hashPassword 10 bytestring)
    where
      bytestring :: ByteString
      bytestring = encodeUtf8 str


-- | New Passwords are meant to be hashed before being used
newtype NewPassword =
  NewPassword ( forall m. (MonadRandom m) => m PasswordHash )

runNewPassword :: MonadRandom m => NewPassword -> m PasswordHash
runNewPassword (NewPassword a) = a

getHash ::
     (MonadRandom m) => NewPassword -> m PasswordHash
getHash = runNewPassword

newPassword :: Text -> NewPassword
newPassword txt = NewPassword (hashPassword txt)

instance Show NewPassword where
  show _ = "**********"

instance FromJSON NewPassword where
  parseJSON (String str) =
    pure $ NewPassword $ hashPassword str
  parseJSON x =  typeMismatch "passwords are strings" x

-- | Only here to help documentation. Doesn't actually return the plain text
-- password
instance ToJSON NewPassword where
  toJSON _ = String "**********"

instance Arbitrary NewPassword where
  arbitrary = do
    p <- arbitrary
    pure (NewPassword (hashPassword p))

instance ToSchema NewPassword where
  declareNamedSchema _ =
    let textSchema = toSchema (Proxy :: Proxy Text)
    in pure $ NamedSchema (Just "NewPassword") textSchema


-- | Token used to reset passwords
-- newtype PasswordResetToken = PasswordResetToken Text
--   deriving(Show, Eq, Generic)
--   deriving newtype (FromJSON, ToJSON)

-- instance Arbitrary PasswordResetToken where
--   arbitrary = PasswordResetToken <$> arbitrary

-- instance ToSchema PasswordResetToken
-- instance Wrapped PasswordResetToken
-- instance HasSqlValueSyntax be Text => HasSqlValueSyntax be PasswordResetToken where
--   sqlValueSyntax (PasswordResetToken rt) = sqlValueSyntax rt
-- instance HasSqlEqualityCheck Pg.Postgres PasswordResetToken
-- instance FromBackendRow Pg.Postgres PasswordResetToken
-- instance FromField PasswordResetToken where
--   fromField f mdata = do
--       x <- fromField f mdata
--       pure $ PasswordResetToken x


-- getToken :: PasswordResetToken -> Text
-- getToken (PasswordResetToken token) = token


-- -- | Generate a random token. Probably need to make this better/more secure.
-- genToken :: IO PasswordResetToken
-- genToken =
--   PasswordResetToken
--   . toText
--   . take 60
--   . Random.randomRs ('a', 'z')
--   <$> Random.newStdGen

