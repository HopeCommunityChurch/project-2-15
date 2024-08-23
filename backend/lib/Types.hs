{-# LANGUAGE UndecidableInstances #-}

module Types (
  NewType (..),
  UserId,
  ComputerId,
  Email,
  ChurchId,
  GroupStudyId,
  StudyTemplateId,
  DocId,
  CookieToken,
  genCookieToken,
  PasswordResetToken,
  mkPasswordResetToken,
  genPasswordResetToken,

  ShareToken,
  genShareToken,

  genToken,

  Feature(..),
  featureIso,
  featureDescription,
  allFeatures
) where

import Crypto.Random (
  MonadRandom (getRandomBytes),
 )
import Fields.Email (Email)
import Data.OpenApi (ToParamSchema)
import Data.UUID (UUID)
import Database.Beam (FromBackendRow)
import Database.Beam.Backend.SQL (BeamSqlBackendIsString)
import Database.Beam.Backend.SQL.SQL92 (HasSqlValueSyntax (..))
import Database.Beam.Postgres qualified as Pg
import Database.Beam.Query (HasSqlEqualityCheck)
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Orphans ()
import Data.ByteString qualified as BS
import Data.List ((!!))
import Data.Text qualified as T
import Servant.API (FromHttpApiData)
import Web.Scotty.Trans (Parsable)
import Control.Lens (iso, review)

newtype NewType p a = MkNewType a
  deriving (Generic)
  deriving newtype
    ( Show
    , Read
    , Parsable
    , Hashable
    , Eq
    , Ord
    , ToJSON
    , FromJSON
    , ToParamSchema
    , ToSchema
    , FromField
    , ToField
    , FromHttpApiData
    )

instance {-# OVERLAPPABLE #-} HasSqlEqualityCheck be a
  => HasSqlEqualityCheck be (NewType p a)

instance {-# OVERLAPPABLE #-} HasSqlValueSyntax be a
  => HasSqlValueSyntax be (NewType p a) where
  sqlValueSyntax (MkNewType a) = sqlValueSyntax a

instance BeamSqlBackendIsString be a
  => BeamSqlBackendIsString be (NewType p a)


instance forall a (p :: Type) . (Typeable a, FromBackendRow Pg.Postgres a, FromField a, Typeable p)
  => FromBackendRow Pg.Postgres (NewType p a)

instance Wrapped (NewType p a)


data UserId'
type UserId = NewType UserId' UUID


data ComputerId'
type ComputerId = NewType ComputerId' Text


data ChurchId'
type ChurchId = NewType ChurchId' UUID


data StudyTemplateId'
type StudyTemplateId = NewType StudyTemplateId' UUID


data GroupStudyId'
type GroupStudyId = NewType GroupStudyId' UUID


data DocId'
type DocId = NewType DocId' UUID


data CookieToken'
type CookieToken = NewType CookieToken' Text


data PasswordResetToken'
type PasswordResetToken = NewType PasswordResetToken' Text

mkPasswordResetToken :: Text -> PasswordResetToken
mkPasswordResetToken = MkNewType


data ShareToken'
type ShareToken = NewType ShareToken' Text


validChars :: [Char]
validChars = ['0'..'9'] <> ['a'..'z'] <> ['A'..'Z'] <> ['_']

convert :: Word8 -> Text
convert int =
  let (d,m) = int `divMod` fromIntegral (length validChars)
   in T.singleton (validChars !! fromIntegral m)
       <> if d > 0 then convert d else mempty


tokenToText :: ByteString -> Text
tokenToText =
  BS.foldr (\ w r -> r <> convert w) mempty


genToken :: (MonadIO m) => Int -> m Text
genToken i = do
  bytes <- liftIO $ getRandomBytes i
  pure $ tokenToText bytes


genCookieToken :: (MonadIO m) => m CookieToken
genCookieToken = MkNewType <$> genToken 32


genPasswordResetToken :: (MonadIO m) => m PasswordResetToken
genPasswordResetToken = MkNewType <$> genToken 32


genShareToken :: (MonadIO m) => m ShareToken
genShareToken = MkNewType <$> genToken 32


data Feature
  = GroupStudy
  | Unknown
  deriving (Generic, Show, Read, Enum, Bounded, Eq)
  deriving (ToJSON, FromJSON, ToSchema)

featureIso :: Iso' Text Feature
featureIso = iso (fromMaybe Unknown . readMaybe . toString) show

instance FromField Feature where
  fromField f mdata = do
    x <- fromField f mdata
    let r = view featureIso x
    pure r

instance HasSqlEqualityCheck Pg.Postgres Feature

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be Feature where
  sqlValueSyntax = sqlValueSyntax . review featureIso

instance FromBackendRow Pg.Postgres Feature


featureDescription :: Feature -> Text
featureDescription GroupStudy =
  "Study together in groups!"
featureDescription Unknown =
  "Features we remove will be caught be this one."

allFeatures :: [Feature]
allFeatures = [minBound..maxBound]
