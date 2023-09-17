module Types (
  NewType (..),
  UserId,
  Email,
  ChurchId,
  GroupStudyId,
  StudyTemplateId,
  DocId,
  CookieToken,
  genCookieToken,
  genToken,
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

newtype NewType p a = MkNewType a
  deriving (Generic)
  deriving newtype
    ( Show
    , Read
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
