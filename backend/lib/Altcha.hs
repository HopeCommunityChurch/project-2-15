module Altcha where


import Crypto.Hash qualified as Hash
import Crypto.MAC.HMAC qualified as HMAC
import Crypto.Random qualified as R
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteArray.Encoding qualified as BAE
import Data.ByteArray.Parse qualified as Pars
import Data.ByteString.Builder (
  byteStringHex,
  toLazyByteString,
  word32HexFixed,
 )
import Foreign.Storable (Storable, sizeOf)
import GHC.Records (HasField)


type HasAltchaKey env = HasField "altchaKey" env ByteString


data Challenge = MkChallenge
  { algorithm :: Text
  , challenge :: Text
  , maxnumber :: Maybe Int32
  , salt :: Text
  , signature :: Text
  }
  deriving (Show, Generic)
  deriving anyclass (ToJSON, ToSchema)


getRandomNumber :: forall a m . (R.MonadRandom m, Storable a) => m a
getRandomNumber = do
  randomBytes :: ByteString <- R.getRandomBytes (sizeOf (undefined :: a))
  let result = Pars.parse Pars.takeStorable randomBytes
  case result of
    Pars.ParseOK _ r -> pure r
    _ -> error "somehow parsed wrong"


getRandomNum :: R.MonadRandom m => Integer -> m Word32
getRandomNum maxnum = do
  num :: Word32 <- getRandomNumber
  pure $ fromIntegral ((fromIntegral num :: Integer) * maxnum `div` fromIntegral (maxBound @Word32))


createChallenge :: R.MonadRandom m => ByteString -> Integer -> Bool -> m Challenge
createChallenge key maxnumber includeMax = do
  (saltRaw :: ByteString) <- R.getRandomBytes 10
  let (salt :: ByteString) = BAE.convertToBase BAE.Base64URLUnpadded saltRaw
  secretNum <- getRandomNum maxnumber
  let (challengeSHA :: Hash.Digest Hash.SHA256) = Hash.hash (salt <> show secretNum)
  let (challenge :: ByteString) = BAE.convertToBase BAE.Base16 challengeSHA

  let (signatureHMAC :: HMAC.HMAC Hash.SHA256) = HMAC.hmac key challenge
  let (signature :: ByteString) = BAE.convertToBase BAE.Base16 signatureHMAC

  pure $ MkChallenge
            "SHA-256"
            (decodeUtf8 challenge)
            (if includeMax then Just (fromIntegral maxnumber) else Nothing)
            (decodeUtf8 salt)
            (decodeUtf8 signature)


data ChallengeResp = MkChallengeResp
  { algorithm :: Text
  , challenge :: Text
  , number :: Word32
  , salt :: Text
  , signature :: Text
  }
  deriving (Show, Generic)
  deriving anyclass (FromJSON)


verifyChallenge
  :: ByteString
  -> ByteString -- the response from the form
  -> Bool
verifyChallenge key response =
  case BAE.convertFromBase BAE.Base64 response of
    Left err -> False
    Right respBS ->
      case Aeson.decode (toLazy respBS) of
        Nothing -> False
        Just (resp :: ChallengeResp) ->
          let (challengeSHA :: Hash.Digest Hash.SHA256) = Hash.hash (encodeUtf8 resp.salt <> (show resp.number :: ByteString))
              (challenge :: ByteString) = BAE.convertToBase BAE.Base16 challengeSHA

              (signatureHMAC :: HMAC.HMAC Hash.SHA256) = HMAC.hmac key (encodeUtf8 resp.challenge :: ByteString)
              (signature :: ByteString) = BAE.convertToBase BAE.Base16 signatureHMAC

           in signature == encodeUtf8 resp.signature
              && challenge == encodeUtf8 resp.challenge
              && resp.algorithm == "SHA-256"
