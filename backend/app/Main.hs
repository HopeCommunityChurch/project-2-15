module Main where

import Prelude hiding (get)
import Web.Scotty
import qualified Data.Aeson as Aeson

data DbInfo = MkDbInfo
  { host :: String
  , port :: Word16
  , username :: String
  , password :: String
  , database :: String
  }
  deriving (Generic)
  deriving anyclass (FromJSON)


data SecretsFile = MkSecretsFile
  { db :: DbInfo
  }
  deriving (Generic)
  deriving anyclass (FromJSON)


main :: IO ()
main = do
  envResult <- Aeson.eitherDecodeFileStrict "/var/run/keys/secrets"
  case envResult of
    Left err -> error (toText err)
    Right (r :: SecretsFile) ->
      putStrLn "read the secrets file"
  scotty 3000 $ do
    get "/:word" $ do
        beam <- param "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
    get "/" $ do
        html "<h1>Scotty, me up!</h1>"
