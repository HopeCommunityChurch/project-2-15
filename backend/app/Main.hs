module Main where

import Api qualified
import Data.Aeson qualified as Aeson
import EnvFields (EnvType (..))
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (
  logStdout,
  logStdoutDev,
 )
import Servant.Server (
  serveWithContext,
 )
import Prelude hiding (get)
import qualified DbHelper as Db
import Data.Generics.Product (HasField'(field'))

data DbInfo = MkDbInfo
  { host :: String
  , port :: Word16
  , username :: String
  , password :: String
  , database :: String
  }
  deriving (Generic)
  deriving anyclass (FromJSON)


dbToConnectInfo :: DbInfo -> Db.ConnectInfo
dbToConnectInfo MkDbInfo{host, port, username, password, database} =
  Db.ConnectInfo
    host
    port
    username
    password
    database


data SecretsFile = MkSecretsFile
  { db :: DbInfo
  , env :: EnvType
  }
  deriving (Generic)
  deriving anyclass (FromJSON)

logMiddle :: EnvType -> Wai.Middleware
logMiddle (Dev _) = logStdoutDev
logMiddle Prod = logStdout


data Env = MkEnv
  { envType :: EnvType
  , dbConn :: Db.DbConn
  }
  deriving (Generic)

instance Db.HasDbConn Env where
  dbConn = field' @"dbConn"

secretToEnv :: MonadIO m => SecretsFile -> m Env
secretToEnv MkSecretsFile{db, env} = do
  dbConn <- liftIO $ Db.createPool (dbToConnectInfo db)
  pure $ MkEnv env dbConn


main :: IO ()
main = do
  envResult <- Aeson.eitherDecodeFileStrict "/var/run/keys/secrets"
  case envResult of
    Left err -> error (toText err)
    Right (file :: SecretsFile) -> do
      env <- secretToEnv file
      putStrLn "read the secrets file"
      run 3000
        (logMiddle env.envType
           (serveWithContext
              (Proxy @Api.Api)
              Api.serverContext
              (Api.server env)))
