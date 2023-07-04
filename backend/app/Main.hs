module Main where

import Api qualified
import Data.Aeson qualified as Aeson
import Data.Generics.Product (HasField' (field'))
import Data.Text.IO qualified as T (hPutStrLn, putStrLn)
import Database.PostgreSQL.Simple qualified as PgS
import Database.PostgreSQL.Simple.Migration qualified as Mig
import DbHelper qualified as Db
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
import System.Environment qualified as Env

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
      migration (dbToConnectInfo file.db)
      env <- secretToEnv file
      putStrLn "read the secrets file"
      run 3000
        (logMiddle env.envType
           (serveWithContext
              (Proxy @Api.Api)
              (Api.serverContext env)
              (Api.server env)))


migrationOptions :: Mig.MigrationOptions
migrationOptions =
  Mig.MigrationOptions
    Mig.Verbose
    "schema_migrations"
    (either (T.hPutStrLn stderr) T.putStrLn)
    Mig.TransactionPerRun



migration
  :: MonadIO m
  => Db.ConnectInfo
  -> m ()
migration dbInfo = do
  envPath <- liftIO $ Env.getEnv "MIGRATION_PATH"
  conn <- liftIO $ PgS.connect dbInfo
  result <- liftIO $ Mig.runMigrations conn migrationOptions
    [ Mig.MigrationInitialization
    , Mig.MigrationDirectory envPath
    ]
  print result
  liftIO $ PgS.close conn

