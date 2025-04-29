module Main where

import Api.Websocket qualified as WS
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
import System.Environment qualified as Env
import Prelude hiding (get)
import Api.Bible qualified
import Mail qualified
import Api.Htmx.Server qualified as HXServer


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

data Smtp = MkSmtp
  { host :: String
  , port :: Int
  , username :: Maybe Text
  , password :: Maybe Text
  }
  deriving (Generic)
  deriving anyclass (FromJSON)

data SecretsFile = MkSecretsFile
  { db :: DbInfo
  , port :: Maybe Int
  , env :: EnvType
  , esvToken :: Text
  , smtp :: Smtp
  , url :: Text
  , altchaKey :: Text
  }
  deriving (Generic)
  deriving anyclass (FromJSON)

logMiddle :: EnvType -> Wai.Middleware
logMiddle (Dev _) = logStdoutDev
logMiddle Prod = logStdout


data Env = MkEnv
  { envType :: EnvType
  , port :: Maybe Int
  , dbConn :: Db.DbConn
  , esvToken :: Api.Bible.ESVEnv
  , smtp :: Mail.Smtp
  , url :: Text
  , subs :: WS.Subs
  , altchaKey :: ByteString
  }
  deriving (Generic)

instance Db.HasDbConn Env where
  dbConn = field' @"dbConn"

secretToEnv :: (MonadIO m) => SecretsFile -> m Env
secretToEnv MkSecretsFile{db, env, port, esvToken, smtp, url, altchaKey} = do
  dbConn <- liftIO $ Db.createPool (dbToConnectInfo db)
  let esvEnv = Api.Bible.MkESVEnv (encodeUtf8 esvToken)
  let auth = case (smtp.username, smtp.password) of
               (Just username, Just password) ->
                 Just (Mail.MkAuth username password)
               _ ->
                 Nothing
  let ssl = case env of
              Dev "local" -> Mail.NoSSL
              _ -> Mail.NoSSL
              -- _ -> Mail.StartTsl
  let smtp2 = Mail.MkSmtp smtp.host (fromIntegral smtp.port) auth ssl
  putStrLn smtp2.host
  subs <- WS.mkSubs
  pure $ MkEnv env port dbConn esvEnv smtp2 url subs (encodeUtf8 altchaKey)


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  secretsFile <- liftIO $ Env.lookupEnv "SECRETS_FILE"
  envResult <- Aeson.eitherDecodeFileStrict (fromMaybe "./local-secrets.json" secretsFile)
  case envResult of
    Left err -> error (toText err)
    Right (file :: SecretsFile) -> do
      putStrLn "read the secrets file"
      env <- secretToEnv file
      putStrLn "running migration"
      migration (dbToConnectInfo file.db)
      putStrLn "starting on port 3000"
      mapConcurrently_ identity
        [ run (fromMaybe 3000 env.port)
            (logMiddle env.envType
               (serveWithContext
                  (Proxy @Api.Api)
                  (Api.serverContext env)
                  (Api.server env)))
        , runReaderT (runStdoutLoggingT HXServer.scottyServer) env
        ]



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
  envPath <- liftIO $ Env.lookupEnv "MIGRATION_PATH"
  conn <- liftIO $ PgS.connect dbInfo
  result <- liftIO $ Mig.runMigrations conn migrationOptions
    [ Mig.MigrationInitialization
    , Mig.MigrationDirectory (fromMaybe "./migrations/" envPath)
    ]
  print result
  liftIO $ PgS.close conn

