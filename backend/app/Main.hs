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
import Network.Wai.Middleware.Static qualified as Static
import Network.Wai.Middleware.Static (
  addBase,
  hasPrefix,
  CachingStrategy(..),
  cacheContainer,
  initCaching,
  defaultOptions,
  unsafeStaticPolicyWithOptions,
  noDots,
 )
import Network.Wai.Middleware.RequestLogger (
  logStdout,
  logStdoutDev,
 )
import Servant.Server (
  serveWithContext,
 )
import System.Environment qualified as Env
import Prelude hiding (get)
import UnliftIO.Concurrent (threadDelay)
import Api.Bible qualified
import Mail qualified
import Web.Scotty.Trans qualified as Scotty
import Network.Wai.Handler.Warp (Port)
import Api.Htmx.Login qualified as Login
import Api.Htmx.Home qualified as Home
import Api.Htmx.NotFound qualified as NotFound
import Data.List qualified as List


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
  }
  deriving (Generic)

instance Db.HasDbConn Env where
  dbConn = field' @"dbConn"

secretToEnv :: MonadIO m => SecretsFile -> m Env
secretToEnv MkSecretsFile{db, env, port, esvToken, smtp, url} = do
  dbConn <- liftIO $ Db.createPool (dbToConnectInfo db)
  let esvEnv = Api.Bible.MkESVEnv (encodeUtf8 esvToken)
  let smtp2 = Mail.MkSmtp smtp.host (fromIntegral smtp.port)
  subs <- WS.mkSubs
  pure $ MkEnv env port dbConn esvEnv smtp2 url subs


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
      when (env.envType /= Prod) $
        -- pure ()
        threadDelay (2*1000*1000)
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
        , runReaderT (runStdoutLoggingT scottyServer) env
        ]

scottyT
  :: MonadUnliftIO m
  => Port
  -> Scotty.ScottyT LText m ()
  -> m ()
scottyT port action =
  withRunInIO $ \ runInIO ->
    Scotty.scottyT port runInIO action


scottyServer
  :: ( MonadUnliftIO m
     , MonadLogger m
     , Db.MonadDb env m
     )
  => m ()
scottyServer = do
  caching <- liftIO $ initCaching PublicStaticCaching
  scottyT 3001 $ do
    Scotty.middleware logStdout
    let options = defaultOptions { cacheContainer = caching }
    let policy = Static.noDots <> Static.hasPrefix "/static/" <> Static.policy (Just . List.drop 1)
    Scotty.middleware (unsafeStaticPolicyWithOptions options policy)
    Scotty.get "/login" Login.getLogin
    Scotty.post "/login" Login.login
    Scotty.get "/" Home.getHome
    Scotty.notFound NotFound.getHome


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

