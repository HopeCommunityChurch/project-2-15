module Api.Htmx.Server where

import Api.Htmx.Home qualified as Home
import Api.Htmx.Login qualified as Login
import Api.Htmx.Studies qualified as Studies
import Api.Htmx.NotFound qualified as NotFound
import Data.List qualified as List
import DbHelper qualified as Db
import EnvFields (EnvType (..))
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Middleware.RequestLogger (
  logStdout,
  logStdoutDev,
 )
import Network.Wai.Middleware.Static (
  CachingStrategy (..),
  cacheContainer,
  initCaching,
  unsafeStaticPolicyWithOptions,
 )
import Network.Wai.Middleware.Static qualified as Static
import Web.Scotty.Trans qualified as Scotty
import Api.Htmx.AuthHelper (AuthUser, getUserWithRedirect)


scottyT
  :: MonadUnliftIO m
  => Port
  -> Scotty.ScottyT LText m ()
  -> m ()
scottyT port action =
  withRunInIO $ \ runInIO ->
    Scotty.scottyT port runInIO action


logMiddle :: EnvType -> Wai.Middleware
logMiddle (Dev _) = logStdoutDev
logMiddle Prod = logStdout


scottyServer
  :: ( MonadUnliftIO m
     , MonadLogger m
     , Db.MonadDb env m
     )
  => m ()
scottyServer = do
  caching <- liftIO $ initCaching PublicStaticCaching
  env <- ask
  scottyT 3001 $ do
    Scotty.middleware (logMiddle env.envType)
    let options = Static.defaultOptions { cacheContainer = caching }
    let policy = Static.noDots <> Static.hasPrefix "/static/" <> Static.policy (Just . List.drop 1)
    Scotty.middleware (unsafeStaticPolicyWithOptions options policy)
    Scotty.get "/login" Login.getLogin
    Scotty.post "/login" Login.login
    Scotty.get "/studies" $ do
      user <- getUserWithRedirect
      Studies.getStudies user
    Scotty.get "/" Home.getHome
    Scotty.notFound NotFound.getHome
