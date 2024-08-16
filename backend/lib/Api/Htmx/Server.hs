module Api.Htmx.Server where

import Api.Htmx.AuthHelper (getUser, getUserWithRedirect)
import Api.Htmx.Ginger (baseUrl)
import Api.Htmx.Home qualified as Home
import Api.Htmx.Login qualified as Login
import Api.Htmx.NotFound qualified as NotFound
import Api.Htmx.Studies qualified as Studies
import Api.Htmx.Study qualified as Study
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


scottyT
  :: MonadUnliftIO m
  => Port
  -> Scotty.ScottyT m ()
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
  caching <- liftIO $ initCaching $ CustomCaching $ \ f ->
                [ ("Cache-Control", "no-transform,public,max-age=300,s-maxage=900")
                , ("ETag", f.fm_etag)
                , ("Vary", "Accept-Encoding")
                ]
  env <- ask
  scottyT 3001 $ do
    Scotty.middleware (logMiddle env.envType)

    let options = Static.defaultOptions { cacheContainer = caching }
    let policy = Static.noDots <> Static.hasPrefix "static/"
    Scotty.middleware (unsafeStaticPolicyWithOptions options policy)

    Scotty.get "/login" $ do
      mUser <- getUser
      case mUser of
        Nothing -> Login.getLogin
        Just _ -> Scotty.redirect $ baseUrl <> "/studies"
    Scotty.post "/login" Login.login
    Scotty.get "/signout" Login.signout

    Scotty.get "/studies" $ do
      user <- getUserWithRedirect
      Studies.getStudies user

    Scotty.post "/study" $ do
      user <- getUserWithRedirect
      Study.createStudy user
    Scotty.get "/study/:documentId" $ do
      user <- getUserWithRedirect
      Study.getStudy user
    Scotty.delete "/study/:documentId" $ do
      user <- getUserWithRedirect
      Study.deleteStudy user

    Scotty.get "/" $ do
      mUser <- getUser
      case mUser of
        Nothing -> Home.getHome
        Just user -> Studies.getStudies user

    Scotty.notFound NotFound.getHome
