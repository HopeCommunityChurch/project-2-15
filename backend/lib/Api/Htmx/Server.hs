module Api.Htmx.Server where

import Api.Htmx.AuthHelper (getUser, getUserWithRedirect)
import Api.Htmx.Ginger (baseUrl)
import Api.Htmx.Home qualified as Home
import Api.Htmx.Login qualified as Login
import Api.Htmx.NotFound qualified as NotFound
import Api.Htmx.PasswordReset qualified as PasswordReset
import Api.Htmx.Profile qualified as Profile
import Api.Htmx.Signup qualified as Signup
import Api.Htmx.Studies qualified as Studies
import Api.Htmx.Study qualified as Study
import Data.List qualified as L
import DbHelper qualified as Db
import EnvFields (EnvType (..), HasUrl)
import Mail qualified
import Network.HTTP.Types.Status (status302, status500)
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
import Web.Scotty.Internal.Types qualified as Scotty
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
     , Mail.HasSmtp env
     , HasUrl env
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


    Scotty.defaultHandler $ Scotty.Handler $ \ (SomeException e) -> do
      logErrorSH e
      Scotty.status status500
      Scotty.text "Something went wrong"

    Scotty.defaultHandler $ Scotty.Handler $ \ (Scotty.StatusError status txt) -> do
      Scotty.status status
      Scotty.text txt

    Scotty.get "/login" $ do
      mUser <- getUser
      case mUser of
        Nothing -> Login.getLogin
        Just _ -> do
          mRedirect <- L.lookup "redirect" <$> Scotty.formParams
          let url = case mRedirect of
                      Just re ->
                        if re == ""
                          then baseUrl <> "/studies"
                          else re
                      Nothing  -> baseUrl <> "/studies"
          Scotty.setHeader "Location" url
          Scotty.raiseStatus status302 "redirect"
    Scotty.post "/login" Login.login
    Scotty.get "/signout" Login.signout

    Scotty.get "/signup" $ do
      mUser <- getUser
      case mUser of
        Nothing -> Signup.getSignup
        Just _ -> Scotty.redirect $ baseUrl <> "/studies"
    Scotty.post "/signup" Signup.signup

    Scotty.get "/resetpassword" $ do
      PasswordReset.getPasswordReset
    Scotty.post "/reset_email"
      PasswordReset.resetEmail
    Scotty.get "/reset_token" $ do
      PasswordReset.getResetToken
    Scotty.post "/reset_token" $ do
      PasswordReset.postResetToken


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

    Scotty.get "/group_study/:documentId" $ do
      user <- getUserWithRedirect
      Study.getGroupStudy user
    Scotty.post "/group_study" $ do
      user <- getUserWithRedirect
      Study.createGroupStudy user
    Scotty.delete "/group_study/share/:shareToken" $ do
      user <- getUserWithRedirect
      Study.rejectShare user
    Scotty.post "/group_study/share/:shareToken" $ do
      user <- getUserWithRedirect
      Study.acceptShare user
    Scotty.delete "/group_study/:groupId/share/:shareToken" $ do
      -- In theory I could check to make sure that the token is actually part of
      -- this group and that the user is an owner, but you can already reject a
      -- token by tokenId
      user <- getUserWithRedirect
      Study.ownerShareDelete user

    Scotty.get "/profile" $ do
      user <- getUserWithRedirect
      Profile.getProfile user
    Scotty.put "/profile" $ do
      user <- getUserWithRedirect
      Profile.putProfile user
    Scotty.post "/profile/feature" $ do
      user <- getUserWithRedirect
      Profile.postFeatures user

    Scotty.get "/" $ do
      mUser <- getUser
      case mUser of
        Nothing -> Home.getHome
        Just user -> Studies.getStudies user

    Scotty.notFound NotFound.getNotFound
