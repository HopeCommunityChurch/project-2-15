module Api.Htmx.Server where

import Altcha qualified
import Api.Bible (HasESVEnv, getVerses)
import Api.Htmx.AuthHelper (getUser, getUserWithRedirect)
import Api.Htmx.Ginger (baseUrl)
import Api.Htmx.GroupStudy qualified as GroupStudy
import Api.Htmx.Home qualified as Home
import Api.Htmx.BlogArticleHistoryOfOutlining qualified as BlogArticleHistoryOfOutlining
import Api.Htmx.Contribute qualified as Contribute
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
import Network.HTTP.Types.Status (status301, status302, status500)
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
import Data.Typeable (cast)


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


errorWrapper
  :: (MonadUnliftIO m, MonadLogger m)
  => Scotty.ActionT m ()
  -> Scotty.ActionT m ()
errorWrapper action = do
  result <- try action
  case result of
    Right r -> pure r
    Left (SomeException err) -> do
      logErrorSH err
      Scotty.status status500
      Scotty.text "Something went wrong"


scottyServer
  :: ( MonadUnliftIO m
     , MonadLogger m
     , Db.MonadDb env m
     , Altcha.HasAltchaKey env
     , Mail.HasSmtp env
     , HasUrl env
     , HasESVEnv env
     )
  => m ()
scottyServer = do
  caching <- liftIO $ initCaching $ CustomCaching $ \ f ->
                [ ("Cache-Control", "no-transform,private,max-age=300,s-maxage=900,no-cache")
                , ("ETag", f.fm_etag)
                , ("Vary", "Accept-Encoding")
                ]
  env <- ask
  scottyT 3001 $ do
    Scotty.middleware (logMiddle env.envType)

    let options = Static.defaultOptions { cacheContainer = caching }
    let policy = Static.noDots <> Static.hasPrefix "static/"
    Scotty.middleware (unsafeStaticPolicyWithOptions options policy)

    Scotty.defaultHandler $ Scotty.Handler $ \ (SomeException e) ->
      case cast e of
        Just (Scotty.StatusError status txt) -> do
          Scotty.status status
          Scotty.text (toLazy txt)
        Nothing ->
          case cast e of
            Just (_ :: Scotty.ActionError) ->
              pure ()
            Nothing -> do
              logErrorSH e
              Scotty.status status500
              Scotty.text "Something went wrong"

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
          Scotty.setHeader "Location" (toLazy url)
          Scotty.status status302
    Scotty.post "/login" Login.login
    Scotty.get "/signout" Login.signout

    Scotty.get "/signup" $ do
      mUser <- getUser
      case mUser of
        Nothing -> Signup.getSignup
        Just _ -> do
          logInfo "signed in redirecting"
          let url = baseUrl <> "/studies"
          Scotty.setHeader "Location" url
          Scotty.status status302
    Scotty.post "/signup" Signup.signup

    Scotty.get "/resetpassword" $ do
      PasswordReset.getPasswordReset
    Scotty.post "/reset_email" $ do
      result <- try PasswordReset.resetEmail
      case result of
        (Left (SomeException err)) -> do
          Scotty.status status500
          Scotty.text "Something went wrong"
          logErrorSH err
        Right r -> pure r
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
      GroupStudy.getGroupStudy user
    Scotty.post "/group_study" $ errorWrapper $ do
      user <- getUserWithRedirect
      GroupStudy.createGroupStudy user
    Scotty.delete "/group_study/share/:shareToken" $ do
      user <- getUserWithRedirect
      GroupStudy.rejectShare user
    Scotty.post "/group_study/share/:shareToken" $ do
      user <- getUserWithRedirect
      GroupStudy.acceptShare user
    Scotty.delete "/group_study/:groupId/share/:shareToken" $ do
      user <- getUserWithRedirect
      GroupStudy.ownerShareDelete user
    Scotty.post "/group_study/:groupId/share/:shareToken/resend" $ do
      user <- getUserWithRedirect
      GroupStudy.resendInvite user
    Scotty.delete "/group_study/:groupId/member/:docId" $ do
      user <- getUserWithRedirect
      GroupStudy.removeMemberDoc user
    Scotty.post "/group_study/:groupId/member/:docId/ownership" $ do
      user <- getUserWithRedirect
      GroupStudy.ownershipMemberDoc user
    Scotty.post "/group_study/invite/add" $ do
      user <- getUserWithRedirect
      GroupStudy.postInvite user
    Scotty.post "/group_study/:groupId/name" $ do
      user <- getUserWithRedirect
      GroupStudy.nameUpdate user

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

    Scotty.get "/home" $ do
      mUser <- getUser
      case mUser of
        Nothing -> Home.getHome
        Just user -> Studies.getStudies user

    Scotty.get "/blog/the-rise-of-outline-method" $ do
      BlogArticleHistoryOfOutlining.getBlogArticleHistoryOfOutlining

    Scotty.get "/contribute" $ do
      Contribute.getContribute

    Scotty.get "/api/bible/esv" $ do
      user <- getUserWithRedirect
      q <- Scotty.queryParam "q"
      esvResponse <- lift $ getVerses user q
      Scotty.json esvResponse

    Scotty.get (Scotty.regex "^/app/(.*)$") $ do
      url <- Scotty.captureParam "1"
      Scotty.setHeader "Location" ("/" <> url)
      Scotty.status status301

    Scotty.notFound NotFound.getNotFound
