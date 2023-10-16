{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api.Auth (
  Api,
  server,
  authCookie,
  AuthUser (..),
  authCookieToken
) where

import Api.Errors qualified as Errs
import Data.List qualified as List
import Data.OpenApi qualified as OpenApi
import Data.Time.Lens qualified as TL
import Database qualified as Db
import Database.Beam (
  all_,
  guard_,
  insert,
  insertValues,
  runInsert,
  runSelectReturningOne,
  update,
  runUpdate,
  select,
  val_,
  (==.),
  (<-.),
  (>=.),
 )
import DbHelper (HasDbConn, MonadDb, runBeam, withTransaction)
import Entity qualified as E
import Entity.User qualified as User
import Entity.AuthUser (AuthUser(..))
import EnvFields (HasEnvType, EnvType(..), HasUrl)
import Network.Wai (
  Request,
  requestHeaders,
 )
import Password (Password, PasswordHash, comparePassword)
import Servant
import Servant.Server.Experimental.Auth (
  AuthHandler,
  AuthServerData,
  mkAuthHandler,
 )
import SwaggerHelpers (AuthDescription (..))
import Types qualified as T
import Web.Cookie qualified as Cookie
import Mail qualified
import Emails.Welcome qualified
import Emails.PasswordReset qualified


instance AuthDescription "cookie" where
  securityName = "cookie"
  securityScheme = OpenApi.SecurityScheme type_ (Just desc)
    where
      type_ = OpenApi.SecuritySchemeApiKey
                (OpenApi.ApiKeyParams "p215-auth" OpenApi.ApiKeyCookie)
      desc  = "p215-auth cookie"

type instance AuthServerData (AuthProtect "cookie") = AuthUser

instance AuthDescription "cookie-with-token" where
  securityName = "cookie"
  securityScheme = OpenApi.SecurityScheme type_ (Just desc)
    where
      type_ = OpenApi.SecuritySchemeApiKey
                (OpenApi.ApiKeyParams "p215-auth" OpenApi.ApiKeyCookie)
      desc  = "p215-auth cookie"

type instance AuthServerData (AuthProtect "cookie-with-token") = (AuthUser, T.CookieToken)


authCookie
  :: HasDbConn env
  => HasEnvType env
  => env
  -> AuthHandler Request AuthUser
authCookie env =
  mkAuthHandler $ \ req -> do
    case List.lookup "Cookie" (requestHeaders req) of
      Nothing -> throwError (err401 {errBody = "No Cookies"})
      Just bsCookies -> do
        case List.lookup "p215-auth" (Cookie.parseCookies bsCookies) of
          Nothing -> do
            throwError (err401 {errBody = "Missing Needed Cookie"})
          Just bsToken -> do
            let cookie = T.MkNewType (decodeUtf8 bsToken)
            mResult <- liftIO $ runStdoutLoggingT (runReaderT (lookupSession cookie) env)
            case mResult of
              Nothing -> do
                throwError (err401 {errBody = "Missing Needed Cookie"})
              Just result -> pure result


authCookieToken
  :: HasDbConn env
  => HasEnvType env
  => env
  -> AuthHandler Request (AuthUser, T.CookieToken)
authCookieToken env =
  mkAuthHandler $ \ req -> do
    case List.lookup "Cookie" (requestHeaders req) of
      Nothing -> throwError (err401 {errBody = "No Cookies"})
      Just bsCookies -> do
        case List.lookup "p215-auth" (Cookie.parseCookies bsCookies) of
          Nothing -> do
            throwError (err401 {errBody = "Missing Needed Cookie"})
          Just bsToken -> do
            let cookie = T.MkNewType (decodeUtf8 bsToken)
            mResult <- liftIO $ runStdoutLoggingT (runReaderT (lookupSession cookie) env)
            case mResult of
              Nothing -> do
                throwError (err401 {errBody = "Missing Needed Cookie"})
              Just result -> pure (result, cookie)


lookupSession
  :: MonadDb env m
  => T.CookieToken
  -> m (Maybe AuthUser)
lookupSession token = withTransaction $ do
  now <- getCurrentTime
  fmap (fmap E.toEntity)
    $ runBeam
    $ runSelectReturningOne
    $ select
    $ do
      session <- all_ Db.db.userSession
      guard_ $ session.expires >=. val_ now
      guard_ $ session.token ==. val_ token
      E.queryEntityBy @AuthUser Nothing session.userId



data PassLogin = MkPassLogin
  { email :: T.Email
  , password :: Password
  }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON, ToSchema)


type CookieHeader = Headers '[ Header "Set-Cookie" Cookie.SetCookie]


getPasswordHash
  :: MonadDb env m
  => T.Email
  -> m (Maybe (T.UserId, PasswordHash))
getPasswordHash email =
  runBeam
  $ runSelectReturningOne
  $ select
  $ do
    user <- all_ Db.db.user
    guard_ $ user.email ==. val_ email
    p <- all_ Db.db.userPassword
    guard_ $ p.userId ==. user.userId
    pure (p.userId, p.password)


mkCookie
  :: (MonadDb env m)
  => T.UserId
  -> m (T.CookieToken, UTCTime)
mkCookie userId = do
  token <- T.genCookieToken
  now <- getCurrentTime
  let expiresAt = now & (TL.flexDT . TL.days) +~ 10
  runBeam
    $ runInsert
    $ insert Db.db.userSession
    $ insertValues
      [ Db.MkUserSessionT
          userId
          token
          expiresAt
          now
      ]
  pure (token, expiresAt)


invalidateCookie
  :: (MonadDb env m)
  => T.CookieToken
  -> m ()
invalidateCookie cookie = do
  now <- getCurrentTime
  runBeam
    $ runUpdate
    $ update Db.db.userSession
      (\ r -> r.created <-. val_ now)
      (\ r -> r.token ==. val_ cookie)



setCookie
  :: MonadDb env m
  => T.UserId
  -> m (CookieHeader ())
setCookie userId = do
  envType <- asks (.envType)
  let shouldBeSeure =
        case envType of
          Dev "local" -> False
          _ -> True
  (token, expiresAt) <- mkCookie userId
  let setCookie' = Cookie.defaultSetCookie
                  { Cookie.setCookieName = "p215-auth"
                  , Cookie.setCookieValue = encodeUtf8 (unwrap token)
                  , Cookie.setCookieExpires = Just expiresAt
                  , Cookie.setCookieHttpOnly = True
                  , Cookie.setCookieSecure = shouldBeSeure
                  , Cookie.setCookieSameSite = Just Cookie.sameSiteStrict
                  , Cookie.setCookiePath = Just "/"
                  }
  pure $ addHeader setCookie' ()


setCookieDelete
  :: MonadDb env m
  => m (CookieHeader ())
setCookieDelete = do
  envType <- asks (.envType)
  now <- getCurrentTime
  let shouldBeSeure =
        case envType of
          Dev "local" -> False
          _ -> True
  let setCookie' = Cookie.defaultSetCookie
                  { Cookie.setCookieName = "p215-auth"
                  , Cookie.setCookieValue = "deleted"
                  , Cookie.setCookieExpires = Just now
                  , Cookie.setCookieHttpOnly = True
                  , Cookie.setCookieSecure = shouldBeSeure
                  , Cookie.setCookieSameSite = Just Cookie.sameSiteStrict
                  , Cookie.setCookiePath = Just "/"
                  }
  pure $ addHeader setCookie' ()


passwordLogin
  :: MonadDb env m
  => PassLogin
  -> m (CookieHeader ())
passwordLogin MkPassLogin{email, password} = do
  mHash <- getPasswordHash email
  case mHash of
    Nothing -> Errs.throwAuthErr
    Just (userId, hash) ->
      if comparePassword password hash
        then setCookie userId
        else Errs.throwAuthErr


logout
  :: MonadDb env m
  => (AuthUser, T.CookieToken)
  -> m (CookieHeader ())
logout (_, token) = do
  invalidateCookie token
  setCookieDelete


createUser
  :: ( MonadDb env m
     , Mail.HasSmtp env
     )
  => User.NewUser
  -> m (CookieHeader ())
createUser newUser = do
  userId <- User.createUser newUser
  Mail.sendMail (Emails.Welcome.mail newUser.email)
  setCookie userId


newtype PassReset = MkPassReset
  { email :: T.Email
  }
  deriving (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)


resetPassword
  :: ( MonadDb env m
     , Mail.HasSmtp env
     , HasUrl env
     )
  => PassReset
  -> m NoContent
resetPassword MkPassReset{email} = do
  mToken <- User.passwordResetToken email
  for_ mToken $ \ token -> do
    url <- asks (.url)
    Mail.sendMail (Emails.PasswordReset.mail email token url)
  pure NoContent



type Api =
  "password"
    :> Description "Password login"
    :> ReqBody '[JSON] PassLogin
    :> Verb 'POST 204 '[JSON] (CookieHeader ())
  :<|> "logout"
    :> AuthProtect "cookie-with-token"
    :> Verb 'POST 204 '[JSON] (CookieHeader ())
  :<|> "register"
    :> ReqBody '[JSON] User.NewUser
    :> Verb 'POST 204 '[JSON] (CookieHeader ())
  :<|> "password_reset"
    :> ReqBody '[JSON] PassReset
    :> PostNoContent


server
  :: ( MonadDb env m
     , Mail.HasSmtp env
     , HasUrl env
     )
  => ServerT Api m
server =
  passwordLogin
  :<|> logout
  :<|> createUser
  :<|> resetPassword
