{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api.Auth (Api, server, authCookie, AuthUser) where

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
  select,
  val_,
  (>=.),
  (==.),
 )
import DbHelper (HasDbConn, MonadDb, runBeam, withTransaction)
import Entity qualified as E
import Entity.AuthUser (AuthUser)
import EnvFields (HasEnvType)
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


instance AuthDescription "cookie" where
  securityName = "cookie"
  securityScheme = OpenApi.SecurityScheme type_ (Just desc)
    where
      type_ = OpenApi.SecuritySchemeApiKey
                (OpenApi.ApiKeyParams "p215-auth" OpenApi.ApiKeyCookie)
      desc  = "p215-auth cookie"

type instance AuthServerData (AuthProtect "cookie") = AuthUser


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
        then do
          (token, expiresAt) <- mkCookie userId
          let setCookie = Cookie.defaultSetCookie
                          { Cookie.setCookieName = "p215-auth"
                          , Cookie.setCookieValue = encodeUtf8 (unwrap token)
                          , Cookie.setCookieExpires = Just expiresAt
                          , Cookie.setCookieHttpOnly = True
                          , Cookie.setCookieSecure = True
                          , Cookie.setCookieSameSite = Just Cookie.sameSiteNone
                          , Cookie.setCookiePath = Just "/"
                          }
          pure $ addHeader setCookie ()
        else Errs.throwAuthErr


type Api =
  "password"
    :> Description "Password login"
    :> ReqBody '[JSON] PassLogin
    :> Verb 'POST 204 '[JSON] (CookieHeader ())


server
  :: MonadDb env m
  => ServerT Api m
server =
  passwordLogin
