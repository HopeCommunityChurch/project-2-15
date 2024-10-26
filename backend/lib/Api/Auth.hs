{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api.Auth (
  authCookie,
  setCookie',
  deleteCookie,
  lookupSession,
  AuthUser (..),
  authCookieToken
) where

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
  (==.),
  (>=.),
 )
import DbHelper (HasDbConn, MonadDb, runBeam, withTransaction)
import Entity qualified as E
import Entity.AuthUser (AuthUser (..))
import EnvFields (EnvType (..), HasEnvType)
import Network.Wai (
  Request,
  requestHeaders,
 )
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


setCookie'
  :: MonadDb env m
  => T.UserId
  -> m Cookie.SetCookie
setCookie' userId = do
  envType <- asks (.envType)
  let shouldBeSeure =
        case envType of
          Dev "local" -> False
          _ -> True
  (token, expiresAt) <- mkCookie userId
  let setCookie'' = Cookie.defaultSetCookie
                  { Cookie.setCookieName = "p215-auth"
                  , Cookie.setCookieValue = encodeUtf8 (unwrap token)
                  , Cookie.setCookieExpires = Just expiresAt
                  , Cookie.setCookieHttpOnly = True
                  , Cookie.setCookieSecure = shouldBeSeure
                  , Cookie.setCookieSameSite = Just Cookie.sameSiteStrict
                  , Cookie.setCookiePath = Just "/"
                  }
  pure setCookie''


deleteCookie :: MonadDb env m => m Cookie.SetCookie
deleteCookie = do
  envType <- asks (.envType)
  now <- getCurrentTime
  let shouldBeSeure =
        case envType of
          Dev "local" -> False
          _ -> True
  pure $ Cookie.defaultSetCookie
    { Cookie.setCookieName = "p215-auth"
    , Cookie.setCookieValue = "deleted"
    , Cookie.setCookieExpires = Just now
    , Cookie.setCookieHttpOnly = True
    , Cookie.setCookieSecure = shouldBeSeure
    , Cookie.setCookieSameSite = Just Cookie.sameSiteStrict
    , Cookie.setCookiePath = Just "/"
    }


