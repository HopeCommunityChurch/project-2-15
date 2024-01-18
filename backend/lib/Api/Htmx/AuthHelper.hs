module Api.Htmx.AuthHelper (
  getUser,
  getUserWithRedirect,
  Auth.AuthUser,
) where

import Prelude hiding ((**))
import Web.Scotty.Trans hiding (scottyT)
import Api.Auth qualified as Auth
import Types qualified as T
import DbHelper (MonadDb)
import Web.Cookie qualified as Cookie
import Data.List qualified as List
import Api.Htmx.Ginger (baseUrl)
import Network.HTTP.Types.Status (status204, status302)



getUser
  :: (MonadDb env m, MonadLogger m)
  => ScottyError e
  => ActionT e m (Maybe Auth.AuthUser)
getUser = do
  mCookie <- header "Cookie"
  case mCookie of
    Nothing -> pure Nothing
    Just cookieTxt -> do
      let parsed = Cookie.parseCookies (encodeUtf8 (toStrict cookieTxt))
      case List.lookup "p215-auth" parsed of
        Nothing -> pure Nothing
        Just bsCookie -> do
          let cookie = T.MkNewType (decodeUtf8 bsCookie)
          lift $ Auth.lookupSession cookie


getUserWithRedirect
  :: (MonadDb env m, MonadLogger m)
  => ScottyError e
  => ActionT e m Auth.AuthUser
getUserWithRedirect = do
  mUser <- getUser
  case mUser of
    Just user -> pure user
    Nothing -> do
      mIsHTMX <- header "HX-Request"
      let url = baseUrl <> "/login"
      if isJust mIsHTMX
        then do
          setHeader "HX-Redirect" (toLazy url)
          raiseStatus status204 (stringError "redirect")
        else do
          setHeader "Location" (toLazy url)
          raiseStatus status302 (stringError "redirect")

