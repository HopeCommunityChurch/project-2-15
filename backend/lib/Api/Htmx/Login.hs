module Api.Htmx.Login where

import Api.Auth (deleteCookie, setCookie')
import Api.Htmx.Ginger (baseUrl, basicTemplate)
import Data.CaseInsensitive (original)
import Data.HashMap.Strict qualified as HMap
import Data.List qualified as L
import Database qualified as Db
import Database.Beam (
  all_,
  guard_,
  runSelectReturningOne,
  select,
  val_,
  (==.),
 )
import DbHelper (MonadDb, runBeam)
import Network.HTTP.Types.Status (status200, status302)
import Password (PasswordHash, comparePassword, passwordFromText)
import Text.Ginger (ToGVal (..))
import Types qualified as T
import Web.Cookie qualified as Cookie
import Web.Scotty.Trans hiding (scottyT)
import Prelude hiding ((**))




getLogin
  :: ( MonadIO m
     , MonadLogger m
     )
  => ActionT m ()
getLogin = do
  mRedirect <- L.lookup "redirect" <$> queryParams
  mVerified <- L.lookup "verified" <$> queryParams
  basicTemplate
    "login.html"
    ( HMap.insert "redirect" (toGVal mRedirect)
    . HMap.insert "justVerified" (toGVal (mVerified == Just "1"))
    )


getPasswordHash
  :: MonadDb env m
  => T.Email
  -> m (Maybe (T.UserId, PasswordHash, Bool))
getPasswordHash email =
  runBeam
  $ runSelectReturningOne
  $ select
  $ do
    user <- all_ Db.db.user
    guard_ $ user.email ==. val_ email
    p <- all_ Db.db.userPassword
    guard_ $ p.userId ==. user.userId
    pure (p.userId, p.password, user.emailVerified)


loginForm
  :: ( MonadIO m
     , MonadLogger m
     )
  => Maybe Text
  -> T.Email
  -> Text
  -> ActionT m ()
loginForm mRedirect email password = do
  basicTemplate
    "login/form.html"
    ( HMap.insert "email" (toGVal (original (unwrap email)))
    . HMap.insert "password" (toGVal password)
    . HMap.insert "wasCorrect" "False"
    . HMap.insert "redirect" (toGVal mRedirect)
    )


loginFormUnverified
  :: ( MonadIO m
     , MonadLogger m
     )
  => Maybe Text
  -> T.Email
  -> ActionT m ()
loginFormUnverified mRedirect email = do
  basicTemplate
    "login/form.html"
    ( HMap.insert "email" (toGVal (original (unwrap email)))
    . HMap.insert "password" (toGVal ("" :: Text))
    . HMap.insert "unverified" (toGVal True)
    . HMap.insert "redirect" (toGVal mRedirect)
    )


login
  :: ( MonadDb env m
     , MonadLogger m
     )
  => ActionT m ()
login = do
  email <- formParam "email"
  password <- formParam "password"
  mRedirect <- L.lookup "redirect" <$> formParams
  mHash <- lift $ getPasswordHash email
  case mHash of
    Just (userId, hash, emailVerified) -> do
      if comparePassword (passwordFromText password) hash
        then do
          if emailVerified
            then do
              let url = case mRedirect of
                          Just re ->
                            if re == ""
                              then baseUrl <> "/studies"
                              else re
                          Nothing  -> baseUrl <> "/studies"
              logDebugSH url
              setHeader "HX-Redirect" (toLazy url)
              cookie <- lift $ setCookie' userId
              let cookieTxt = toLazy (decodeUtf8 (Cookie.renderSetCookieBS cookie))
              setHeader "Set-Cookie" cookieTxt
              status status200
            else loginFormUnverified mRedirect email
        else loginForm mRedirect email password
    _ -> loginForm mRedirect email password


signout
  :: ( MonadDb env m
     , MonadLogger m
     )
  => ActionT m ()
signout = do
  setHeader "Location" (toLazy (baseUrl <> "/"))
  cookie <- lift deleteCookie
  let cookieTxt = toLazy (decodeUtf8 (Cookie.renderSetCookieBS cookie))
  setHeader "Set-Cookie" cookieTxt
  status status302
