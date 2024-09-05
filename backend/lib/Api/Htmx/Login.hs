module Api.Htmx.Login where

import Api.Auth (deleteCookie, setCookie')
import Api.Htmx.Ginger (baseContext, baseUrl, gvalHelper, readFromTemplates)
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
import Text.Ginger
import Text.Ginger.Html (htmlSource)
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
  result <- readFromTemplates "login.html"
  mRedirect <- L.lookup "redirect" <$> queryParams
  case result of
    Right template -> do
      let context = baseContext
                    & HMap.insert "redirect" (toGVal mRedirect)
      let content = makeContextHtml (gvalHelper context)
      let h = runGinger content template
      html $ toLazy (htmlSource h)
    Left err -> html (show err)


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


loginForm
  :: ( MonadIO m
     , MonadLogger m
     )
  => Maybe Text
  -> T.Email
  -> Text
  -> ActionT m ()
loginForm mRedirect email password = do
  result <- readFromTemplates "login/form.html"
  case result of
    Right template -> do
      let context = baseContext
                    & HMap.insert "email" (toGVal (original (unwrap email)))
                    & HMap.insert "password" (toGVal password)
                    & HMap.insert "wasCorrect" "False"
                    & HMap.insert "redirect" (toGVal mRedirect)
      logInfoSH context
      let content = makeContextHtml (gvalHelper context)
      let h = runGinger content template
      html $ toLazy (htmlSource h)
    Left err -> html (show err)


login
  :: ( MonadDb env m
     , MonadLogger m
     )
  => ActionT m ()
login = do
  email <- formParam "email"
  password <- formParam "password"
  mRedirect <- L.lookup "redirect" <$> formParams
  let mRedirect' = fmap toStrict mRedirect
  mHash <- lift $ getPasswordHash email
  case mHash of
    Just (userId, hash) -> do
      if comparePassword (passwordFromText password) hash
        then do
          let url = case mRedirect of
                      Just re ->
                        if re == ""
                          then baseUrl <> "/studies"
                          else re
                      Nothing  -> baseUrl <> "/studies"
          logDebugSH url
          setHeader "HX-Redirect" url
          cookie <- lift $ setCookie' userId
          let cookieTxt = toLazy (decodeUtf8 (Cookie.renderSetCookieBS cookie))
          setHeader "Set-Cookie" cookieTxt
          status status200

        else loginForm mRedirect' email password
    _ -> loginForm mRedirect' email password


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
