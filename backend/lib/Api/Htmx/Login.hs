module Api.Htmx.Login where

import Api.Auth (setCookie')
import Api.Htmx.Ginger (baseContext, baseUrl, gvalHelper, readFromTemplates)
import Data.CaseInsensitive (original)
import Data.HashMap.Strict qualified as HMap
import Database qualified as Db
import Database.Beam (
  all_,
  guard_,
  insert,
  insertValues,
  runInsert,
  runSelectReturningOne,
  runUpdate,
  select,
  update,
  val_,
  (<-.),
  (==.),
  (>=.),
 )
import DbHelper (HasDbConn, MonadDb, runBeam, withTransaction)
import Network.HTTP.Types.Status (status200)
import Password (NewPassword, Password, PasswordHash, comparePassword, passwordFromText)
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
  => ScottyError e
  => ActionT e m ()
getLogin = do
  result <- readFromTemplates "login.html"
  case result of
    Right template -> do
      let content = makeContextHtml (gvalHelper baseContext)
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
  => ScottyError e
  => T.Email
  -> Text
  -> ActionT e m ()
loginForm email password = do
  result <- readFromTemplates "login/form.html"
  case result of
    Right template -> do
      let context = baseContext
                    & HMap.insert "email" (toGVal (original (unwrap email)))
                    & HMap.insert "password" (toGVal password)
                    & HMap.insert "wasCorrect" "False"
      logInfoSH context
      let content = makeContextHtml (gvalHelper context)
      let h = runGinger content template
      html $ toLazy (htmlSource h)
    Left err -> html (show err)


login
  :: ( MonadDb env m
     , MonadLogger m
     )
  => ScottyError e
  => ActionT e m ()
login = do
  email <- param "email"
  password <- param "password"
  mHash <- lift $ getPasswordHash email
  case mHash of
    Just (userId, hash) -> do
      if comparePassword (passwordFromText password) hash
        then do
          setHeader "HX-Redirect" (toLazy (baseUrl <> "/studies"))
          cookie <- lift $ setCookie' userId
          let cookieTxt = toLazy (decodeUtf8 (Cookie.renderSetCookieBS cookie))
          setHeader "Set-Cookie" cookieTxt
          status status200

        else loginForm email password
    _ -> loginForm email password
