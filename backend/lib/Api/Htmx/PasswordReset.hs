module Api.Htmx.PasswordReset where

import Api.Auth (setCookie')
import Api.Htmx.Ginger (baseContext, baseUrl, gvalHelper, readFromTemplates)
import Data.Aeson qualified as Aeson
import Data.HashMap.Strict qualified as HMap
import Data.List qualified as L
import Data.Text qualified as T
import DbHelper (MonadDb)
import Emails.PasswordReset qualified
import Entity.User qualified as User
import EnvFields (HasUrl)
import Mail qualified
import Network.HTTP.Types.Status (status200)
import Password (newPassword)
import Text.Ginger
import Text.Ginger.Html (htmlSource)
import Types qualified as T
import Web.Cookie qualified as Cookie
import Web.Scotty.Trans hiding (scottyT)
import Prelude hiding ((**))



getPasswordReset
  :: ( MonadIO m
     , MonadLogger m
     )
  => ActionT m ()
getPasswordReset = do
  result <- readFromTemplates "passwordReset.html"
  mRedirect <- L.lookup "redirect" <$> queryParams
  case result of
    Right template -> do
      let context = baseContext
                    & HMap.insert "redirect" (toGVal mRedirect)
      let content = makeContextHtml (gvalHelper context)
      let h = runGinger content template
      html $ toLazy (htmlSource h)
    Left err -> html (show err)


resetEmail
  :: ( MonadDb env m
     , MonadLogger m
     , Mail.HasSmtp env
     , HasUrl env
     )
  => ActionT m ()
resetEmail = do
  email <- formParam "email"
  mToken <- lift $ User.passwordResetToken email
  for_ mToken $ \ token -> do
    url <- lift $ asks (.url)
    lift $ Mail.sendMail (Emails.PasswordReset.mail email token url)
  html "Check your email"


getResetToken
  :: ( MonadIO m
     , MonadLogger m
     )
  => ActionT m ()
getResetToken = do
  result <- readFromTemplates "passwordResetToken.html"
  (token :: Text) <- queryParam "token"
  case result of
    Right template -> do
      let context = baseContext
                    & HMap.insert "token" (toGVal token)
      let content = makeContextHtml (gvalHelper context)
      let h = runGinger content template
      html $ toLazy (htmlSource h)
    Left err -> html (show err)



data PasswordErrors = MkPasswordErrors
  { passwordsMatch :: Bool
  , passwordLength :: Bool
  , tokenIsValid :: Bool
  , password :: Text
  , password2 :: Text
  }
  deriving (Generic, Show)
  deriving anyclass (ToJSON)


postResetToken
  :: ( MonadDb env m
     , MonadLogger m
     , Mail.HasSmtp env
     )
  => ActionT m ()
postResetToken = do
  password <- formParam "password"
  password2 <- formParam "password2"
  (token :: Text) <- formParam "token"
  let passwordsMatch = password == password2
  let passwordLength = T.length password > 8
  let token' = T.mkPasswordResetToken token
  mUserId <- lift $ User.getUserFromResetToken token'
  if isJust mUserId && passwordsMatch && passwordLength then do
    lift $ User.invalidateResetToken token'
    case mUserId of
      Just userId -> do
        lift $ User.updatePassword userId (newPassword password)
        let url = baseUrl <> "/studies"
        setHeader "HX-Redirect" url
        cookie <- lift $ setCookie' userId
        let cookieTxt = toLazy (decodeUtf8 (Cookie.renderSetCookieBS cookie))
        setHeader "Set-Cookie" cookieTxt
        status status200
      Nothing ->
        pure ()
  else do
    let errors = MkPasswordErrors
                  passwordsMatch
                  passwordLength
                  (isJust mUserId)
                  password
                  password2
    result <- readFromTemplates "passwordReset/form.html"
    case result of
      Right template -> do
        let context = baseContext
                      & HMap.insert "token" (toGVal token)
                      & HMap.insert "wasCorrect" "False"
                      & HMap.insert "errors" (toGVal (Aeson.toJSON errors))
        let content = makeContextHtml (gvalHelper context)
        let h = runGinger content template
        html $ toLazy (htmlSource h)
      Left err -> html (show err)

