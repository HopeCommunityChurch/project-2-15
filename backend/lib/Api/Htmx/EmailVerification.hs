module Api.Htmx.EmailVerification where

import Api.Htmx.Ginger (baseUrl, basicTemplate)
import Data.HashMap.Strict qualified as HMap
import DbHelper (MonadDb)
import Emails.EmailVerification qualified
import Entity.User qualified as User
import EnvFields (HasUrl)
import Mail qualified
import Network.HTTP.Types.Status (status200, status302)
import Text.Ginger (ToGVal (..))
import Types qualified as Ty
import Web.Scotty.Trans hiding (scottyT)
import Prelude hiding ((**))


getVerifyEmail
  :: ( MonadDb env m
     , MonadLogger m
     )
  => ActionT m ()
getVerifyEmail = do
  (tokenText :: Text) <- queryParam "token"
  let token = Ty.MkNewType tokenText
  mUserId <- lift $ User.verifyEmailToken token
  case mUserId of
    Just _ -> do
      setHeader "Location" (toLazy (baseUrl <> "/login?verified=1"))
      status status302
    Nothing ->
      basicTemplate "verifyEmail.html" identity


postResendVerification
  :: ( MonadDb env m
     , MonadLogger m
     , Mail.HasSmtp env
     , HasUrl env
     )
  => ActionT m ()
postResendVerification = do
  emailParam <- formParam "email"
  mUserId <- lift $ User.getUnverifiedUserId emailParam
  case mUserId of
    Nothing ->
      -- Don't reveal whether the email exists; silently return success.
      html "<div class=\"successText\" id=\"resend-result\">If that address has a pending verification, we sent a new email.</div>"
    Just userId -> do
      mSecondsRemaining <- lift $ User.checkVerificationRateLimit userId
      case mSecondsRemaining of
        Just secondsRemaining -> do
          let minutes = secondsRemaining `div` 60
              secs    = secondsRemaining `mod` 60
              msg     = "Please wait " <> show minutes <> ":" <> pad secs <> " before requesting another email."
          html $ toLazy $ "<div class=\"errorText\" id=\"resend-result\">" <> msg <> "</div>"
          status status200
        Nothing -> do
          token <- lift $ User.createVerificationToken userId emailParam
          url <- lift $ asks (.url)
          lift $ Mail.sendMail (Emails.EmailVerification.mail emailParam token url)
          html "<div class=\"successText\" id=\"resend-result\">Verification email sent! Check your inbox.</div>"
          status status200
  where
    pad n = if n < 10 then "0" <> show n else show n
