module Emails.PasswordReset where

import Lucid
import Emails.Base
import Network.Mail.Mime (Mail(..), Address(..), htmlPart)
import Fields.Email (Email)
import Types qualified as T
import Data.CaseInsensitive qualified as CI

mail
  :: Email
  -> T.PasswordResetToken
  -> Text
  -> Mail
mail toAddr token url =
  Mail
    (Address Nothing "no-reply@p215.church")
    [Address Nothing (CI.original (unwrap toAddr))]
    []
    []
    [("Subject", "Password Reset")]
    [ [ htmlPart (renderText (baseHtml mempty (passwordReset token url)))
      ]
    ]

passwordReset
  :: T.PasswordResetToken
  -> Text
  -> Html ()
passwordReset token baseUrl = do
  b_ "Password Reset"
  br_ []
  br_ []
  "Click the button below to reset your password."
  br_ []
  "If you didn't request this, no worries — just ignore this email. The link will expire in 10 minutes."
  br_ []
  br_ []
  a_ [href_ url, style buttonStyle] "Reset Password"
    where
      url = baseUrl <> "/reset_token?token=" <> unwrap token

