module Emails.EmailVerification where

import Lucid
import Emails.Base
import Network.Mail.Mime (Mail(..), Address(..), htmlPart)
import Fields.Email (Email)
import Types qualified as T
import Data.CaseInsensitive qualified as CI

mail
  :: Email
  -> T.EmailVerificationToken
  -> Text
  -> Mail
mail toAddr token url =
  Mail
    (Address Nothing "no-reply@p215.church")
    [Address Nothing (CI.original (unwrap toAddr))]
    []
    []
    [("Subject", "Verify your email address")]
    [ [ htmlPart (renderText (baseHtml mempty (verifyEmail token url)))
      ]
    ]

verifyEmail
  :: T.EmailVerificationToken
  -> Text
  -> Html ()
verifyEmail token baseUrl = do
  b_ "Verify your email address"
  br_ []
  br_ []
  "Welcome to Project 2:15! Click the button below to verify your email address."
  br_ []
  "This link will expire in 24 hours."
  br_ []
  br_ []
  a_ [href_ url, style buttonStyle] "Verify Email"
    where
      url = baseUrl <> "/verify_email?token=" <> unwrap token
