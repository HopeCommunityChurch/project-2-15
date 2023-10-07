module Emails.Welcome (mail) where

import Lucid
import Emails.Base
import Network.Mail.Mime (Mail(..), Address(..), htmlPart)
import Fields.Email (Email)
import Data.CaseInsensitive qualified as CI

mail :: Email -> Mail
mail toAddr =
  Mail
    (Address Nothing "no-reply@p215.church")
    [(Address Nothing (CI.original (unwrap toAddr)))]
    []
    []
    [("Subject", "Welcome to Project 2:15")]
    [ [ htmlPart (renderText (baseHtml mempty welcome))
      ]
    ]

welcome :: Html ()
welcome = do
  b_ "Welcome to Project 2:15."
  br_ []
  br_ []
  "Right now this email is basically blank. "
  "We'll fill it in the future and there will be a verification link. "
