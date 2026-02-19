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
  b_ "Welcome to Project 2:15!"
  br_ []
  br_ []
  "We're so glad you've joined us! "
  "Jump in whenever you're ready — there's no wrong way to start. "
  br_ []
  br_ []
  "Happy studying!"
