module Emails.EmailChangeNotification where

import Lucid
import Emails.Base
import Network.Mail.Mime (Mail(..), Address(..), htmlPart)
import Fields.Email (Email)
import Data.CaseInsensitive qualified as CI

mail
  :: Email
  -> Email
  -> Mail
mail oldAddr newAddr =
  Mail
    (Address Nothing "no-reply@p215.church")
    [Address Nothing (CI.original (unwrap oldAddr))]
    []
    []
    [("Subject", "Your email address is being changed")]
    [ [ htmlPart (renderText (baseHtml mempty (emailChangeNotice newAddr)))
      ]
    ]

emailChangeNotice
  :: Email
  -> Html ()
emailChangeNotice newAddr = do
  b_ "Email address change requested"
  br_ []
  br_ []
  "A request was made to change the email address on your Project 2:15 account to:"
  br_ []
  br_ []
  b_ (toHtml (CI.original (unwrap newAddr)))
  br_ []
  br_ []
  "If you made this request, you can ignore this email. The change will take effect once the new address is verified."
  br_ []
  "If you did not make this request, please contact support immediately."
