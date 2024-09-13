module Emails.ShareGroupStudy where

import Data.CaseInsensitive qualified as CI
import Emails.Base
import Entity.Shares qualified as Shares
import Lucid hiding (for_)
import Network.Mail.Mime (Address (..), Mail (..), htmlPart)
import Types qualified as T

mail
  :: Shares.ShareUnit
  -> Text
  -> T.ShareToken
  -> Text
  -> Mail
mail Shares.MkShareUnit{..} name token url =
  Mail
    (Address Nothing "no-reply@p215.church")
    [Address Nothing (CI.original (unwrap email))]
    []
    []
    [("Subject", "Invited Group Study")]
    [ [ htmlPart (renderText (baseHtml mempty (shareEmail token name message url)))
      ]
    ]

shareEmail
  :: T.ShareToken
  -> Text
  -> Maybe Text
  -> Text
  -> Html ()
shareEmail token name mMessage baseUrl = do
  b_ $ "Invited to join a Group Study " <> toHtml name
  br_ []
  br_ []

  for_ mMessage $ \ message -> do
    toHtml message
    br_ []
    br_ []

  "Click the button below to join " <> toHtml name <> "."
  br_ []
  "If you don't join, the invitation will expire in 2 weeks."
  br_ []
  br_ []
  a_ [href_ url, style buttonStyle] "join"
    where
      url = baseUrl <> "/studies?share_token=" <> unwrap token


