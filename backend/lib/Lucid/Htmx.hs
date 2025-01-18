module Lucid.Htmx where

import Lucid.Base (makeAttribute, Attribute, Term, term)
import Lucid qualified as L
import Web.Scotty.Trans qualified as Scotty


hxGet_ :: Text -> Attribute
hxGet_ = makeAttribute "hx-get"

hxPost_ :: Text -> Attribute
hxPost_ = makeAttribute "hx-post"

hxDelete_ :: Text -> Attribute
hxDelete_ = makeAttribute "hx-delete"

hxTarget_ :: Text -> Attribute
hxTarget_ = makeAttribute "hx-target"

hxConfirm_ :: Text -> Attribute
hxConfirm_ = makeAttribute "hx-confirm"

hxTrigger_ :: Text -> Attribute
hxTrigger_ = makeAttribute "hx-trigger"

hxSwap_ :: Text -> Attribute
hxSwap_ = makeAttribute "hx-swap"

hxSwapOob_ :: Text -> Attribute
hxSwapOob_ = makeAttribute "hx-swap-oob"


-- Not htmx, but I need it somewher

notifcation_ :: Term arg result => arg -> result
notifcation_ = term "p-notification"

timems_ :: Text -> Attribute
timems_ = makeAttribute "time-ms"

pSelect_ :: Term arg result => arg -> result
pSelect_ = term "p-select"


renderScotty
  :: MonadIO m
  => L.HtmlT (Scotty.ActionT m) a
  -> Scotty.ActionT m b
renderScotty markup = do
  Scotty.html =<< L.renderTextT markup
  Scotty.finish


