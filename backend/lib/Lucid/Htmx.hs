module Lucid.Htmx where

import Lucid.Base (makeAttribute, Attribute)


hxGet :: Text -> Attribute
hxGet = makeAttribute "hx-get"

hxPost :: Text -> Attribute
hxPost = makeAttribute "hx-post"

hxDelete :: Text -> Attribute
hxDelete = makeAttribute "hx-delete"

hxTarget :: Text -> Attribute
hxTarget = makeAttribute "hx-target"

hxConfirm :: Text -> Attribute
hxConfirm = makeAttribute "hx-confirm"

hxSwap :: Text -> Attribute
hxSwap = makeAttribute "hx-swap"
