module Emails.Base where

import Lucid hiding (for_)
import Clay ((?))
import Clay qualified as C
import Clay.Media qualified as CM


baseCss :: C.Css
baseCss = do
  C.body ? do
    C.backgroundColor (C.rgb 246 246 246)
    C.fontFamily ["sans-serif"] [C.sansSerif]
    C.margin (C.px 0) (C.px 0) (C.px 0) (C.px 0)
  "#body" ? do
    C.backgroundColor C.white
    C.width (C.px 500)
    C.maxWidth (C.pct 100)
    C.margin (C.px 30) C.auto C.auto C.auto
    C.padding (C.px 30) (C.px 10) (C.px 30) (C.px 10)
    C.boxSizing C.borderBox

  C.query CM.screen [CM.maxWidth (C.px 620)] $ do
    "#body" ? do
      C.width (C.pct 100)
      C.margin (C.px 10) (C.px 0) (C.px 0) (C.px 0)


baseHtml :: C.Css -> Html () -> Html ()
baseHtml css inner = do
  doctypehtml_ $ do
    head_ $ do
      meta_ [content_ "width=device-width, initial-scale=1.0", name_ "viewport"]
    style_ [type_ "text/css", media_ "screen"] $
      toStrict (C.render (baseCss <> css))
    body_ $ do
      div_ [ id_ "body"] inner
