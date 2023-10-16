module Emails.Base where

import Clay ((?))
import Clay qualified as C
import Clay.Media qualified as CM
import Clay.Render (Config (..))
import Lucid hiding (for_)

bodyStyle :: C.Css
bodyStyle = do
  C.backgroundColor (C.rgb 246 246 246)
  C.fontFamily ["sans-serif"] [C.sansSerif]
  C.margin (C.px 0) (C.px 0) (C.px 0) (C.px 0)


innerBodyStyle :: C.Css
innerBodyStyle = do
  C.backgroundColor C.white
  C.width (C.px 500)
  C.maxWidth (C.pct 100)
  C.margin (C.px 30) C.auto C.auto C.auto
  C.padding (C.px 30) (C.px 10) (C.px 30) (C.px 10)
  C.boxSizing C.borderBox


buttonStyle :: C.Css
buttonStyle = do
  C.display C.flex
  C.cursor C.pointer
  C.outline C.none (C.px 0) C.white
  C.border C.none (C.px 0) C.white
  C.borderRadius (C.px 5) (C.px 5) (C.px 5) (C.px 5)
  C.backgroundColor (C.rgb 0 87 209)
  C.whiteSpace C.nowrap
  --there's got to be a better way to do this
  C.fontWeight (C.other (C.value (600 :: Integer)))
  C.color C.white


baseCss :: C.Css
baseCss = do
  C.body ? do
    bodyStyle
  "#innerBody" ? do
    innerBodyStyle

  C.query CM.screen [CM.maxWidth (C.px 620)] $ do
    "#innerBody" ? do
      C.important (C.width (C.pct 100))
      C.important (C.margin (C.px 10) (C.px 0) (C.px 0) (C.px 0))


inline :: Config
inline = Config
  { indentation    = ""
  , newline        = " "
  , sep            = ""
  , lbrace         = ""
  , rbrace         = ""
  , finalSemicolon = False
  , warn           = False
  , align          = False
  , banner         = False
  , comments       = False
  }


style :: C.Css -> Attribute
style css =
    style_ $ toStrict (C.renderWith inline [] css)


baseHtml :: C.Css -> Html () -> Html ()
baseHtml css inner = do
  doctypehtml_ $ do
    head_ $ do
      meta_ [content_ "width=device-width, initial-scale=1.0", name_ "viewport"]
    style_ [type_ "text/css", media_ "screen"] $
      toStrict (C.render (baseCss <> css))
    body_ $ do
      div_ [ id_ "innerBody", style innerBodyStyle] inner
