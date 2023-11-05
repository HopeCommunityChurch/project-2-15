module Api.Htmx.Studies where

import Clay ((?), (-:), (**))
import Clay qualified as C
import Clay.Media qualified as CM
import Clay.Render (Config (..))
import Lucid hiding (for_)
import Servant
import Servant.HTML.Lucid
import DbHelper (MonadDb)
import Api.Auth (AuthUser(..))
import Data.Text qualified as T
import Entity.Document qualified as Doc
import Prelude hiding ((**))
import Data.FileEmbed (embedFile, makeRelativeToLocationPredicate)

urlBase :: Text
urlBase = "/api/htmx"


type Url = Text


blue100 :: Text
blue100 = "#0057d1"

jsFileBs :: ByteString
jsFileBs =
  $(makeRelativeToLocationPredicate (const True) "./Studies.js" >>= embedFile)

css :: C.Css
css = do
  C.body ? do
    "font-family" -: "Poppins, sans-serif !important"
    "margin" -: "0px"
  C.header ** C.img ? do
    "max-height" -: "40px"
  C.header ** C.nav ? do
    "margin-left" -: "auto"
  C.header ** C.nav ** C.ul ? do
    "list-style" -: "none"
    "display" -: "flex"
  C.header ** C.nav ** C.li ? do
    "margin-left" -: "20px"
    "display" -: "flex"
  C.header ** C.nav ** C.li ** C.a ? do
    "color" -: "#000"
    "margin" -: "auto"
    "text-decoration" -: "none"
  C.header ? do
    "box-sizing" -: "border-box"
    "background-color" -: "#fff"
    "grid-column" -: "1/13"
    "align-items" -: "center"
    "width" -: "100%"
    "height" -: "52px"
    "padding" -: "0 20px"
    "display" -: "flex"
    "box-shadow" -: "0 2px 4px #0003"
  C.button ? do
    "cursor" -: "pointer"
    "vertical-align" -: "middle"
    "font-size" -: "inherit"
    "border" -: "0"
    "outline" -: "none"
    "font-family" -: "inherit"
    "text-decoration" -: "none"
    "display" -: "inline-block"
    "position" -: "relative"
  ".blue_button" ? do
    "color" -: "#fff"
    "background-color" -: blue100
    "white-space" -: "nowrap"
    "text-overflow" -: "ellipsis"
    "border" -: "none"
    "border-radius" -: "5px"
    "padding" -: "7px 15px"
    "font-size" -: "14px"
    "font-weight" -: "600"
    "overflow" -: "hidden"

  ".profile_button" ? do
    "color" -: "#333"
    "cursor" -: "pointer"
    "background-color" -: "#eee"
    "border-radius" -: "50%"
    "justify-content" -: "center"
    "align-items" -: "center"
    "width" -: "40px"
    "height" -: "40px"
    "margin-left" -: "16px"
    "display" -: "flex"
    "position" -: "relative"

  ".profile_dropdown" ? do
    "z-index" -: "1000"
    "background-color" -: "#fff"
    "border" -: "1px solid #ccc"
    "border-radius" -: "7px"
    "padding" -: "14px 0"
    "display" -: "none"
    "position" -: "absolute"
    "top" -: "50px"
    "right" -: "10px"
    "box-shadow" -: "0 2px 4px #0003"

  ".profile_dropdown a" ? do
    "text-decoration" -: "none"
    "color" -: "#6c6c6c"
    "padding" -: "8px 26px"

  ".profile_dropdown a:hover" ? do
    "background-color" -: "#f6f6f6"

  ".my-studies" ? do
    "width" -: "95%"
    "max-width" -: "1080px"
    "margin" -: "0 auto"
  ".studies" ? do
    "color" -: "#1d1e21"
    "border-collapse" -: "separate"
    "border-spacing" -: "0 10px"
    "width" -: "100%"
  ".studies th" ? do
    "text-align" -: "left"
  ".studies td" ? do
    "padding" -: "10px"
  ".studies .tableRow:hover" ? do
    "background-color" -: "#eef5ff"
  ".studies .tableRow" ? do
    "cursor" -: "pointer"
    "border-radius" -: "9px"
    "transition" -: "background-color .3s"
    "box-shadow" -: "0 4px 7px 1px #00000012"
  ".studies .tableRow .groupStudy" ? do
    "color" -: "#797d80"
  ".show" ? do
    "display" -: "block"


cssRendered :: Text
cssRendered =
  toStrict (C.renderWith C.compact [] css)


externalLinks :: Html ()
externalLinks = do
  traverse_
    (uncurry linkHtml)
    links
  where
    links =
      [ ("Teachings", "https://experiencethehope.com/teaching")
      , ("Equipping", "https://experiencethehope.com/equipping")
      , ("Messaging", "https://messaging.subsplash.com/25FXCW/auth")
      ]
    linkHtml :: Text -> Url -> Html ()
    linkHtml name url =
      li_ [] $
        a_ [href_ url, target_ "_blank"] (toHtml name)


header :: AuthUser -> Html ()
header user = do
  header_ [] $ do
    img_ []
    nav_ [] $ do
      ul_ [] $ do
        externalLinks
        li_ $ do
          button_ [class_ "blue_button" ] "+ New Study"
    div_ [] $ do
      div_ [class_ "profile_button", id_ "profile_button"] "JO"
      div_ [class_ "profile_dropdown", id_ "profile_dropdown"] $
        a_ [href_ (urlBase <> "/signout")] "Sign Out"
  where
    nameAbbr = T.toUpper $ T.take 2 $ user.name


baseHtml :: MonadDb env m => AuthUser -> m (Html ())
baseHtml user = do
  docs <- Doc.getAllDocs user
  pure $ do
    doctypehtml_ $ do
      head_ $ do
        meta_ [content_ "width=device-width, initial-scale=1.0", name_ "viewport"]
        script_ [src_ "https://unpkg.com/htmx.org@1.9.7"] ("" :: Text)
        title_ "Project 2:15 - My Studies"
        style_ [type_ "text/css", media_ "screen"] $
          cssRendered
      body_ [] $ do
        header user
        div_ [class_ "my-studies"] $ do
          h1_ "My Studies"
          table_ [ class_ "studies" ] $ do
            thead_ $ do
              th_ "Study Title"
              th_ "Group Study"
              th_ "Last Opened by Me"
            tbody_ $ for_ docs $ \ doc -> do
              tr_ [class_ "tableRow"] $ do
                td_ $ toHtml doc.name
                td_ [class_ "groupStudy"] $ toHtml $
                  case doc.groupStudyName of
                    Just name -> name
                    Nothing -> "Independent"
                td_ $ toHtml (show @Text doc.updated)

        script_ [] $ jsFileBs


type Api
  = AuthProtect "cookie"
    :> Get '[HTML] (Html ())


server
  :: MonadDb env m
  => ServerT Api m
server = baseHtml
