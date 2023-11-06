module Api.Htmx.Studies where

import Clay ((?), (-:), (**))
import Clay qualified as C
import Lucid qualified as L
import Lucid hiding (for_)
import Lucid.Base
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

-- jsFileBs :: ByteString
-- jsFileBs =
--   $(makeRelativeToLocationPredicate (const True) "Studies.js" >>= embedFile)

hxTrigger_ :: Text -> Attribute
hxTrigger_ = makeAttribute "hx-trigger"

hxSwap_ :: Text -> Attribute
hxSwap_ = makeAttribute "hx-swap"

hxTarget_ :: Text -> Attribute
hxTarget_ = makeAttribute "hx-target"

hxPost_ :: Url -> Attribute
hxPost_ = makeAttribute "hx-post"

hxGet_ :: Url -> Attribute
hxGet_ = makeAttribute "hx-get"

under_ :: Text -> Attribute
under_ = makeAttribute "_"

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

  "#profile_button" ? do
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

  "#profile_dropdown" ? do
    "z-index" -: "1000"
    "background-color" -: "#fff"
    "border" -: "1px solid #ccc"
    "border-radius" -: "7px"
    "padding" -: "14px 0"
    "position" -: "absolute"
    "top" -: "50px"
    "right" -: "10px"
    "box-shadow" -: "0 2px 4px #0003"

  "#profile_dropdown a" ? do
    "text-decoration" -: "none"
    "color" -: "#6c6c6c"
    "padding" -: "8px 26px"

  "#profile_dropdown a:hover" ? do
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
  ".modal-background" ? do
    "z-index" -: "1001"
    "background-color" -: "#00000080"
    "justify-content" -: "center"
    "align-items" -: "center"
    "width" -: "100%"
    "height" -: "100%"
    "display" -: "flex"
    "position" -: "fixed"
    "top" -: "0"
    "left" -: "0"
  ".modal" ? do
    "background-color" -: "#fff"
    "border-radius" -: "8px"
    "width" -: "90%"
    "max-width" -: "700px"
    "min-height" -: "50vh"
    "max-height" -: "93vh"
    "padding" -: "20px"
    "position" -: "relative"
    "overflow-y" -: "scroll"
    "box-shadow" -: "0 4px 6px #0000001a"
  ".model label" ? do
    "color" -: "#666"
    "margin-bottom" -: "5px"
    "font-size" -: "16px"
  ".modal input" ? do
    "border" -: "1px solid #ccc"
    "border-radius" -: "4px"
    "margin-bottom" -: "15px"
    "padding" -: "12px"
    "font-size" -: "14px"
  ".modal p" ? do
    "color" -: "#aaa"
    "margin-top" -: "-7px"
    "font-size" -: "14px"
  ".modal form" ? do
    "flex-direction" -: "column"
    "align-items" -: "stretch"
    "width" -: "100%"
    "margin-top" -: "20px"
    "display" -: "flex"
  ".modal form:invalid input" ? do
    "border-color" -: "red"
  ".modal form:invalid button" ? do
    "opacity" -: ".5"
    "cursor" -: "not-allowed !important"





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

xData :: Text -> Attribute
xData = makeAttribute "x-data"

xClick :: Text -> Attribute
xClick = makeAttribute "@click"

xClickOutside :: Text -> Attribute
xClickOutside = makeAttribute "@click.outside"

xShow :: Text -> Attribute
xShow = makeAttribute "x-show"

xRef :: Text -> Attribute
xRef = makeAttribute "x-ref"

header :: AuthUser -> Html ()
header user = do
  header_ [] $ do
    img_ []
    nav_ [] $ do
      ul_ [] $ do
        externalLinks
        li_ $
          button_
            [ class_ "blue_button"
            , hxGet_ (urlBase <> "/studies/new-study")
            , hxTarget_ "body"
            , hxSwap_ "beforeend"
            ]
            "+ New Study"
    div_ [xData "{open : false}"] $ do
      div_
        [ id_ "profile_button"
        , xClick "open = !open"
        ]
        (toHtml nameAbbr)
      div_
        [ id_ "profile_dropdown"
        , xShow "open"
        , xClickOutside "open = false"
        ]
        (a_ [href_ (urlBase <> "/signout")] "Sign Out")
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
        script_ [defer_ "true", src_ "https://cdn.jsdelivr.net/npm/alpinejs@3.x.x/dist/cdn.min.js"] ("" :: Text)
        title_ "Project 2:15 - My Studies"
        style_ [type_ "text/css", media_ "screen"] cssRendered
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
                  fromMaybe "Independent" doc.groupStudyName
                td_ $ toHtml (show @Text doc.updated)

        -- script_ [] $ jsFileBs


newStudyHtml :: MonadDb env m => AuthUser -> m (Html ())
newStudyHtml _ = pure $
  div_
    [ class_ "modal-background"
    , xData ""
    , xRef "test"
    ]
    $ div_
      [ xClickOutside "$refs.test.remove()"
      , class_ "modal"
      ]
      $ do
        h3_ "Create New Study"
        form_ [hxPost_ "/studies/new-study"] $ do
          label_ [L.for_ "name"] "Title"
          input_ [id_ "name", placeholder_ "New Title", required_ ""]
          p_ [] $ do
            "Ex: \""
            em_ "Romans"
            "\" or \""
            em_ "1 Corinthians"
          button_ [type_ "submit", class_ "blue_button"] "Create"





type Api
  = AuthProtect "cookie"
    :> Get '[HTML] (Html ())
  :<|> AuthProtect "cookie"
    :> "new-study"
    :> Get '[HTML] (Html ())


server
  :: MonadDb env m
  => ServerT Api m
server =
  baseHtml
  :<|> newStudyHtml
