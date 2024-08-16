module Api.Htmx.Study where

import Api.Htmx.AuthHelper (AuthUser (..))
import Api.Htmx.Ginger (baseContext, baseUrl, gvalHelper, readFromTemplates)
import Api.Htmx.NotAuthorized qualified as NotAuth
import Api.Htmx.NotFound qualified as NotFound
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.HashMap.Strict qualified as HMap
import Data.Text qualified as Txt
import Data.UUID as UUID
import DbHelper (MonadDb)
import Entity qualified as E
import Entity.Document qualified as Doc
import Entity.User qualified as User
import EnvFields (EnvType (..))
import Network.HTTP.Types.Status (status200)
import Text.Ginger
import Text.Ginger.Html (htmlSource)
import Web.Scotty.Trans hiding (scottyT)
import Prelude hiding ((**))


data SystemType = Linux | Mac | Windows | Unknown
  deriving (Show, Eq, Ord, Read)

contains :: Text -> Text -> Bool
contains needle haystack =
  let (_, s) = Txt.breakOn needle haystack
   in not (Txt.null s)


getSystem :: Maybe Text -> SystemType
getSystem Nothing = Unknown
getSystem (Just str)
  | contains "Mac" str = Mac
  | contains "Linux" str = Linux
  | contains "Windows" str = Windows
  | otherwise = Unknown


modifierKey :: SystemType -> Text
modifierKey Mac = "⌘"
modifierKey _ = "ctrl"


getStudy
  :: ( MonadDb env m
     , MonadLogger m
     )
  => AuthUser
  -> ActionT m ()
getStudy user = do
  mUserAgent <- header "User-Agent"
  mHost <- header "X-Forwarded-Host"
  let host = fromMaybe "local.p215.church" mHost
  docId <- param "documentId"
  doc <- NotFound.handleNotFound (E.getByIdForUser @Doc.GetDoc user) docId
  let modKey = modifierKey $ getSystem (fmap toStrict mUserAgent)
  result <- readFromTemplates "study.html"
  envType <- lift (asks (.envType))
  let isLocal = envType == Dev "local"
  case result of
    Right template -> do
      let context = baseContext
                    & HMap.insert "isLocal" (toGVal isLocal)
                    & HMap.insert "user" (toGVal (Aeson.toJSON user))
                    & HMap.insert "modkey" (toGVal modKey)
                    & HMap.insert "docName" (toGVal doc.name)
                    & HMap.insert "doc" (toGVal (Aeson.toJSON doc))
                    & HMap.insert "host" (toGVal host)
      let content = makeContextHtml (gvalHelper context)
      let h = runGinger content template
      html $ toLazy (htmlSource h)
    Left err -> html (show err)

unsafeAsObject :: Aeson.Value -> Aeson.Object
unsafeAsObject (Aeson.Object o) = o
unsafeAsObject _ = error "not an object"


emptyStudy :: Aeson.Object
emptyStudy = unsafeAsObject $ Aeson.object
  [ "type" .= ("doc" :: Text)
  , "content" .=
    [ Aeson.object
      [ "type" .= ("section" :: Text)
      , "content" .=
        [ Aeson.object
          [ "type" .= ("sectionHeader" :: Text)
          , "content" .= [ Aeson.object [ "text" .= ("Untitled" :: Text), "type" .= ("text" :: Text)]]
          ]
        , Aeson.object
          [ "type" .= ("studyBlocks" :: Text)
          , "content" .= [ Aeson.object [ "type" .= ("questions" :: Text)]]
          ]
        ]
      ]
    ]
  ]


createStudy
  :: ( MonadDb env m
     )
  => AuthUser
  -> ActionT m ()
createStudy user = do
  title <- param "studyTitle"
  let crDoc = Doc.CrDoc Nothing title emptyStudy user.userId
  docId <- lift $ Doc.crDocument crDoc
  let url = baseUrl <> "/study/" <> UUID.toText (unwrap docId)
  setHeader "HX-Redirect" (toLazy url)
  status status200


deleteStudy
  :: ( MonadDb env m
     )
  => AuthUser
  -> ActionT m ()
deleteStudy user = do
  docId <- param "documentId"
  doc <- NotFound.handleNotFound (E.getByIdForUser @Doc.GetDoc user) docId
  unless (user.userId `elem` fmap (.userId) doc.editors) $ do
    NotAuth.getHome
  lift $ Doc.deleteDocument docId
  let url = baseUrl <> "/"
  setHeader "HX-Redirect" (toLazy url)
  status status200
