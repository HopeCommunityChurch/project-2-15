module Api.Htmx.Study where

import Api.Htmx.AuthHelper (AuthUser (..))
import Api.Htmx.Ginger (baseUrl, basicTemplate)
import Api.Htmx.NotAuthorized qualified as NotAuth
import Api.Htmx.NotFound qualified as NotFound
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.HashMap.Strict qualified as HMap
import Data.Text qualified as Txt
import Data.UUID as UUID
import DbHelper (MonadDb, withTransaction)
import Emails.ShareGroupStudy qualified
import Entity qualified as E
import Entity.Document qualified as Doc
import Entity.Feature qualified as Feature
import Entity.GroupStudy qualified as GroupStudy
import Entity.Shares qualified as Shares
import Entity.User qualified as User
import EnvFields (EnvType (..), HasUrl)
import Fields.Email (mkEmail)
import Lucid qualified as L
import Lucid.Htmx qualified as L
import Mail qualified
import Network.HTTP.Types.Status (status200, status204)
import Text.Ginger
import Text.Ginger.Html (unsafeRawHtml)
import Types qualified as T
import Web.Scotty.Trans hiding (scottyT)
import Prelude hiding ((**))
import Data.CaseInsensitive qualified as CI


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
  userFeatures <- lift $ Feature.getFeaturesForUser user.userId
  docId <- captureParam "documentId"
  doc <- NotFound.handleNotFound (E.getByIdForUser @Doc.GetDoc user) docId
  unless (user.userId `elem` fmap (.userId) doc.editors) $ do
    NotFound.getNotFound
  groupStudy <- forM doc.groupStudyId $ \ groupStudyId ->
     lift $ E.getByIdForUser @GroupStudy.GetGroupStudy user groupStudyId
  let modKey = modifierKey $ getSystem (fmap toStrict mUserAgent)
  envType <- lift (asks (.envType))
  let isLocal = envType == Dev "local"
  basicTemplate
    "study.html"
    ( HMap.insert "isLocal" (toGVal isLocal)
    . HMap.insert "user" (toGVal (Aeson.toJSON user))
    . HMap.insert "modkey" (toGVal modKey)
    . HMap.insert "docName" (toGVal doc.name)
    . HMap.insert "doc" (toGVal (Aeson.toJSON doc))
    . HMap.insert "groupStudy" (toGVal (Aeson.toJSON (join groupStudy)))
    -- unsafeRawHtml is required because Ginger's `| safe` filter produces empty
    -- output for Text GVals in ginger-0.10.5.2 (asText is not used by the filter).
    -- Safety: Aeson.encode produces valid JSON with all user-supplied strings
    -- escaped as JSON string literals (e.g. quotes become \"). The one remaining
    -- HTML risk is "</script>" in a user name breaking out of the <script> tag,
    -- so we replace it with the JSON-equivalent "<\/script>" which is
    -- semantically identical to a JSON parser but safe to the HTML parser.
    . HMap.insert "groupStudyJson" (toGVal (unsafeRawHtml (Txt.replace "</script>" "<\\/script>" (Txt.pack (BLC.unpack (Aeson.encode (join groupStudy)))))))
    . HMap.insert "host" (toGVal host)
    . HMap.insert "features" (toGVal (Aeson.toJSON userFeatures))
    )


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
  title <- formParam "studyTitle"
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
  docId <- captureParam "documentId"
  doc <- NotFound.handleNotFound (E.getByIdForUser @Doc.GetDoc user) docId
  unless (user.userId `elem` fmap (.userId) doc.editors) $ do
    NotAuth.getNotAuth
  lift $ Doc.deleteDocument docId
  let url = baseUrl <> "/"
  setHeader "HX-Redirect" (toLazy url)
  status status200

