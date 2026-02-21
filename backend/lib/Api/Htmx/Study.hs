module Api.Htmx.Study where

import Api.Htmx.AuthHelper (AuthUser (..))
import Api.Htmx.Ginger (baseUrl, basicTemplate)
import Api.Htmx.NotAuthorized qualified as NotAuth
import Api.Htmx.NotFound qualified as NotFound
import Data.Aeson qualified as Aeson
import Data.HashMap.Strict qualified as HMap
import Data.Text qualified as Txt
import Data.UUID as UUID
import DbHelper (MonadDb)
import Entity qualified as E
import Entity.Document qualified as Doc
import Entity.Feature qualified as Feature
import Entity.GroupStudy qualified as GroupStudy
import EnvFields (EnvType (..), HasUrl)
import Lucid qualified as L
import Lucid.Htmx qualified as L
import Network.HTTP.Types.Status (status200)
import Text.Ginger
import Types qualified as T
import Web.Scotty.Trans hiding (scottyT)
import Prelude hiding ((**))
import Network.HTTP.Types (urlEncode)


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
    . HMap.insert "host" (toGVal host)
    . HMap.insert "features" (toGVal (Aeson.toJSON userFeatures))
    )


createStudy
  :: ( MonadDb env m
     )
  => AuthUser
  -> ActionT m ()
createStudy user = do
  title <- formParam "studyTitle"
  let crDoc = Doc.CrDoc Nothing title Doc.emptyStudy user.userId
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
  let encodedName = decodeUtf8 (urlEncode True (encodeUtf8 doc.name))
  let url = baseUrl <> "/studies?deleted=" <> UUID.toText (unwrap docId) <> "&name=" <> encodedName
  setHeader "HX-Redirect" (toLazy url)
  status status200

restoreStudy
  :: ( MonadDb env m
     )
  => AuthUser
  -> ActionT m ()
restoreStudy user = do
  docId <- captureParam "documentId"
  deletedDocs <- lift $ Doc.getDeletedDocs user
  unless (any (\d -> d.docId == docId) deletedDocs) $ do
    NotAuth.getNotAuth
  lift $ Doc.restoreDocument docId
  redirect (toLazy (baseUrl <> "/study/" <> UUID.toText (unwrap docId)))

