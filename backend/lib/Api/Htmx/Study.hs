module Api.Htmx.Study where

import Api.Htmx.AuthHelper (AuthUser (..))
import Api.Htmx.Ginger (baseUrl, basicTemplate)
import Api.Htmx.NotAuthorized qualified as NotAuth
import Api.Htmx.NotFound qualified as NotFound
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
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
import Mail qualified
import Network.HTTP.Types.Status (status200)
import Text.Ginger
import Types qualified as T
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
  userFeatures <- lift $ Feature.getFeaturesForUser user.userId
  docId <- captureParam "documentId"
  doc <- NotFound.handleNotFound (E.getByIdForUser @Doc.GetDoc user) docId
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



textToPermission :: Text -> Maybe GroupStudy.Permission
textToPermission "owner" = Just GroupStudy.Owner
textToPermission "member" = Just GroupStudy.Member
textToPermission _ = Nothing


createGroupStudy
  :: ( MonadDb env m
     , Mail.HasSmtp env
     , HasUrl env
     )
  => AuthUser
  -> ActionT m ()
createGroupStudy user = do
  docId <- formParam @T.DocId "documentId"
  doc <- NotFound.handleNotFound (E.getByIdForUser @Doc.GetDoc user) docId
  groupName <- formParam "name"
  form <- formParams
  let permissions = form
                    & filter (\ (key, _) -> key == "permission[]")
                    & fmap (textToPermission . toStrict . snd)
  let emails = form
                & filter (\ (key, _) -> key == "email[]")
                & fmap (mkEmail . toStrict . snd)
  let shares = zip emails permissions
              & mapMaybe (\case
                    (Right email, Just per) -> Just (email, per)
                    _ -> Nothing
              )
              & fmap (\ (email, per) ->
                Shares.MkShareUnit email (per == GroupStudy.Owner) Nothing
              )
  logInfoSH emails  -- lift $ GroupStudy.addStudy user.userId undefined
  let crGroupStudy = GroupStudy.MkCrStudy groupName doc.studyTemplateId
  lift $ withTransaction $ do
    groupId <- GroupStudy.addStudy user.userId crGroupStudy
    Doc.addToGroup doc.docId groupId
    result <- Shares.addShares groupId shares
    url <- asks (.url)
    for_ result $ \ (share, token) ->  do
      let email = Emails.ShareGroupStudy.mail share groupName token url
      Mail.sendMail email
  basicTemplate
    "study/studyGroup.html"
    ( HMap.insert "user" (toGVal (Aeson.toJSON user))
    . HMap.insert "doc" (toGVal (Aeson.toJSON doc))
    . HMap.insert "created" (toGVal True)
    )

