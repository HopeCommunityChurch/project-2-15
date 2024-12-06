module Api.Htmx.GroupStudy where

import Api.Htmx.AuthHelper (AuthUser (..))
import Api.Htmx.NotFound qualified as NotFound
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.CaseInsensitive qualified as CI
import Data.List (head)
import Data.UUID as UUID
import DbHelper (MonadDb, withTransaction)
import Emails.ShareGroupStudy qualified
import Entity qualified as E
import Entity.Document qualified as Doc
import Entity.GroupStudy qualified as GroupStudy
import Entity.Shares qualified as Shares
import Entity.User qualified as User
import EnvFields (HasUrl)
import Fields.Email (mkEmail)
import Lucid qualified as L
import Lucid.Htmx qualified as L
import Mail qualified
import Network.HTTP.Types.Status (status200)
import Types qualified as T
import Web.Scotty.Trans hiding (scottyT)
import Prelude hiding ((**))



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


getGroupStudy'
  :: ( MonadDb env m
     , MonadLogger m
     )
  => AuthUser
  -> T.GroupStudyId
  -> ActionT m ()
getGroupStudy' user groupStudyId = do
  groupStudy <- NotFound.handleNotFound
                  (E.getByIdForUser @GroupStudy.GetGroupStudy user)
                  groupStudyId
  let isOwner = any (\ o -> o.userId == user.userId) groupStudy.owners

  shares <- lift $ Shares.getGroupShareData groupStudy.groupStudyId
  logDebugSH shares
  html =<< L.renderTextT (groupStudyHTML user isOwner shares groupStudy)

getGroupStudy
  :: ( MonadDb env m
     , MonadLogger m
     )
  => AuthUser
  -> ActionT m ()
getGroupStudy user = do
  docId <- captureParam "documentId"
  doc <- NotFound.handleNotFound (E.getByIdForUser @Doc.GetDoc user) docId
  unless (user.userId `elem` fmap (.userId) doc.editors) $ do
    NotFound.getNotFound
  case doc.groupStudyId of
    Just groupStudyId -> do
      getGroupStudy' user groupStudyId
    Nothing -> do
      html =<< L.renderTextT (createStudyGroupHTML doc)


createStudyGroupHTML
  :: Monad m
  => Doc.GetDoc
  -> L.HtmlT m ()
createStudyGroupHTML doc = do
  L.div_ [L.class_ "groupStudyInner"] $ do
    L.h3_ "Create Group Study"
    let formUrl = "/group_study"
    L.form_ [ L.hxPost_ formUrl, L.class_ "groupStudyEditorHolder"] $ do
      L.input_
        [ L.id_ "documentId"
        , L.name_ "documentId"
        , L.type_ "hidden"
        , L.value_ (UUID.toText (unwrap doc.docId))
        ]
      L.label_ [L.for_ "createName"] "Study Name"
      L.input_
        [ L.id_ "createName"
        , L.name_ "name"
        , L.type_ "text"
        , L.required_ ""
        , L.placeholder_ "1 Corinthians - Wednesday Guy's Group"
        ]
      L.label_ [L.for_ "createPeople"] "People"
      L.div_ [L.id_ "createPeoples"] $
        L.template_ [L.id_ "peopleInputTemplate"] $ do
          L.div_ [L.class_ "peopleInput"] $ do
            L.input_
              [ L.name_ "email[]"
              , L.type_ "email"
              , L.placeholder_ "jonny@p215.church"
              ]
            L.pSelect_ [ L.name_ "permission[]"] $ do
              L.option_ [ L.value_ "member"] "Member"
              L.option_ [ L.value_ "owner"] "Owner"
              L.button_ [L.class_ "red"] "-"
      L.button_ [L.type_ "submit", L.class_ "blue"]
        "Create"
  L.script_ "addPeopleInput()";


shareHTML
  :: Monad m
  => Bool
  -> Shares.GetShareData
  -> GroupStudy.GetGroupStudy
  -> L.HtmlT m ()
shareHTML isOwner share groupStudy = do
  let shareId = "share-" <> unwrap share.token
  L.div_ [L.class_ "share", L.id_ shareId] $ do
    L.div_ [L.class_ "share-email"] $ do
      L.toHtml (CI.original (unwrap share.email))
      when share.rejected $ do
        L.span_ [L.class_ "expired"] "(rejected)"
      when share.isExpired $ do
        L.span_ [L.class_ "expired"] "(expired)"
    when isOwner $ do
      L.div_ [L.class_ "buttons"] $ do
        let resendUrl =
              "/group_study/"
              <> UUID.toText (unwrap groupStudy.groupStudyId)
              <> "/share/"
              <> unwrap share.token
              <> "/resend"
        L.button_
          [ L.class_ "lightBlue"
          , L.hxPost_ resendUrl
          , L.hxTarget_ ("#" <> shareId)
          , L.hxSwap_ "outerHTML"
          ]
          "Resend"
        let deleteUrl =
              "/group_study/"
              <> UUID.toText (unwrap groupStudy.groupStudyId)
              <> "/share/"
              <> unwrap share.token
        L.button_
          [ L.class_ "red trash"
          , L.hxDelete_ deleteUrl
          , L.hxTarget_ ("#" <> shareId)
          , L.hxSwap_ "outerHTML"
          ]
          (L.img_ [L.src_ "/static/img/gray-trash-icon.svg"])


groupStudyHTML
  :: Monad m
  => AuthUser
  -> Bool
  -> [Shares.GetShareData]
  -> GroupStudy.GetGroupStudy
  -> L.HtmlT m ()
groupStudyHTML user isOwner shares groupStudy = do
  L.div_ [ L.class_ "groupStudyEditorHolder" , L.id_ "groupStudyInner" ] $ do
    L.h3_ (L.toHtml groupStudy.name)
    L.h4_ "Invites"
    L.div_ [L.id_ "studyGroupInvites"] $
      for_ shares $ \ share -> do
        shareHTML isOwner share groupStudy

    L.h4_ "Members"
    L.div_ [L.id_ "studyGroupMembers"] $
      for_ groupStudy.docs $ \ gdoc -> do
        let editor = head gdoc.editors
        let memberId = "member-doc-" <> UUID.toText (unwrap gdoc.docId)
        L.div_ [L.class_ "member", L.id_ memberId] $ do
          L.div_ $ do
            L.toHtml editor.name
            when (editor.userId == user.userId) $
              L.span_ "(you)"
          L.div_ [L.class_ "buttons"] $
            when isOwner $ do
              let optionUrl =
                    "/groupStudy/"
                    <> UUID.toText (unwrap groupStudy.groupStudyId)
                    <> "/member/"
                    <> UUID.toText (unwrap editor.userId)
                    <> "/ownerShip"
              L.pSelect_ [ L.hxPost_ optionUrl , L.hxTrigger_ "input"] $ do
                L.option_
                  [L.value_ "member", L.selected_ ""]
                  "Member"
                L.option_
                  [L.value_ "owner"]
                  "Owner"
              L.button_
                [ L.class_ "red trash"
                , L.hxDelete_ optionUrl
                , L.hxTarget_ ("#" <> memberId)
                , L.hxSwap_ "outerHTML"
                ]
                (L.img_ [L.src_ "/static/img/gray-trash-icon.svg"])

    when isOwner $
      L.button_
        [L.class_ "lightBlue"]
        "Invite New Members"


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
  groupId <- lift $ withTransaction $ do
    groupId <- GroupStudy.addStudy user.userId crGroupStudy
    Doc.addToGroup doc.docId groupId
    result <- Shares.addShares groupId shares
    url <- asks (.url)
    for_ result $ \ (share, token) ->  do
      let email = Emails.ShareGroupStudy.mail share groupName token url
      Mail.sendMail email
    pure groupId
  getGroupStudy' user groupId


rejectShare
  :: MonadDb env m
  => AuthUser
  -> ActionT m ()
rejectShare _ = do
  shareToken <- captureParam "shareToken"
  lift $ Shares.rejectToken shareToken
  status status200


resendInvite
  :: ( MonadDb env m
     , Mail.HasSmtp env
     , HasUrl env
     )
  => AuthUser
  -> ActionT m ()
resendInvite _ = do
  shareToken <- captureParam "shareToken"
  groupStudy <- NotFound.handleNotFound
                  (E.getOneEntityBy @GroupStudy.GetGroupStudy)
                  shareToken
  share <- NotFound.handleNotFound
                  Shares.getGroupShareDataByToken
                  shareToken
  shareForEmail <- lift $ Shares.expandShareExpire shareToken

  lift $ withTransaction $ do
    url <- asks (.url)
    let email = Emails.ShareGroupStudy.mail
                    shareForEmail
                    groupStudy.name
                    shareToken
                    url
    Mail.sendMail email
  html $ L.renderText $ do
      shareHTML True share groupStudy
      L.notifcation_ [ L.timems_ "2500" ]
        "Sent!"



acceptShare
  :: MonadDb env m
  => AuthUser
  -> ActionT m ()
acceptShare user = do
  shareToken <- captureParam "shareToken"
  docStuff <- formParam "document"
  docId <-
    if docStuff == ("new" :: Text) then do
      let crDoc = Doc.CrDoc Nothing "Untitled" emptyStudy user.userId
      lift $ Doc.crDocument crDoc
    else do
      docId <- formParam "document"
      doc <- NotFound.handleNotFound (E.getByIdForUser @Doc.GetDoc user) docId
      unless (user.userId `elem` fmap (.userId) doc.editors) $ do
        NotFound.getNotFound
      pure docId

  didWork <- lift $ Shares.acceptShare user.userId shareToken docId
  unless didWork
    NotFound.getNotFound

  let url = "/study/" <> UUID.toText (unwrap docId)
  setHeader "HX-Redirect" (toLazy url)
  status status200


-- In theory I could check to make sure that the token is actually part of
-- this group and that the user is an owner, but you can already reject a
-- token by tokenId
ownerShareDelete
  :: MonadDb env m
  => AuthUser
  -> ActionT m ()
ownerShareDelete _ = do
  shareToken <- captureParam "shareToken"
  lift $ Shares.deleteToken shareToken
  status status200


