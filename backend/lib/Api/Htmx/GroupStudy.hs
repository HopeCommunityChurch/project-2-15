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
      L.div_ [L.id_ "createPeoples"] createPeopleTemplate
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


memberHTML
  :: Monad m
  => Bool
  -> AuthUser
  -> GroupStudy.GetGroupStudy
  -> GroupStudy.GetDocMeta
  -> L.HtmlT m ()
memberHTML isOwner user groupStudy gdoc = do
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
              "/group_study/"
              <> UUID.toText (unwrap groupStudy.groupStudyId)
              <> "/member/"
              <> UUID.toText (unwrap gdoc.docId)
              <> "/ownership"
        L.pSelect_
          [ L.hxPost_ optionUrl
          , L.hxTrigger_ "change"
          , L.name_ "ownership"
          , L.hxTarget_ ("#" <> memberId)
          , L.hxSwap_ "outerHTML"
          ]
          $ do
            let editorIsOwner = elem editor.userId (fmap (.userId) groupStudy.owners)
            let memberSelect = if editorIsOwner then [] else [L.selected_ ""]
            let ownerSelect = if editorIsOwner then [L.selected_ ""] else []
            L.option_
              ([L.value_ "member"] <> memberSelect)
              "Member"
            L.option_
              ([L.value_ "owner"] <> ownerSelect)
              "Owner"
        let deleteUrl =
              "/group_study/"
              <> UUID.toText (unwrap groupStudy.groupStudyId)
              <> "/member/"
              <> UUID.toText (unwrap gdoc.docId)
        L.button_
          [ L.class_ "red trash"
          , L.hxDelete_ deleteUrl
          , L.hxTarget_ ("#" <> memberId)
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
    if isOwner then
      L.header_ [L.class_ "groupStudyName"] $ do
        let groupIdTxt = UUID.toText (unwrap groupStudy.groupStudyId)
        L.input_
          [ L.contenteditable_ ""
          , L.hxIndicator_ ".saving"
          , L.hxTrigger_ "keyup changed delay:1s"
          , L.hxPost_ ("/group_study/" <> groupIdTxt <> "/name")
          , L.hxTarget_ ".saved"
          , L.name_ "groupName"
          , L.value_ groupStudy.name
          ]
        L.div_ [L.class_ "saving-box"] $ do
          L.span_ [L.class_ "saving saving-indicator"] "saving"
          L.span_ [L.class_ "saved"] mempty
    else
      L.h3_ (L.toHtml groupStudy.name)

    L.h4_ "Invites"
    L.div_ [L.id_ "studyGroupInvites"] $
      for_ shares $ \ share -> do
        shareHTML isOwner share groupStudy

    L.h4_ "Members"
    L.div_ [L.id_ "studyGroupMembers"] $
      for_ groupStudy.docs $ \ gdoc -> do
        memberHTML isOwner user groupStudy gdoc

    when isOwner $ do
      let groupIdTxt = UUID.toText (unwrap groupStudy.groupStudyId)
      L.button_
        [ L.class_ "lightBlue"
        , L.hxGet_ ("/group_study/invite/" <> groupIdTxt)
        , L.hxTarget_ "#groupStudyInner"
        ]
        "Invite New Members"


textToPermission :: Text -> Maybe GroupStudy.Permission
textToPermission "owner" = Just GroupStudy.Owner
textToPermission "member" = Just GroupStudy.Member
textToPermission _ = Nothing


parseFormBody :: [Param] -> [Shares.ShareUnit]
parseFormBody form =
  let permissions = form
                    & filter (\ (key, _) -> key == "permission[]")
                    & fmap (textToPermission . snd)
      emails = form
                & filter (\ (key, _) -> key == "email[]")
                & fmap (mkEmail . snd)
  in zip emails permissions
              & mapMaybe (\case
                    (Right email, Just per) -> Just (email, per)
                    _ -> Nothing
              )
              & fmap (\ (email, per) ->
                Shares.MkShareUnit email (per == GroupStudy.Owner) Nothing
              )



createGroupStudy
  :: ( MonadDb env m
     , Mail.HasSmtp env
     , HasUrl env
     )
  => AuthUser
  -> ActionT m ()
createGroupStudy user = do
  (docId :: T.DocId) <- formParam "documentId"
  doc <- NotFound.handleNotFound (E.getByIdForUser @Doc.GetDoc user) docId
  groupName <- formParam "name"
  form <- formParams
  let shares = parseFormBody form
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


removeMemberDoc
  :: MonadDb env m
  => AuthUser
  -> ActionT m ()
removeMemberDoc user = do
  groupStudyId <- captureParam "groupId"
  (docId :: T.DocId) <- captureParam "docId"
  groupStudy <- NotFound.handleNotFound
                  (E.getById @GroupStudy.GetGroupStudy)
                  groupStudyId
  let isOwner = any (\ o -> o.userId == user.userId) groupStudy.owners

  docMeta <- NotFound.handleNotFound
                  (E.getOneEntityBy @GroupStudy.GetDocMeta)
                  docId

  unless isOwner $
    L.renderScotty $ do
      memberHTML False user groupStudy docMeta
      L.notifcation_ [ L.timems_ "2500", L.type_ "error" ]
        "You are not an owner. You cannot modify the group."

  let tryingToDeleteEditor = any (\e -> e.userId == user.userId) docMeta.editors
  when (length groupStudy.owners == 1 && tryingToDeleteEditor) $
    L.renderScotty $ do
      memberHTML True user groupStudy docMeta
      L.notifcation_ [ L.timems_ "2500", L.type_ "error" ]
        "Cannot remove the last owner."

  lift $ GroupStudy.removeFromGroup docId
  lift $ GroupStudy.removeOwner groupStudyId (fmap (.userId) docMeta.editors)

  L.renderScotty $ do
    L.notifcation_ [ L.timems_ "2500" ]
      "Removed from group."


ownershipMemberDoc
  :: MonadDb env m
  => AuthUser
  -> ActionT m ()
ownershipMemberDoc user = do
  groupStudyId <- captureParam "groupId"
  logInfoSH groupStudyId
  (docId :: T.DocId) <- captureParam "docId"
  logInfoSH docId

  groupStudy <- NotFound.handleNotFound
                  (E.getById @GroupStudy.GetGroupStudy)
                  groupStudyId
  let isOwner = any (\ o -> o.userId == user.userId) groupStudy.owners
  logInfoSH isOwner

  docMeta <- NotFound.handleNotFound
                  (E.getOneEntityBy @GroupStudy.GetDocMeta)
                  docId
  logInfoSH docMeta

  (ownership :: Text) <- formParam "ownership"

  logInfo ownership

  permission <- case textToPermission ownership of
                  Nothing -> do
                    L.renderScotty $ do
                      memberHTML False user groupStudy docMeta
                      L.notifcation_ [ L.timems_ "2500", L.type_ "error" ]
                        "You are not an owner. You cannot modify the group."
                  Just p -> pure p

  logInfoSH permission

  unless isOwner $
    L.renderScotty $ do
      memberHTML False user groupStudy docMeta
      L.notifcation_ [ L.timems_ "2500", L.type_ "error" ]
        "You are not an owner. You cannot modify the group."


  let modifyingSelf = user.userId `elem` fmap (.userId) docMeta.editors
  when (length groupStudy.owners == 1 && permission == GroupStudy.Member && modifyingSelf) $
    L.renderScotty $ do
      memberHTML True user groupStudy docMeta
      L.notifcation_ [ L.timems_ "2500", L.type_ "error" ]
        "Cannot remove the last owner."

  case permission of
    GroupStudy.Member ->
      lift $ GroupStudy.removeOwner groupStudyId (fmap (.userId) docMeta.editors)
    GroupStudy.Owner ->
      lift $ GroupStudy.addOwners groupStudyId (fmap (.userId) docMeta.editors)

  groupStudy2 <- NotFound.handleNotFound
                  (E.getById @GroupStudy.GetGroupStudy)
                  groupStudyId

  docMeta2 <- NotFound.handleNotFound
                  (E.getOneEntityBy @GroupStudy.GetDocMeta)
                  docId

  L.renderScotty $ do
    memberHTML True user groupStudy2 docMeta2
    L.notifcation_ [ L.timems_ "2500" ]
      "Modified ownership"




getInvite
  :: ( MonadDb env m
     , MonadLogger m
     )
  => AuthUser
  -> ActionT m ()
getInvite _ = do
  groupId <- captureParam "groupId"
  html =<< L.renderTextT (getInviteNewMemberHTML groupId)


createPeopleTemplate :: Monad m => L.HtmlT m ()
createPeopleTemplate = do
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


getInviteNewMemberHTML
  :: Monad m
  => T.GroupStudyId
  -> L.HtmlT m ()
getInviteNewMemberHTML groupId = do
  L.div_ [L.class_ "groupStudyInner"] $ do
    L.h3_ "Invite New People"
    let formUrl = "/group_study/invite/add"
    L.form_ [ L.hxPost_ formUrl, L.class_ "groupStudyEditorHolder", L.hxTarget_ "#groupStudyInner"] $ do
      L.input_
        [ L.id_ "groupId"
        , L.name_ "groupId"
        , L.type_ "hidden"
        , L.value_ (UUID.toText (unwrap groupId))
        ]
      L.label_ [L.for_ "createPeople"] "People"
      L.div_ [L.id_ "createPeoples"] createPeopleTemplate
      L.button_ [L.type_ "submit", L.class_ "blue"]
        "Invite"
  L.script_ "addPeopleInput()";


postInvite
  :: ( MonadDb env m
     , MonadLogger m
     , Mail.HasSmtp env
     , HasUrl env
     )
  => AuthUser
  -> ActionT m ()
postInvite user = do
  groupId <- formParam "groupId"
  groupStudy <- NotFound.handleNotFound
                  (E.getByIdForUser @GroupStudy.GetGroupStudy user)
                  groupId
  let isOwner = any (\ o -> o.userId == user.userId) groupStudy.owners

  unless isOwner $
    L.renderScotty $ do
      L.notifcation_ [ L.timems_ "2500", L.type_ "error" ]
        "You are not an owner. You cannot modify the group."

  form <- formParams
  let shares = parseFormBody form
  html =<< L.renderTextT (getInviteNewMemberHTML groupId)
  lift $ withTransaction $ do
    result <- Shares.addShares groupId shares
    url <- asks (.url)
    for_ result $ \ (share, token) ->  do
      let email = Emails.ShareGroupStudy.mail share groupStudy.name token url
      Mail.sendMail email
  getGroupStudy' user groupId


nameUpdate
  :: ( MonadDb env m
     , MonadLogger m
     , HasUrl env
     )
  => AuthUser
  -> ActionT m ()
nameUpdate user = do
  groupId <- captureParam "groupId"
  groupStudy <- NotFound.handleNotFound
                  (E.getByIdForUser @GroupStudy.GetGroupStudy user)
                  groupId
  let isOwner = any (\ o -> o.userId == user.userId) groupStudy.owners

  unless isOwner $
    L.renderScotty $ do
      L.notifcation_ [ L.timems_ "2500", L.type_ "error" ]
        "You are not an owner. You cannot modify the group."

  name <- formParam "groupName"
  lift $ GroupStudy.updateName groupId name
  html $ L.renderText $ do
    L.notifcation_ [ L.timems_ "2500" ]
      "Saved!"


reviewShareModal
  :: ( MonadDb env m
     , MonadLogger m
     )
  => AuthUser
  -> ActionT m ()
reviewShareModal _ = do
  shareToken <- captureParam "shareToken"
  share <- NotFound.handleNotFound Shares.getShareFromToken shareToken
  L.renderScotty $ do
    L.div_ [L.class_ "modal-content invite-modal"] $ do
      L.div_ [L.class_ "modal-header"] $ do
        L.h3_ "You\x2019ve been invited to study!"
        L.div_ [L.class_ "modal-header-buttons"] $ do
          L.button_
            [ L.class_ "lightBlue"
            , L.hxGet_ ("/group_study/share/" <> unwrap shareToken <> "/confirm-reject")
            , L.hxTarget_ "#inviteModal"
            , L.hxSwap_ "innerHTML"
            ]
            "Reject"
          L.button_
            [ L.class_ "blue"
            , L.hxGet_ ("/group_study/share/" <> unwrap shareToken <> "/select-document")
            , L.hxTarget_ "#inviteModal"
            , L.hxSwap_ "innerHTML"
            ]
            "Accept"
          L.img_
            [ L.src_ "/static/img/x.svg"
            , L.class_ "closeModalIcon"
            , L.onclick_ "document.querySelector('#inviteModal').close()"
            ]
      L.div_ [L.class_ "modal-body"] $ do
        L.p_ $ do
          maybe "Someone" L.toHtml share.ownerName
          " has invited you to join \x201c"
          L.toHtml share.groupStudyName
          "\x201d"
          case share.studyTemplateName of
            Just tName -> do
              ", a study based on the \x201c"
              L.toHtml tName
              "\x201d template."
            Nothing -> "."


confirmRejectModal
  :: ( MonadDb env m
     , MonadLogger m
     )
  => AuthUser
  -> ActionT m ()
confirmRejectModal _ = do
  shareToken <- captureParam "shareToken"
  share <- NotFound.handleNotFound Shares.getShareFromToken shareToken
  L.renderScotty $ do
    L.div_ [L.class_ "modal-content invite-modal"] $ do
      L.div_ [L.class_ "modal-header"] $ do
        L.h3_ "Are you sure?"
        L.div_ [L.class_ "modal-header-buttons"] $ do
          L.img_
            [ L.src_ "/static/img/x.svg"
            , L.class_ "closeModalIcon"
            , L.onclick_ "document.querySelector('#confirmRejectModal').close()"
            ]
      L.div_ [L.class_ "modal-body"] $ do
        L.p_ $ do
          "Are you sure you want to reject the invite to \x201c"
          L.toHtml share.groupStudyName
          "\x201d?"
        L.div_ [L.class_ "modal-buttons"] $ do
          L.button_
            [ L.class_ "lightBlue"
            , L.onclick_ "document.querySelector('#confirmRejectModal').close()"
            ]
            "Cancel"
          L.button_
            [ L.class_ "red"
            , L.hxDelete_ ("/group_study/share/" <> unwrap shareToken)
            , L.hxTarget_ ("#invite-" <> unwrap shareToken)
            , L.hxSwap_ "delete"
            , L.hxOn_ "after-request" "document.querySelector('#confirmRejectModal').close(); document.querySelector('#inviteModal').close()"
            ]
            "Reject"


selectDocumentModal
  :: ( MonadDb env m
     , MonadLogger m
     )
  => AuthUser
  -> ActionT m ()
selectDocumentModal user = do
  shareToken <- captureParam "shareToken"
  share <- NotFound.handleNotFound Shares.getShareFromToken shareToken
  docs <- lift $ Doc.getAllDocs user
  let ungroupedDocs = filter (\d -> isNothing d.groupStudyId) docs
  L.renderScotty $ do
    L.div_ [L.class_ "modal-content invite-modal"] $ do
      L.div_ [L.class_ "modal-header"] $ do
        L.h3_ "Join this study"
        L.div_ [L.class_ "modal-header-buttons"] $ do
          L.button_
            [ L.class_ "red"
            , L.hxGet_ ("/group_study/share/" <> unwrap shareToken <> "/confirm-reject")
            , L.hxTarget_ "#confirmRejectModal"
            , L.hxSwap_ "innerHTML"
            , L.hxOn_ "after-request" "toggleModal('#confirmRejectModal')"
            ]
            "Reject"
          L.img_
            [ L.src_ "/static/img/x.svg"
            , L.class_ "closeModalIcon"
            , L.onclick_ "document.querySelector('#inviteModal').close()"
            ]
      L.div_ [L.class_ "modal-body"] $ do
        L.p_ $ do
          maybe "Someone" L.toHtml share.ownerName
          " has invited you to join \x201c"
          L.toHtml share.groupStudyName
          "\x201d. How would you like to join?"
        L.div_ [L.class_ "doc-choice-cards"] $ do
          L.div_ [L.class_ "doc-choice-card"] $ do
            L.h4_ "Use an existing document"
            if (length ungroupedDocs == 0) then do
              L.p_ [L.class_ "doc-choice-desc doc-choice-empty"]
                "You don\x2019t have any documents that aren\x2019t already part of a group study."
            else do
              L.p_ [L.class_ "doc-choice-desc"] "Link a document you\x2019ve already been working on to this group study."
              L.form_
                [ L.hxPost_ ("/group_study/share/" <> unwrap shareToken)
                ] $ do
                L.pSelect_ [L.name_ "document", L.id_ "docSelect", L.variant_ "modern"] $ do
                  for_ ungroupedDocs $ \d ->
                    L.option_ [L.value_ (UUID.toText (unwrap d.docId))] (L.toHtml d.name)
                L.button_ [L.type_ "submit", L.class_ "blue"]
                  "Join with this document"
          L.div_ [L.class_ "doc-choice-card"] $ do
            L.h4_ "Create a new document"
            L.p_ [L.class_ "doc-choice-desc"] "Start fresh with a blank document for this study."
            L.form_
              [ L.hxPost_ ("/group_study/share/" <> unwrap shareToken)
              ] $ do
              L.input_ [L.type_ "hidden", L.name_ "document", L.value_ "new"]
              L.button_ [L.type_ "submit", L.class_ "blue"]
                "Create & join"
