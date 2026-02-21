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
    L.img_
      [ L.alt_ "Close"
      , L.src_ "/static/img/x.svg"
      , L.class_ "closeModalIcon"
      , L.onclick_ "toggleModal('#groupStudy')"
      ]
    L.h3_ "Create Group Study"
    L.p_ [L.class_ "field-desc"]
      "A group study lets multiple people work through the same study template together, each with their own document."
    let formUrl = "/group_study"
    L.form_ [ L.hxPost_ formUrl, L.class_ "groupStudyEditorHolder"] $ do
      L.input_
        [ L.id_ "documentId"
        , L.name_ "documentId"
        , L.type_ "hidden"
        , L.value_ (UUID.toText (unwrap doc.docId))
        ]
      L.label_ [L.for_ "createName", L.class_ "field-label"] "Study Name"
      L.input_
        [ L.id_ "createName"
        , L.name_ "name"
        , L.type_ "text"
        , L.required_ ""
        , L.placeholder_ "1 Corinthians - Wednesday Guy's Group"
        ]
      L.label_ [L.for_ "createPeople", L.class_ "field-label"] "Invite people (optional)"
      L.p_ [L.class_ "field-desc"]
        "You can also invite people later."
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
  let statusBadge
        | share.rejected  = L.span_ [L.class_ "badge badge-error"] "Rejected"
        | share.isExpired = L.span_ [L.class_ "badge badge-error"] "Expired"
        | otherwise       = L.span_ [L.class_ "badge badge-pending"] "Invited"
  L.div_ [L.class_ "share person-row", L.id_ shareId] $ do
    L.div_ [L.class_ "person-info"] $ do
      L.span_ [L.class_ "person-name"] $ do
        L.toHtml (CI.original (unwrap share.email))
        statusBadge
    when isOwner $ do
      L.div_ [L.class_ "person-actions"] $ do
        let resendUrl =
              "/group_study/"
              <> UUID.toText (unwrap groupStudy.groupStudyId)
              <> "/share/"
              <> unwrap share.token
              <> "/resend"
        L.button_
          [ L.class_ "ghost-blue"
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
          [ L.class_ "remove-btn"
          , L.hxDelete_ deleteUrl
          , L.hxTarget_ ("#" <> shareId)
          , L.hxSwap_ "outerHTML"
          ]
          "Remove"


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
  L.div_ [L.class_ "member person-row", L.id_ memberId] $ do
    L.div_ [L.class_ "person-info"] $ do
      L.span_ [L.class_ "person-name"] $ do
        L.toHtml editor.name
        when (editor.userId == user.userId) $
          L.span_ [L.class_ "you-label"] " (you)"
        let editorIsOwner = elem editor.userId (fmap (.userId) groupStudy.owners)
        unless isOwner $
          L.span_ [L.class_ "person-role"] $
            if editorIsOwner then "Owner" else "Member"
      L.span_ [L.class_ "person-email"] $
        L.toHtml (CI.original (unwrap editor.email))
    L.div_ [L.class_ "person-actions"] $ do
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
          [ L.class_ "remove-btn"
          , L.hxDelete_ deleteUrl
          , L.hxTarget_ ("#" <> memberId)
          , L.hxSwap_ "outerHTML"
          ]
          "Remove"


groupStudyHTML
  :: Monad m
  => AuthUser
  -> Bool
  -> [Shares.GetShareData]
  -> GroupStudy.GetGroupStudy
  -> L.HtmlT m ()
groupStudyHTML user isOwner shares groupStudy = do
  let groupIdTxt = UUID.toText (unwrap groupStudy.groupStudyId)
  L.div_ [ L.class_ "groupStudyEditorHolder" , L.id_ "groupStudyInner" ] $ do
    L.img_
      [ L.alt_ "Close"
      , L.src_ "/static/img/x.svg"
      , L.class_ "closeModalIcon"
      , L.onclick_ "toggleModal('#groupStudy')"
      ]

    -- Header
    L.div_ [L.class_ "groupStudy-header"] $ do
      L.h3_ "Group Study"
      if isOwner then do
        L.label_ [L.class_ "field-label"] "Study name"
        L.div_ [L.class_ "groupStudyName"] $ do
          L.input_
            [ L.type_ "text"
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
        L.p_ [L.class_ "groupStudy-name-readonly"] (L.toHtml groupStudy.name)

    -- Inline invite form (owners only)
    when isOwner $ do
      L.div_ [L.class_ "groupStudy-section"] $ do
        L.label_ [L.class_ "field-label"] "Invite someone"
        L.p_ [L.class_ "field-desc"]
          "They'll receive an email with a link to join this study."
        L.form_
          [ L.class_ "groupStudy-invite-row"
          , L.hxPost_ "/group_study/invite/add"
          , L.hxTarget_ "#groupStudyInner"
          ] $ do
            L.input_
              [ L.name_ "groupId"
              , L.type_ "hidden"
              , L.value_ groupIdTxt
              ]
            L.input_
              [ L.name_ "email[]"
              , L.type_ "email"
              , L.placeholder_ "Email address"
              , L.required_ ""
              ]
            L.pSelect_ [ L.name_ "permission[]"] $ do
              L.option_ [ L.value_ "member"] "Member"
              L.option_ [ L.value_ "owner"] "Owner"
            L.button_ [L.type_ "submit", L.class_ "blue"] "Invite"

    L.hr_ [L.class_ "groupStudy-divider"]

    -- People with access
    L.div_ [L.class_ "groupStudy-section"] $ do
      L.label_ [L.class_ "field-label"] "People with access"
      when isOwner $
        L.p_ [L.class_ "field-desc"]
          "Use the role dropdown to set someone as a Member or Owner."
      L.div_ [L.class_ "groupStudy-people"] $ do
        for_ groupStudy.docs $ \ gdoc -> do
          memberHTML isOwner user groupStudy gdoc
        unless (Prelude.null shares) $ do
          for_ shares $ \ share -> do
            shareHTML isOwner share groupStudy


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
