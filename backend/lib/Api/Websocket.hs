module Api.Websocket where


import Api.Auth (AuthUser (..))
import Control.Monad.Logger.Prefix (prefixLogs)
import Data.Aeson qualified as Aeson
import Data.Map qualified as Map
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import DbHelper (MonadDb, withTransaction)
import Entity qualified as E
import Entity.Document qualified as Doc
import Entity.User qualified as User
import GHC.Records (HasField)
import Network.WebSockets (Connection, withPingThread)
import Network.WebSockets qualified as WS
import Relude (atomicModifyIORef_)
import Servant
import Types qualified as T
import WebsocketServant
import Entity.Document (GetDoc)
import Types (NewType(MkNewType))


type ConnId = UUID

type DocumentSubs = IORef (Map ConnId Connection)

data DocState = MkDocState
  { editors :: IORef (Map ConnId Connection)
  , subscriptions :: DocumentSubs
  }
  deriving (Generic)

type Subs = IORef (Map T.DocId DocState)

type HasSubs env = HasField "subs" env Subs


mkSubs :: MonadIO m => m Subs
mkSubs = newIORef Map.empty


newtype SaveDoc = MkSaveDoc
  { document :: Aeson.Object
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON)


data InUpdatedPayload = MkInUpdatedPayload
  { version  :: Int32
  , steps    :: [Aeson.Value]
  , clientId :: Text
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON)


data InMsg
  = InOpenDoc T.DocId -- Call before you send updates and saves
  | InCloseDoc T.DocId -- Call when done sending updates
  | InUpdated InUpdatedPayload -- Updating the doc via collab steps
  | InUpdateName Text
  | InSaveDoc SaveDoc
  | InListenToDoc T.DocId  -- Call when you want to listen to the updates for a doc
  | InStopListenToDoc T.DocId
  | OpenStudyChat T.GroupStudyId -- Not implemented yet
  | CloseStudyChat T.GroupStudyId -- Not implemented yet
  deriving (Show, Generic)


fieldModifier :: Int -> String -> String
fieldModifier n = drop n


instance FromJSON InMsg where
  parseJSON =
    Aeson.genericParseJSON
      Aeson.defaultOptions { Aeson.constructorTagModifier = fieldModifier 2 }


newtype SavedDoc = MkSavedDoc
  { time :: UTCTime
  }
  deriving (Generic, Show)
  deriving anyclass (ToJSON)


-- | Payload for confirmed/broadcast step messages.
data StepsPayload = MkStepsPayload
  { version   :: Int32
  , steps     :: [Aeson.Value]
  , clientIds :: [Text]
  , docId     :: T.DocId
  }
  deriving (Generic, Show)
  deriving anyclass (ToJSON)


-- | Payload for conflict messages (no version — client must rebase).
data ConflictPayload = MkConflictPayload
  { steps     :: [Aeson.Value]
  , clientIds :: [Text]
  }
  deriving (Generic, Show)
  deriving anyclass (ToJSON)


-- | Payload sent to the client when a document is opened.
-- Includes the base snapshot document, the OT version it corresponds to,
-- and any steps that need to be applied to reach the current version.
data DocOpenPayload = MkDocOpenPayload
  { doc          :: GetDoc
  , snapVersion  :: Int32
  , pendingSteps :: [Aeson.Value]
  , pendingClientIds :: [Text]
  }
  deriving (Generic, Show)
  deriving anyclass (ToJSON)


data ListenStartPayload = MkListenStartPayload
  { document         :: Aeson.Object
  , version          :: Int32
  , snapVersion      :: Int32
  , pendingSteps     :: [Aeson.Value]
  , pendingClientIds :: [Text]
  }
  deriving (Generic, Show)
  deriving anyclass (ToJSON)


data OutMsg
  = OutDocUpdated StepsPayload   -- broadcast to listeners
  | OutDocConfirmed StepsPayload -- sent back to the editor who submitted steps
  | OutDocConflict ConflictPayload -- version mismatch: client must rebase
  | OutDocOpened DocOpenPayload
  | OutDocOpenedOther -- when the doc is opened on another device, you should close your document
  | OutDocListenStart ListenStartPayload
  | OutDocSaved SavedDoc
  | OutDocNameUpdated
  | OutUnauthorized
  | OutNotFound
  | OutParseError String
  deriving (Show, Generic)

instance ToJSON OutMsg where
  toJSON =
    Aeson.genericToJSON
      Aeson.defaultOptions { Aeson.constructorTagModifier = fieldModifier 3 }


sendOut :: (MonadUnliftIO m, MonadLogger m) => Connection -> OutMsg -> m ()
sendOut conn msg = do
  result <- try $ liftIO $ WS.sendTextData conn (Aeson.encode msg)
  case result of
    Left (err :: SomeException) -> logErrorSH err
    Right _ -> pure ()

data SocketState = MkSocketState
  { openDocument :: Maybe T.DocId
  , sideDoc :: Maybe T.DocId
  , computerId :: T.ComputerId
  }
  deriving (Show, Generic)


getComputerId
  :: MonadUnliftIO m
  => Connection
  -> m T.ComputerId
getComputerId conn = do
  str <- liftIO $ WS.receiveData conn
  pure (MkNewType str)


websocketSever
  :: ( MonadDb env m
     , HasSubs env
     )
  => AuthUser
  -> Connection
  -> m ()
websocketSever user conn = do
  logInfo $ user.name <> " connected"
  connId <- liftIO UUID.nextRandom
  withRunInIO $ \ runInIO -> do
    withPingThread conn 20 (pure ()) $ runInIO $ prefixLogs (show connId) $ do
      computerId <- getComputerId conn
      st <- newIORef (MkSocketState Nothing Nothing computerId)
      result <- try $ forever $ do
        str <- liftIO $ WS.receiveData conn
        case Aeson.eitherDecode' str of
          Left err ->
            sendOut conn (OutParseError err)
          Right (msg :: InMsg) -> do
            logInfoSH msg
            case msg of
              InOpenDoc docId ->
                prefixLogs "OpenDoc" $ handleOpenDoc user (conn, connId) st docId
              InUpdated payload ->
                prefixLogs "Updated" $ handleUpdated conn connId user st payload
              InListenToDoc docId ->
                prefixLogs "ListenToDoc" $ handleListenToDoc user connId st conn docId
              InSaveDoc obj ->
                prefixLogs "SaveDoc" $ handleSave conn user.userId st obj
              InUpdateName txt ->
                prefixLogs "UpdateName" $ handleUpdateName conn st txt
              other -> prefixLogs "other" $
                logInfo $ show other
      case result of
        Right _ -> pure () -- this should never happen
        Left (err :: SomeException) -> do
          logErrorSH err
          mst <- readIORef st
          case mst.openDocument of
            Nothing -> pure ()
            Just previousDocId -> do
              closeDoc connId st previousDocId


handleUpdated
  :: ( MonadDb env m
     , HasSubs env
     )
  => Connection
  -> ConnId
  -> AuthUser
  -> IORef SocketState
  -> InUpdatedPayload
  -> m ()
handleUpdated senderConn senderConnId user rst payload = do
  st <- readIORef rst
  void $ runMaybeT $ do
    docId <- hoistMaybe st.openDocument
    rsubs <- asks (.subs)
    subs <- readIORef rsubs
    rdDocSt <- hoistMaybe (Map.lookup docId subs)
    result <- lift $ withTransaction $ do
      currentVersion <- Doc.getDocVersion docId
      if currentVersion /= payload.version
        then do
          stepsSince <- Doc.getStepsSince docId payload.version
          pure $ Left stepsSince
        else do
          let n = fromIntegral (length payload.steps) :: Int32
          Doc.insertSteps docId currentVersion user.userId (MkNewType payload.clientId) payload.steps
          let newVersion = currentVersion + n
          Doc.updateDocVersion docId newVersion
          pure $ Right newVersion
    case result of
      Left stepsSince -> do
        let conflictSteps     = map (\ (_, s, _) -> s) stepsSince
            conflictClientIds = map (\ (_, _, MkNewType c) -> c) stepsSince
        lift $ sendOut senderConn (OutDocConflict (MkConflictPayload conflictSteps conflictClientIds))
      Right newVersion -> do
        let n = length payload.steps
            stepsPayload = MkStepsPayload newVersion payload.steps (replicate n payload.clientId) docId
        lift $ sendOut senderConn (OutDocConfirmed stepsPayload)
        dsubs <- readIORef rdDocSt.subscriptions
        for_ dsubs $ \ listenerConn ->
          lift $ sendOut listenerConn (OutDocUpdated stepsPayload)
        deditors <- readIORef rdDocSt.editors
        for_ (Map.delete senderConnId deditors) $ \ editorConn ->
          lift $ sendOut editorConn (OutDocUpdated stepsPayload)


handleUpdateName
  :: ( MonadDb env m
     )
  => Connection
  -> IORef SocketState
  -> Text
  -> m ()
handleUpdateName conn rst name =
  void $ runMaybeT $ do
    st <- readIORef rst
    docId <- hoistMaybe st.openDocument
    lift $ Doc.updateDocMeta docId name
    lift $ sendOut conn OutDocNameUpdated
    pure ()



handleSave
  :: ( MonadDb env m
     , HasSubs env
     )
  => Connection
  -> T.UserId
  -> IORef SocketState
  -> SaveDoc
  -> m ()
handleSave conn userId rst doc = do
  logInfo "saving"
  st <- readIORef rst
  void $ runMaybeT $ do
    logDebugSH st
    docId <- hoistMaybe st.openDocument
    updatedTime <- lift $ Doc.updateDocument docId userId st.computerId doc.document
    lift $ Doc.maybeTakeSnapshot docId doc.document
    lift $ sendOut conn (OutDocSaved (MkSavedDoc updatedTime))


handleListenToDoc
  :: ( MonadDb env m
     , HasSubs env
     )
  => AuthUser
  -> ConnId
  -> IORef SocketState
  -> Connection
  -> T.DocId
  -> m ()
handleListenToDoc user connId st conn docId = do
  logInfo $ "user " <> show user.name <> " listening to " <> show docId
  mSideDoc <- (.sideDoc) <$> readIORef st
  for_ mSideDoc $ \ sideDoc -> do
    rsubs <- asks (.subs)
    subs <- readIORef rsubs
    let mdocSt = Map.lookup sideDoc subs
    for_ mdocSt $ \ docSt -> do
      logInfo "closing doc"
      atomicModifyIORef_ docSt.subscriptions (Map.delete connId)
      test <- readIORef docSt.subscriptions
      logInfoSH (Map.keys test)

  mDoc <- Doc.getDocInStudyGroup user docId
  for_ mDoc $ \ doc -> do
    logInfo "found doc"
    rsubs <- asks (.subs)
    subs <- readIORef rsubs
    let mdocSt = Map.lookup docId subs
    docSt <-
      case mdocSt of
        Nothing -> mkDocSt Nothing docId
        Just docSt -> pure docSt
    atomicModifyIORef_ docSt.subscriptions (Map.insert connId conn)
    let currentVersion = doc.version
    mSnap <- Doc.getLatestSnapshotBefore docId currentVersion
    listenPayload <- case mSnap of
      Nothing -> do
        pure $ MkListenStartPayload
          { document         = doc.document
          , version          = currentVersion
          , snapVersion      = currentVersion
          , pendingSteps     = []
          , pendingClientIds = []
          }
      Just (sv, sd) -> do
        stepsSince <- Doc.getStepsSince docId sv
        -- If too many steps to replay, serve the current doc directly.
        if length stepsSince > 50
          then pure $ MkListenStartPayload
            { document         = doc.document
            , version          = currentVersion
            , snapVersion      = currentVersion
            , pendingSteps     = []
            , pendingClientIds = []
            }
          else do
            let pendingStepsVals = map (\ (_, s, _) -> s) stepsSince
                pendingCIds      = map (\ (_, _, MkNewType c) -> c) stepsSince
            pure $ MkListenStartPayload
              { document         = sd
              , version          = currentVersion
              , snapVersion      = sv
              , pendingSteps     = pendingStepsVals
              , pendingClientIds = pendingCIds
              }
    sendOut conn (OutDocListenStart listenPayload)
    atomicModifyIORef_ st (\ st' -> st' & #sideDoc ?~ docId)


mkDocSt
  :: ( MonadIO m
     , MonadReader env m
     , HasSubs env
     )
  => Maybe (Connection, ConnId)
  -> T.DocId
  -> m DocState
mkDocSt openCon docId = do
  subs <- asks (.subs)
  docSubs <- newIORef Map.empty
  let initEditors = maybe Map.empty (\ (c, cId) -> Map.singleton cId c) openCon
  editorsRef <- newIORef initEditors
  let docSt = MkDocState editorsRef docSubs
  atomicModifyIORef_ subs (Map.insert docId docSt)
  pure docSt


closeDoc
  :: ( MonadDb env m
     , HasSubs env
     )
  => ConnId
  -> IORef SocketState
  -> T.DocId
  -> m ()
closeDoc connId rst docId = prefixLogs ("closing doc " <> show docId) $ do
  logDebug "closing"
  rsubs <- asks (.subs)
  subs <- readIORef rsubs
  atomicModifyIORef_ rst (\ st -> st { openDocument = Nothing })
  let mdocSt = Map.lookup docId subs
  case mdocSt of
    Nothing ->
      logDebug "Somehow the doc was never open?"
    Just (docSt :: DocState) -> do
      logDebug "updating the state"
      atomicModifyIORef_ docSt.editors (Map.delete connId)
  pure ()



handleOpenDoc
  :: ( MonadDb env m
     , HasSubs env
     )
  => AuthUser
  -> (Connection, ConnId)
  -> IORef SocketState
  -> T.DocId
  -> m ()
handleOpenDoc user (conn, connId) rst docId = do
  st <- readIORef rst
  case st.openDocument of
    Nothing -> pure ()
    Just previousDocId -> do
      logDebug "already had a doc open closing it now"
      closeDoc connId rst previousDocId
  mDoc <- E.getById @Doc.GetDoc docId
  case mDoc of
    Nothing -> sendOut conn OutNotFound
    Just doc -> do
      if user.userId `notElem` fmap (.userId) doc.editors then
        sendOut conn OutUnauthorized
      else do
        rsubs <- asks (.subs)
        atomicWriteIORef rst (st { openDocument = Just docId })
        mLastUpdate <- Doc.getLastUpdate docId
        currentVersion <- Doc.getDocVersion docId
        mSnap <- Doc.getLatestSnapshotBefore docId currentVersion
        (snapVer, snapDoc, stepsSince) <- case mSnap of
          Just (sv, sd) -> do
            steps <- Doc.getStepsSince docId sv
            -- If there are too many steps to replay, take a fresh snapshot at
            -- the current version so the client opens without pending steps.
            if length steps > 50
              then do
                Doc.insertSnapshotIfAbsent docId currentVersion doc.document
                pure (currentVersion, doc.document, [])
              else pure (sv, sd, steps)
          Nothing -> do
            -- Legacy document: no snapshot yet. Insert one at the current
            -- version so future opens can use the step-based path. The
            -- document column is assumed to be current (kept so by the
            -- historical SaveDoc echo).
            Doc.insertSnapshotIfAbsent docId currentVersion doc.document
            pure (currentVersion, doc.document, [])
        let pendingStepsVals = map (\ (_, s, _) -> s) stepsSince
            pendingClientIds = map (\ (_, _, MkNewType c) -> c) stepsSince
            openPayload = MkDocOpenPayload
              { doc          = doc { Doc.lastUpdate = mLastUpdate, Doc.document = snapDoc }
              , snapVersion  = snapVer
              , pendingSteps = pendingStepsVals
              , pendingClientIds = pendingClientIds
              }
        sendOut conn (OutDocOpened openPayload)
        subs <- readIORef rsubs
        let mdocSt = Map.lookup docId subs
        case mdocSt of
          Nothing -> do
            logDebug "No previous state"
            void $ mkDocSt (Just (conn, connId)) docId
          Just (docSt :: DocState) -> do
            logDebug "Adding editor to existing DocState"
            atomicModifyIORef_ docSt.editors (Map.insert connId conn)



type Api =
  AuthProtect "cookie"
    :> WebSocket


server
  :: ( MonadDb env m
     , HasSubs env
     )
  => ServerT Api m
server =
  websocketSever

