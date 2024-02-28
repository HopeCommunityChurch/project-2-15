module Api.Websocket where


import Api.Auth (AuthUser (..))
import Api.Errors qualified as Errs
import Bible.Esv.Parser qualified as ESV
import Control.Monad.Logger.Prefix (prefixLogs)
import Data.Aeson qualified as Aeson
import Data.Char (toLower)
import Data.Map qualified as Map
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import DbHelper (MonadDb)
import Entity qualified as E
import Entity.Document qualified as Doc
import Entity.User qualified as User
import GHC.Records (HasField)
import Network.WebSockets (Connection, withPingThread)
import Network.WebSockets qualified as WS
import Network.Wreq qualified as Wreq
import Relude (atomicModifyIORef_, drop)
import Servant
import Text.Parsec qualified as Parsec
import Types qualified as T
import WebsocketServant


type ConnId = UUID

type DocumentSubs = IORef (Map ConnId Connection)

data DocState = MkDocState
  { openedBy :: IORef (Maybe (Connection, ConnId))
  , subscriptions :: DocumentSubs
  }
  deriving (Generic)

type Subs = IORef (Map T.DocId DocState)

type HasSubs env = HasField "subs" env Subs


mkSubs :: MonadIO m => m Subs
mkSubs = newIORef Map.empty


data InMsg
  = InOpenDoc T.DocId -- Call before you send updates and saves
  | InCloseDoc T.DocId -- Call when done sending updates
  | InUpdated Aeson.Value -- Updating the doc
  | InSaveDoc Aeson.Object
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


data OutMsg
  = OutDocUpdated Aeson.Value
  | OutDocOpened Aeson.Value
  | OutDocOpenedOther -- when the doc is opened on another device, you should close your document
  | OutDocListenStart Aeson.Object
  | OutDocSaved
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
  }
  deriving (Show, Generic)


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
  st <- newIORef (MkSocketState Nothing)
  withRunInIO $ \ runInIO -> do
    withPingThread conn 20 (pure ()) $ runInIO $ prefixLogs (show connId) $ do
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
              InUpdated obj ->
                prefixLogs "Updated" $ handleUpdated st obj
              InListenToDoc docId ->
                prefixLogs "ListenToDoc" $ handleListenToDoc user connId conn docId
              InSaveDoc obj ->
                prefixLogs "SaveDoc" $ handleSave conn st obj
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
  => IORef SocketState
  -> Aeson.Value
  -> m ()
handleUpdated rst obj = do
  st <- readIORef rst
  void $ runMaybeT $ do
    docId <- hoistMaybe st.openDocument
    rsubs <- asks (.subs)
    subs <- readIORef rsubs
    rdDocSt <- hoistMaybe (Map.lookup docId subs)
    dsubs <- readIORef rdDocSt.subscriptions
    for_ dsubs $ \ conn ->
      lift $ sendOut conn (OutDocUpdated obj)


handleSave
  :: ( MonadDb env m
     , HasSubs env
     )
  => Connection
  -> IORef SocketState
  -> Aeson.Object
  -> m ()
handleSave conn rst obj = do
  logInfo "saving"
  st <- readIORef rst
  void $ runMaybeT $ do
    logDebugSH st
    docId <- hoistMaybe st.openDocument
    lift $ Doc.updateDocument docId obj
    lift $ sendOut conn OutDocSaved


handleListenToDoc
  :: ( MonadDb env m
     , HasSubs env
     )
  => AuthUser
  -> ConnId
  -> Connection
  -> T.DocId
  -> m ()
handleListenToDoc user connId conn docId = do
  logInfo $ "user " <> show user.name <> " listening to " <> show docId
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
    sendOut conn (OutDocListenStart doc.document)


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
  openDoc <- newIORef openCon
  let docSt = MkDocState openDoc docSubs
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
      atomicModifyIORef_
        docSt.openedBy
        (\case
          Just (_, cId) | connId == cId -> Nothing
          a -> a
        )
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
        sendOut conn (OutDocOpened (Aeson.Object doc.document))
        subs <- readIORef rsubs
        let mdocSt = Map.lookup docId subs
        case mdocSt of
          Nothing -> do
            logDebug "No previous state"
            void $ mkDocSt (Just (conn, connId)) docId
          Just (docSt :: DocState) -> do
            logDebug "Previous state"
            mOpenedBy <- readIORef docSt.openedBy
            case mOpenedBy of
              Nothing -> do
                logDebug "Doc not previously open"
              Just (openConn, _) -> do
                logDebug "Telling someone to close"
                sendOut openConn OutDocOpenedOther
            atomicWriteIORef docSt.openedBy (Just (conn, connId))



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

