module Api.Websocket where


import Api.Auth (AuthUser(..))
import Api.Errors qualified as Errs
import Types qualified as T
import Bible.Esv.Parser qualified as ESV
import DbHelper (MonadDb)
import GHC.Records (HasField)
import Network.WebSockets (Connection, withPingThread)
import Network.WebSockets qualified as WS
import Network.Wreq qualified as Wreq
import Entity qualified as E
import Servant
import Text.Parsec qualified as Parsec
import Data.Map qualified as Map
import Data.Aeson qualified as Aeson
import WebsocketServant
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Entity.Document qualified as Doc
import Entity.User qualified as User
import Relude (atomicModifyIORef_)


type ConnId = UUID

type DocumentSubs = IORef (Map ConnId Connection)

type Subs = IORef (Map T.DocId DocumentSubs)

type HasSubs env = HasField "subs" env Subs


mkSubs :: MonadIO m => m Subs
mkSubs = newIORef Map.empty


data InMsg
  = InOpenDoc T.DocId -- Call before you send updates
  | InCloseDoc T.DocId -- Call when done sending updates
  | InUpdated Aeson.Object -- Updating the doc
  | InListenToDoc T.DocId  -- Call when you want to listen to the updates for a doc
  | InStopListenToDoc T.DocId
  | OpenStudyChat T.GroupStudyId -- Not implemented yet
  | CloseStudyChat T.GroupStudyId -- Not implemented yet
  deriving (Show, Generic)
  deriving anyclass (FromJSON)


data OutMsg
  = OutDocClosed T.DocId -- sent when the other person closes the websocket connection
  | OutDocUpdated Aeson.Object
  | OutDocListenStart Aeson.Object
  | OutUnauthorized
  | OutNotFound
  | OutParseError String
  deriving (Show, Generic)
  deriving anyclass (ToJSON)

sendOut :: MonadIO m => Connection -> OutMsg -> m ()
sendOut conn msg =
  liftIO $ WS.sendTextData conn (Aeson.encode msg)

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
    withPingThread conn 20 (pure ()) $ runInIO $ forever $ do
      str <- liftIO $ WS.receiveData conn
      case Aeson.eitherDecode' str of
        Left err ->
          sendOut conn (OutParseError err)
        Right (msg :: InMsg) ->
          case msg of
            InOpenDoc docId -> handleOpenDoc user conn st docId
            InUpdated obj -> handleUpdated st obj
            InListenToDoc docId -> handleListenToDoc user connId conn docId
            other ->
              logInfo $ show other


handleUpdated
  :: ( MonadDb env m
     , HasSubs env
     )
  => IORef SocketState
  -> Aeson.Object
  -> m ()
handleUpdated rst obj = do
  st <- readIORef rst
  void $ runMaybeT $ do
    docId <- hoistMaybe st.openDocument
    rsubs <- asks (.subs)
    subs <- readIORef rsubs
    rdSubs <- hoistMaybe (Map.lookup docId subs)
    dsubs <- readIORef rdSubs
    for_ dsubs $ \ conn ->
      sendOut conn (OutDocUpdated obj)


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
  mDoc <- E.getByIdForUser @Doc.GetDoc user docId
  for_ mDoc $ \ doc -> do
    rsubs <- asks (.subs)
    subs <- readIORef rsubs
    let mRDSubs = Map.lookup docId subs
    rdsubs <-
      case mRDSubs of
        Nothing -> mkDocSubs docId
        Just rdsubs -> pure rdsubs
    atomicModifyIORef_ rdsubs (Map.insert connId conn)
    sendOut conn (OutDocListenStart doc.document)


mkDocSubs
  :: ( MonadIO m
     , MonadReader env m
     , HasSubs env
     )
  => T.DocId
  -> m (IORef (Map ConnId Connection))
mkDocSubs docId = do
  subs <- asks (.subs)
  docSubs <- newIORef Map.empty
  atomicModifyIORef_ subs (Map.insert docId docSubs)
  pure docSubs


handleOpenDoc
  :: ( MonadDb env m
     , HasSubs env
     )
  => AuthUser
  -> Connection
  -> IORef SocketState
  -> T.DocId
  -> m ()
handleOpenDoc user conn rst docId = do
  st <- readIORef rst
  case st.openDocument of
    Nothing -> pure ()
    Just docId -> -- todo: call close, then continue
      pure ()
  mDoc <- E.getById @Doc.GetDoc docId
  case mDoc of
    Nothing -> sendOut conn OutNotFound
    Just doc -> do
      if user.userId `notElem` fmap (.userId) doc.editors then
        sendOut conn OutUnauthorized
      else do
        atomicWriteIORef rst (st { openDocument = Just docId })


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

