module Api.Websocket where


import Api.Auth (AuthUser (..))
import Api.Errors qualified as Errs
import Bible.Esv.Parser qualified as ESV
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

type Subs = IORef (Map T.DocId DocumentSubs)

type HasSubs env = HasField "subs" env Subs


mkSubs :: MonadIO m => m Subs
mkSubs = newIORef Map.empty


data InMsg
  = InOpenDoc T.DocId -- Call before you send updates
  | InCloseDoc T.DocId -- Call when done sending updates
  | InUpdated Aeson.Value -- Updating the doc
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
  = OutDocClosed T.DocId -- sent when the other person closes the websocket connection
  | OutDocUpdated Aeson.Value
  | OutDocListenStart Aeson.Object
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
    withPingThread conn 20 (pure ()) $ runInIO $ forever $ do
      str <- liftIO $ WS.receiveData conn
      case Aeson.eitherDecode' str of
        Left err ->
          sendOut conn (OutParseError err)
        Right (msg :: InMsg) -> do
          logInfoSH msg
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
  -> Aeson.Value
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
      lift $ sendOut conn (OutDocUpdated obj)


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
    let mRDSubs = Map.lookup docId subs
    rdsubs <-
      case mRDSubs of
        Nothing -> mkDocSubs docId
        Just rdsubs -> pure rdsubs
    atomicModifyIORef_ rdsubs (Map.insert connId conn)
    logInfo "test"
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

