module WebsocketServant where

import           Control.Lens                        (at)
import           Control.Monad.Trans.Resource        (runResourceT)
import           Data.OpenApi.Lens                   (paths)
import           Network.Wai.Handler.WebSockets      (websocketsOr)
import           Network.WebSockets
    ( Connection
    , acceptRequest
    , defaultConnectionOptions
    )
import           Servant.Server
    ( HasServer(..)
    , ServerError(..)
    , ServerT
    , runHandler
    )
import           Servant.Server.Internal.Delayed     (runDelayed)
import           Servant.Server.Internal.Router      (leafRouter)
import           Servant.Server.Internal.RouteResult (RouteResult(..))
import           Servant.OpenApi                          (HasOpenApi(..))


-- Taken from: https://hackage.haskell.org/package/servant-websockets-2.0.0/docs/src/Servant.API.WebSocket.html#WebSocket

-- | Endpoint for defining a route to provide a web socket. The
-- handler function gets an already negotiated websocket 'Connection'
-- to send and receive data.
--
-- Example:
--
-- > type WebSocketApi = "stream" :> WebSocket
-- >
-- > server :: Server WebSocketApi
-- > server = streamData
-- >  where
-- >   streamData :: MonadIO m => Connection -> m ()
-- >   streamData c = do
-- >     liftIO $ forkPingThread c 10
-- >     liftIO . forM_ [1..] $ \i -> do
-- >        sendTextData c (pack $ show (i :: Int)) >> threadDelay 1000000
data WebSocket


instance HasOpenApi WebSocket where
  toOpenApi _ = mempty & paths . at "/" ?~ mempty


instance HasServer WebSocket ctx where

  type ServerT WebSocket m = Connection -> m ()

  hoistServerWithContext _ _ nat svr = nat . svr

  route Proxy _ app = leafRouter $ \env request respond -> runResourceT $
    runDelayed app env request >>= liftIO . go request respond
   where
    go request respond (Route app') =
      websocketsOr defaultConnectionOptions (runApp app') (backupApp respond) request (respond . Route)
    go _ respond (Fail e) = respond $ Fail e
    go _ respond (FailFatal e) = respond $ FailFatal e

    runApp a = acceptRequest >=> \c -> void (runHandler $ a c)

    backupApp respond _ _ = respond $ Fail ServerError { errHTTPCode = 426
                                                       , errReasonPhrase = "Upgrade Required"
                                                       , errBody = mempty
                                                       , errHeaders = mempty
                                                       }

