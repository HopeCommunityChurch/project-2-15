module Api where

import Altcha qualified
import Api.Altcha qualified
import Api.Bible qualified
import Api.Errors qualified as Errs

-- import Api.Htmx.Studies qualified

import Api.Auth qualified
import Api.Websocket qualified
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.OpenApi qualified as OpenApi
import Data.Typeable (tyConName, typeRep, typeRepTyCon)
import DbHelper (HasDbConn, MonadDb)
import Entity.AuthUser (AuthUser)
import EnvFields (HasEnvType, HasUrl)
import Mail qualified
import Network.Wai (Request)
import Servant
import Servant.OpenApi (toOpenApi)
import Servant.Server.Experimental.Auth (
  AuthHandler,
 )
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)
import Servant.Swagger.UI.ReDoc qualified as ReDoc
import SwaggerHelpers (OpenApiTag)
import Types qualified as T



type Api'
  = OpenApiTag "websocket" "websocket"
    :> "document" :> "realtime" :> Api.Websocket.Api
  :<|> OpenApiTag "bible" "bible stuff"
    :> "bible" :> Api.Bible.Api
  :<|> OpenApiTag "altcha" "altcha stuff"
    :> "altcha" :> Api.Altcha.Api
  -- :<|> OpenApiTag "htmx studies" "htmx studies"
  --   :> "htmx" :> "studies" :> Api.Htmx.Studies.Api


server'
  :: ( MonadDb env m
     , Mail.HasSmtp env
     , HasUrl env
     , Api.Websocket.HasSubs env
     , Altcha.HasAltchaKey env
     )
  => Api.Bible.HasESVEnv env
  => ServerT Api' m
server'
  = Api.Websocket.server
  :<|> Api.Bible.server
  :<|> Api.Altcha.server
  -- :<|> Api.Htmx.Studies.server


errToJSON :: Errs.SomeApiException -> ServerError
errToJSON (Errs.MkSomeApiException (e :: errType)) =
  ServerError
    420
    "Too much smoke"
    msg
    []
  where
    msg = Aeson.encode $ Aeson.object
      [ "error" .= Aeson.String modelName
      , "content" .= Aeson.toJSON e
      ]
    modelName = toText $ tyConName $ typeRepTyCon $ typeRep (Proxy @errType)


catchErrors :: IO a -> IO (Either ServerError a)
catchErrors a =
  handle
    (\(e :: Errs.SomeApiException) ->
      pure $ Left (errToJSON e)
    )
  $ fmap Right a


toHandler
  :: HasDbConn env
  => env
  -> ( ReaderT env (LoggingT IO) a -> Handler a)
toHandler env myMonad =
  liftIO (catchErrors (runStdoutLoggingT (runReaderT myMonad env))) >>= \case
    Right a -> pure a
    Left err -> throwError err


type MyContext =
  '[  AuthHandler Request AuthUser
   ,  AuthHandler Request (AuthUser, T.CookieToken)
  ]


serverContext
  :: HasDbConn env
  => HasEnvType env
  => env
  -> Context MyContext
serverContext env =
  Api.Auth.authCookie env
  :. Api.Auth.authCookieToken env
  :. EmptyContext


type Api
  = Api'
  :<|> SwaggerSchemaUI "swagger-ui" "swagger.json"
  :<|> ReDoc.SwaggerSchemaUI "swagger-ui2" "swagger2.json"


openApi :: OpenApi.OpenApi
openApi =
  toOpenApi (Proxy @Api')
  & OpenApi.info . OpenApi.title   .~ "Project 2:15"
  & OpenApi.info . OpenApi.version   .~ "0.1"
  & OpenApi.info . OpenApi.description   ?~ "Yay!"


server
  :: HasDbConn env
  => HasEnvType env
  => HasUrl env
  => Api.Bible.HasESVEnv env
  => Mail.HasSmtp env
  => Api.Websocket.HasSubs env
  => Altcha.HasAltchaKey env
  => env
  -> Server Api
server env =
  hoistServerWithContext
    (Proxy @Api')
    (Proxy @MyContext)
    (toHandler env)
    server'
  :<|> swaggerSchemaUIServer openApi
  :<|> ReDoc.redocSchemaUIServer openApi
