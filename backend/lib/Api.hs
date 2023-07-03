module Api where

import Api.Auth qualified
import Api.Errors qualified as Errs
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.OpenApi qualified as OpenApi
import Data.Typeable (tyConName, typeRep, typeRepTyCon)
import DbHelper (HasDbConn, MonadDb)
import EnvFields (HasEnvType)
import Servant
import Servant.OpenApi (toOpenApi)
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)
import Servant.Swagger.UI.ReDoc qualified as ReDoc
import SwaggerHelpers (OpenApiTag)


type Api'
  = OpenApiTag "auth" "auth stuff"
    :> "auth" :> Api.Auth.Api


server' :: MonadDb env m => ServerT Api' m
server'
  = Api.Auth.server

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


type MyContext = (  '[])

serverContext
  :: Context MyContext
serverContext = EmptyContext


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
