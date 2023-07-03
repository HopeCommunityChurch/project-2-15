module Api.Errors where

import Data.Typeable (tyConName, typeRep, typeRepTyCon)
import Text.Show qualified as Show
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson


class (Exception e, ToJSON e) => ApiException e


data SomeApiException = forall e . (ApiException e) =>
  MkSomeApiException e

instance Show.Show SomeApiException where
  show (MkSomeApiException a) = show a

instance Exception SomeApiException


data NotFound
  = forall e id . (Typeable e, Show id, ToJSON id)
  => MkNotFound (Proxy e) id

instance Show.Show NotFound where
  show (MkNotFound p id) =str
      where
    str = "Can't find " <> modelName <> " with key: " <> show id
    modelName = tyConName $ typeRepTyCon $ typeRep p

instance Exception NotFound

instance ToJSON NotFound where
  toJSON (MkNotFound p id) = Aeson.object
    [ "model" .= Aeson.String (toText modelName)
    , "id" .= id
    ]
      where
    modelName = tyConName $ typeRepTyCon $ typeRep p

instance ApiException NotFound


handleNotFound
  :: forall e id m
   . (Typeable e, ToJSON id, Show id, MonadUnliftIO m)
  => (id -> m (Maybe e))
  -> (id -> m e)
handleNotFound finding id = do
  finding id >>= \case
    Just e ->
      pure e
    Nothing ->
      throwIO (MkNotFound (Proxy @e) id)


data AuthError = AuthError
  deriving (Show, Generic)
  deriving anyclass (Exception, ToJSON, ApiException)

throwAuthErr :: MonadUnliftIO m => m a
throwAuthErr = throwIO AuthError
