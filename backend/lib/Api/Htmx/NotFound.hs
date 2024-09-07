module Api.Htmx.NotFound where

import Prelude hiding ((**))
import Text.Ginger
import Text.Ginger.Html (htmlSource)
import Api.Htmx.Ginger (readFromTemplates, basicTemplate, baseContext, gvalHelper)
import Web.Scotty.Trans hiding (scottyT)


getNotFound
  :: (MonadIO m, MonadLogger m)
  => ActionT m a
getNotFound = do
  basicTemplate "notFound.html" identity
  finish



handleNotFound
  :: ( MonadIO m
     , MonadLogger m
     )
  => (id -> m (Maybe a))
  -> (id -> ActionT m a)
handleNotFound finding id =
  lift (finding id) >>= \case
    Just a -> pure a
    Nothing -> getNotFound
