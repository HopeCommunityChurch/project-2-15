module Api.Htmx.NotAuthorized where

import Prelude hiding ((**))
import Text.Ginger
import Text.Ginger.Html (htmlSource)
import Api.Htmx.Ginger (readFromTemplates, baseContext, gvalHelper)
import Web.Scotty.Trans hiding (scottyT)


getHome
  :: (MonadIO m, MonadLogger m)
  => ActionT m a
getHome = do
  result <- readFromTemplates "notAuthorized.html"
  case result of
    Right template -> do
      let content = makeContextHtml (gvalHelper baseContext)
      let h = runGinger content template
      html $ toLazy (htmlSource h)
      finish
    Left err ->  do
      html (show err)
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
    Nothing -> getHome
