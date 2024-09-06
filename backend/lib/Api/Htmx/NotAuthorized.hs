module Api.Htmx.NotAuthorized where

import Prelude hiding ((**))
import Api.Htmx.Ginger (basicTemplate)
import Web.Scotty.Trans hiding (scottyT)


getNotAuth
  :: (MonadIO m, MonadLogger m)
  => ActionT m a
getNotAuth = do
  basicTemplate "notAuthorized.html" identity
  finish



