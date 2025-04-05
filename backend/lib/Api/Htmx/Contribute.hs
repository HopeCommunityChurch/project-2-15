module Api.Htmx.Contribute where

import Prelude hiding ((**))
import Api.Htmx.Ginger (basicTemplate)
import Web.Scotty.Trans hiding (scottyT)
import Api.Htmx.AuthHelper (getUser)
import DbHelper (MonadDb)



getContribute
  :: (MonadDb env m, MonadLogger m)
  => ActionT m ()
getContribute = do
  user <- getUser
  logInfoSH user
  basicTemplate
    "contribute.html"
    identity


