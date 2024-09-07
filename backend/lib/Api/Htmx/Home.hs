module Api.Htmx.Home where

import Prelude hiding ((**))
import Api.Htmx.Ginger (basicTemplate)
import Web.Scotty.Trans hiding (scottyT)
import Api.Htmx.AuthHelper (getUser)
import DbHelper (MonadDb)



getHome
  :: (MonadDb env m, MonadLogger m)
  => ActionT m ()
getHome = do
  user <- getUser
  logInfoSH user
  basicTemplate
    "home.html"
    identity


