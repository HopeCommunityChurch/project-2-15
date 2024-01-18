module Api.Htmx.Home where

import Prelude hiding ((**))
import Text.Ginger
import Text.Ginger.Html (htmlSource)
import Api.Htmx.Ginger (readFromTemplates, baseContext, gvalHelper)
import Web.Scotty.Trans hiding (scottyT)
import Api.Htmx.AuthHelper (getUser)
import DbHelper (MonadDb)



getHome
  :: (MonadDb env m, MonadLogger m)
  => ScottyError e
  => ActionT e m ()
getHome = do
  user <- getUser
  logInfoSH user
  result <- readFromTemplates "home.html"
  case result of
    Right template -> do
      let content = makeContextHtml (gvalHelper baseContext)
      let h = runGinger content template
      html $ toLazy (htmlSource h)
    Left err -> html (show err)


