module Api.Htmx.NotFound where

import Prelude hiding ((**))
import Text.Ginger
import Text.Ginger.Html (htmlSource)
import Api.Htmx.Ginger (readFromTemplates, baseContext, gvalHelper)
import Web.Scotty.Trans hiding (scottyT)


getHome
  :: (MonadIO m, MonadLogger m)
  => ScottyError e
  => ActionT e m ()
getHome = do
  result <- readFromTemplates "notFound.html"
  case result of
    Right template -> do
      let content = makeContextHtml (gvalHelper baseContext)
      let h = runGinger content template
      html $ toLazy (htmlSource h)
    Left err -> html (show err)



