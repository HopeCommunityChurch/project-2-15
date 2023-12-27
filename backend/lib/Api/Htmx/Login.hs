module Api.Htmx.Login where

import Prelude hiding ((**))
import Text.Ginger
import Text.Ginger.Html (htmlSource)
import Api.Htmx.Ginger (readFromTemplates)
import Web.Scotty.Trans hiding (scottyT)
import Data.HashMap.Strict qualified as HMap




sampleContext :: HashMap Text Text
sampleContext = fromList [("name", "Alice")]

getLogin
  :: MonadIO m
  => ScottyError e
  => ActionT e m ()
getLogin = do
  result <- readFromTemplates "login.html"
  case result of
    Right template -> do
      let content = makeContextHtml (toGVal . flip HMap.lookup sampleContext)
      let h = runGinger content template
      html $ toLazy (htmlSource h)
    Left err -> html (show err)


