module Api.Htmx.Ginger where

import Data.Default (def)
import Data.HashMap.Strict qualified as HMap
import Relude (readFile)
import System.Directory (doesFileExist)
import Text.Ginger
import Text.Ginger.Html (htmlSource, Html)
import Web.Scotty.Trans (ActionT, html)
import Prelude hiding ((**))
import Control.Monad.Writer (Writer)


readFromTemplates
  :: ( MonadIO m
     , MonadLogger m
     )
  => SourceName
  -> m (Either ParserError (Template SourcePos))
readFromTemplates sn = do
  parseGingerFile getFileFromTemplatesFolder sn


templatesFolder :: String
templatesFolder = "./templates/"


getFileFromTemplatesFolder
  :: ( MonadIO m
     , MonadLogger m
     )
  => SourceName
  -> m (Maybe Source)
getFileFromTemplatesFolder sn = do
  let path = templatesFolder <> sn
  ex <- liftIO $ doesFileExist path
  if ex
    then Just <$> readFile path
    else do
      logError $ "Can't find file: " <> toText path
      pure Nothing


baseUrl :: IsString s => s
baseUrl = ""

baseContext :: HashMap Text (GVal m)
baseContext = fromList [("base", baseUrl)]


gvalHelper :: HashMap Text (GVal m) -> Text -> GVal m
gvalHelper ctx varName =
  fromMaybe def $ HMap.lookup varName ctx



basicTemplate
  :: ( MonadIO m
     , MonadLogger m
     )
  => SourceName
  -> (HashMap Text (GVal (Run SourcePos (Writer Html) Html))
      -> HashMap Text (GVal (Run SourcePos (Writer Html) Html))
     )
  -> ActionT m ()
basicTemplate sn contextMap = do
  eTemplate <- readFromTemplates sn
  case eTemplate of
    Left err -> html (show err)
    Right template ->
      let context = contextMap baseContext
          content = makeContextHtml (gvalHelper context)
          h = runGinger content template
       in html $ toLazy (htmlSource h)

