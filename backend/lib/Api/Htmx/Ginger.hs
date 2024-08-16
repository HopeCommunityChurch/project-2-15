module Api.Htmx.Ginger where

import Prelude hiding ((**))
import Text.Ginger
import Relude (readFile)
import System.Directory (doesFileExist)
import Data.Default (def)
import Data.HashMap.Strict qualified as HMap


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



