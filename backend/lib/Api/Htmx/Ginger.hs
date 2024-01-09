module Api.Htmx.Ginger where

import Prelude hiding ((**))
import Text.Ginger
import Relude (readFile)
import System.Directory (doesFileExist)


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


-- urlBase :: Text
-- urlBase = "/api/htmx"


-- type Url = Text

-- jsFileBs :: ByteString
-- jsFileBs =
--   $(makeRelativeToLocationPredicate (const True) "LocalTime.js" >>= embedFile)





