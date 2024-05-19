module Api.Htmx.Study where

import Api.Auth (setCookie')
import Api.Htmx.AuthHelper (AuthUser)
import Api.Htmx.Ginger (baseContext, baseUrl, gvalHelper, readFromTemplates)
import Data.Aeson qualified as Aeson
import Data.CaseInsensitive (original)
import Data.HashMap.Strict qualified as HMap
import Data.Text qualified as Txt
import Database qualified as Db
import Database.Beam (
  all_,
  guard_,
  insert,
  insertValues,
  runInsert,
  runSelectReturningOne,
  runUpdate,
  select,
  update,
  val_,
  (<-.),
  (==.),
  (>=.),
 )
import DbHelper (HasDbConn, MonadDb, runBeam, withTransaction)
import Entity qualified as E
import Entity.Document qualified as Doc
import EnvFields (EnvType (..))
import Network.HTTP.Types.Status (status200)
import Password (NewPassword, Password, PasswordHash, comparePassword, passwordFromText)
import Text.Ginger
import Text.Ginger.Html (htmlSource)
import Types qualified as T
import Web.Cookie qualified as Cookie
import Web.Scotty.Trans hiding (scottyT)
import Prelude hiding ((**))
import Api.Htmx.NotFound qualified as NotFound


data SystemType = Linux | Mac | Windows | Unknown
  deriving (Show, Eq, Ord, Read)

contains :: Text -> Text -> Bool
contains needle haystack =
  let (_, s) = Txt.breakOn needle haystack
   in not (Txt.null s)


getSystem :: Maybe Text -> SystemType
getSystem Nothing = Unknown
getSystem (Just str)
  | contains "Mac" str = Mac
  | contains "Linux" str = Linux
  | contains "Windows" str = Windows
  | otherwise = Unknown


modifierKey :: SystemType -> Text
modifierKey Mac = "⌘"
modifierKey _ = "ctrl"


getStudy
  :: ( MonadDb env m
     , MonadLogger m
     )
  => ScottyError e
  => AuthUser
  -> ActionT e m ()
getStudy user = do
  mUserAgent <- header "User-Agent"
  docId <- param "documentId"
  mDoc <- lift $ E.getByIdForUser @Doc.GetDoc user docId
  case mDoc of
    Nothing -> NotFound.getHome
    Just doc -> do
      let modKey = modifierKey $ getSystem (fmap toStrict mUserAgent)
      result <- readFromTemplates "study.html"
      env <- asks (.envType)
      case result of
        Right template -> do
          let context = baseContext
                        & HMap.insert "env" (toGVal (Aeson.toJSON env))
                        & HMap.insert "user" (toGVal (Aeson.toJSON user))
                        & HMap.insert "modkey" (toGVal modKey)
                        & HMap.insert "docname" (toGVal doc.name)
          let content = makeContextHtml (gvalHelper context)
          let h = runGinger content template
          html $ toLazy (htmlSource h)
        Left err -> html (show err)
