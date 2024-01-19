module Api.Htmx.Studies where

import Data.Aeson qualified as Aeson
import Api.Auth (setCookie')
import Api.Htmx.Ginger (baseContext, baseUrl, gvalHelper, readFromTemplates)
import Data.CaseInsensitive (original)
import Data.HashMap.Strict qualified as HMap
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
import Network.HTTP.Types.Status (status200)
import Password (NewPassword, Password, PasswordHash, comparePassword, passwordFromText)
import Text.Ginger
import Text.Ginger.Html (htmlSource)
import Types qualified as T
import Web.Cookie qualified as Cookie
import Web.Scotty.Trans hiding (scottyT)
import Prelude hiding ((**))
import Api.Htmx.AuthHelper (AuthUser)




getStudies
  :: ( MonadIO m
     , MonadLogger m
     )
  => ScottyError e
  => AuthUser
  -> ActionT e m ()
getStudies user = do
  result <- readFromTemplates "studies.html"
  case result of
    Right template -> do
      let context = baseContext
                    & HMap.insert "user" (toGVal (Aeson.toJSON user))
      let content = makeContextHtml (gvalHelper context)
      let h = runGinger content template
      html $ toLazy (htmlSource h)
    Left err -> html (show err)
