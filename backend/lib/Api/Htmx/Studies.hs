module Api.Htmx.Studies where

import Api.Auth (setCookie')
import Api.Htmx.AuthHelper (AuthUser)
import Api.Htmx.Ginger (basicTemplate)
import Data.Aeson qualified as Aeson
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
import Entity.Document qualified as Doc
import Network.HTTP.Types.Status (status200)
import Password (NewPassword, Password, PasswordHash, comparePassword, passwordFromText)
import Text.Ginger
import Text.Ginger.Html (htmlSource)
import Types qualified as T
import Web.Cookie qualified as Cookie
import Web.Scotty.Trans hiding (scottyT)
import Prelude hiding ((**))




getStudies
  :: ( MonadDb env m
     , MonadLogger m
     )
  => AuthUser
  -> ActionT m ()
getStudies user = do
  docs <- lift $ Doc.getAllDocs user
  basicTemplate
    "studies.html"
    ( HMap.insert "user" (toGVal (Aeson.toJSON user))
    . HMap.insert "studies" (toGVal (Aeson.toJSON docs))
    )
