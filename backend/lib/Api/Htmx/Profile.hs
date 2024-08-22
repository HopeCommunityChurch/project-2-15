module Api.Htmx.Profile where

import Api.Auth (setCookie')
import Api.Htmx.AuthHelper (AuthUser (..), getUserWithRedirect)
import Api.Htmx.Ginger (baseContext, baseUrl, gvalHelper, readFromTemplates)
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
import Entity.User qualified as User
import Network.HTTP.Types.Status (status200)
import Password (NewPassword, Password, PasswordHash, comparePassword, passwordFromText)
import Text.Ginger
import Text.Ginger.Html (htmlSource)
import Types qualified as T
import Web.Cookie qualified as Cookie
import Web.Scotty.Trans hiding (scottyT)
import Prelude hiding ((**))




getProfile
  :: ( MonadDb env m
     , MonadLogger m
     )
  => AuthUser
  -> ActionT m ()
getProfile user = do
  result <- readFromTemplates "profile.html"
  case result of
    Right template -> do
      let context = baseContext
                    & HMap.insert "user" (toGVal (Aeson.toJSON user))
      let content = makeContextHtml (gvalHelper context)
      let h = runGinger content template
      html $ toLazy (htmlSource h)
    Left err -> html (show err)


putProfile
  :: ( MonadDb env m
     , MonadLogger m
     )
  => AuthUser
  -> ActionT m ()
putProfile user = do
  result <- readFromTemplates "profile/form.html"
  email <- formParam "email"
  name <- formParam "name"
  let up = User.MkUpdateUser
                email
                name
  lift $ logInfoSH up
  lift $ User.updateUser user.userId up
  user' <- getUserWithRedirect
  case result of
    Right template -> do
      let context = baseContext
                    & HMap.insert "user" (toGVal (Aeson.toJSON user'))
      let content = makeContextHtml (gvalHelper context)
      let h = runGinger content template
      html $ toLazy (htmlSource h)
    Left err -> html (show err)
