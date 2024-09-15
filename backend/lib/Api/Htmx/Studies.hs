module Api.Htmx.Studies where

import Api.Auth (setCookie')
import Api.Htmx.AuthHelper (AuthUser)
import Api.Htmx.Ginger (basicTemplate)
import Data.Aeson qualified as Aeson
import Data.CaseInsensitive (original)
import Data.HashMap.Strict qualified as HMap
import Data.List (nubBy)
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
import Entity.Shares qualified as Shares
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
  qParams <- queryParams
  let mShareToken = qParams
                    & find (\(key, _) -> key == "share_token")
                    & fmap (T.mkShareToken . toStrict . snd)
  logInfoSH mShareToken
  mShareData <- lift $ forM mShareToken Shares.getShareFromToken
  logInfoSH mShareData
  shares <- lift $ Shares.getSharesForUser user
  let allShares = (shares <> maybeToList (join mShareData))
                    & nubBy (\ a b -> a.token == b.token)
  basicTemplate
    "studies.html"
    ( HMap.insert "user" (toGVal (Aeson.toJSON user))
    . HMap.insert "studies" (toGVal (Aeson.toJSON docs))
    . HMap.insert "shares" (toGVal (Aeson.toJSON allShares))
    )
