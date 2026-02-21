module Api.Htmx.Studies where

import Api.Htmx.AuthHelper (AuthUser)
import Api.Htmx.Ginger (basicTemplate)
import Data.Aeson qualified as Aeson
import Data.HashMap.Strict qualified as HMap
import Data.List (nubBy)
import DbHelper (MonadDb)
import Entity.Document qualified as Doc
import Entity.Shares qualified as Shares
import Text.Ginger
import Types qualified as T
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
                    & fmap (T.mkShareToken . snd)
  logInfoSH mShareToken
  mShareData <- lift $ forM mShareToken Shares.getShareFromToken
  logInfoSH mShareData
  shares <- lift $ Shares.getSharesForUser user
  let allShares = (shares <> maybeToList (join mShareData))
                    & nubBy (\ a b -> a.token == b.token)
                    & sortOn (.created)
  basicTemplate
    "studies.html"
    ( HMap.insert "user" (toGVal (Aeson.toJSON user))
    . HMap.insert "studies" (toGVal (Aeson.toJSON docs))
    . HMap.insert "shares" (toGVal (Aeson.toJSON allShares))
    )
