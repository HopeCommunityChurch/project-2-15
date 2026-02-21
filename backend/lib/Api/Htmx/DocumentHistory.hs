module Api.Htmx.DocumentHistory where

import Api.Htmx.AuthHelper (AuthUser (..))
import Api.Htmx.Ginger (basicTemplate)
import Api.Htmx.NotFound qualified as NotFound
import Data.HashMap.Strict qualified as HMap
import Data.UUID as UUID
import EnvFields (EnvType (..))
import Data.Aeson qualified as Aeson
import Text.Ginger (toGVal)
import Database.PostgreSQL.Simple qualified as PgS
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import DbHelper (MonadDb, withRawConnection)
import Entity qualified as E
import Entity.Document qualified as Doc
import Entity.User qualified as User
import Types qualified as T
import Web.Scotty.Trans hiding (scottyT)
import Web.Scotty.Trans qualified as Scotty
import Network.HTTP.Types.Status (status403)


data HistoryGroup = MkHistoryGroup
  { startVersion :: Int32
  , endVersion   :: Int32
  , startedAt    :: UTCTime
  , endedAt      :: UTCTime
  , stepCount    :: Int64
  }
  deriving (Generic, Show)
  deriving anyclass (Aeson.ToJSON)

instance FromRow HistoryGroup where
  fromRow = MkHistoryGroup <$> field <*> field <*> field <*> field <*> field


data DocAtVersion = MkDocAtVersion
  { snapshotDoc  :: Aeson.Object
  , steps        :: [Aeson.Value]
  , fromVersion  :: Int32
  , toVersion    :: Int32
  }
  deriving (Generic, Show)
  deriving anyclass (Aeson.ToJSON)


getHistoryGroups
  :: MonadDb env m
  => T.DocId
  -> T.UserId
  -> m [HistoryGroup]
getHistoryGroups docId userId =
  withRawConnection $ \conn ->
    liftIO $ PgS.query conn
      "SELECT \
      \  MIN(version)::integer     AS \"startVersion\", \
      \  MAX(version)::integer     AS \"endVersion\", \
      \  MIN(\"createdAt\")        AS \"startedAt\", \
      \  MAX(\"createdAt\")        AS \"endedAt\", \
      \  COUNT(*)                  AS \"stepCount\" \
      \FROM \"document_step\" \
      \WHERE \"docId\" = ? \
      \  AND \"userId\" = ? \
      \GROUP BY to_timestamp(floor(extract(epoch FROM \"createdAt\") / 300) * 300) \
      \ORDER BY MIN(version) DESC"
      (docId, userId)


getDocAtVersion
  :: MonadDb env m
  => T.DocId
  -> Int32
  -> m DocAtVersion
getDocAtVersion docId targetVersion = do
  mSnap <- Doc.getLatestSnapshot docId
  let (snapVersion, snapDoc) = case mSnap of
        Nothing         -> (0, mempty)
        Just (v, d)     -> (v, d)
  rawSteps <- Doc.getStepsSince docId snapVersion
  let filtered = [ s | (v, s, _) <- rawSteps, v <= targetVersion ]
  pure $ MkDocAtVersion snapDoc filtered snapVersion targetVersion


getHistoryApi
  :: ( MonadDb env m
     , MonadLogger m
     )
  => AuthUser
  -> ActionT m ()
getHistoryApi user = do
  docId <- captureParam "documentId"
  doc   <- NotFound.handleNotFound (E.getByIdForUser @Doc.GetDoc user) docId
  unless (user.userId `elem` fmap (.userId) doc.editors) $ do
    status status403
    Scotty.text "Forbidden"
    finish
  groups <- lift $ getHistoryGroups docId user.userId
  json groups


getVersionApi
  :: ( MonadDb env m
     , MonadLogger m
     )
  => AuthUser
  -> ActionT m ()
getVersionApi user = do
  docId         <- captureParam "documentId"
  targetVersion <- captureParam "versionNum"
  doc           <- NotFound.handleNotFound (E.getByIdForUser @Doc.GetDoc user) docId
  unless (user.userId `elem` fmap (.userId) doc.editors) $ do
    status status403
    Scotty.text "Forbidden"
    finish
  result <- lift $ getDocAtVersion docId targetVersion
  json result


getHistoryPage
  :: ( MonadDb env m
     , MonadLogger m
     )
  => AuthUser
  -> ActionT m ()
getHistoryPage user = do
  docId <- captureParam "documentId"
  doc   <- NotFound.handleNotFound (E.getByIdForUser @Doc.GetDoc user) docId
  unless (user.userId `elem` fmap (.userId) doc.editors) $ do
    status status403
    Scotty.text "Forbidden"
    finish
  mHost <- header "X-Forwarded-Host"
  let host = fromMaybe "local.p215.church" mHost
  envType <- lift (asks (.envType))
  let isLocal = envType == Dev "local"
  basicTemplate
    "study-history.html"
    ( HMap.insert "isLocal" (toGVal isLocal)
    . HMap.insert "docId" (toGVal (UUID.toText (unwrap docId)))
    . HMap.insert "docName" (toGVal doc.name)
    . HMap.insert "host" (toGVal host)
    )
