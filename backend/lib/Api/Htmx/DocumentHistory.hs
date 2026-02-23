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


type SubHistoryGroup = HistoryGroup


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
      "WITH gaps AS ( \
      \  SELECT \
      \    version, \
      \    \"createdAt\", \
      \    CASE \
      \      WHEN extract(epoch FROM (\"createdAt\" - LAG(\"createdAt\") OVER (ORDER BY \"createdAt\"))) > 1800 \
      \      THEN 1 ELSE 0 \
      \    END AS is_new_session \
      \  FROM \"document_step\" \
      \  WHERE \"docId\" = ? AND \"userId\" = ? \
      \), \
      \sessions AS ( \
      \  SELECT \
      \    version, \
      \    \"createdAt\", \
      \    SUM(is_new_session) OVER (ORDER BY \"createdAt\") AS session_id \
      \  FROM gaps \
      \) \
      \SELECT \
      \  MIN(version)::integer  AS \"startVersion\", \
      \  MAX(version)::integer  AS \"endVersion\", \
      \  MIN(\"createdAt\")     AS \"startedAt\", \
      \  MAX(\"createdAt\")     AS \"endedAt\", \
      \  COUNT(*)               AS \"stepCount\" \
      \FROM sessions \
      \GROUP BY session_id \
      \ORDER BY MIN(version) DESC"
      (docId, userId)


-- | Returns sub-groups within the given version range, bucketed by 1-minute
-- windows for a finer-grained view within each session group.
getSubHistoryGroups
  :: MonadDb env m
  => T.DocId
  -> T.UserId
  -> Int32   -- ^ startVersion (inclusive)
  -> Int32   -- ^ endVersion (inclusive)
  -> m [SubHistoryGroup]
getSubHistoryGroups docId userId startVer endVer =
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
      \  AND version >= ? \
      \  AND version <= ? \
      \GROUP BY to_timestamp(floor(extract(epoch FROM \"createdAt\") / 60) * 60) \
      \ORDER BY MIN(version) DESC"
      (docId, userId, startVer, endVer)


getDocAtVersion
  :: MonadDb env m
  => T.DocId
  -> Int32
  -> m DocAtVersion
getDocAtVersion docId targetVersion = do
  -- Find the nearest collab snapshot at or before the target version.
  -- If none exists, start from version 0 with an empty document and
  -- replay all steps from the beginning up to the target version.
  mSnap <- Doc.getLatestSnapshotBefore docId targetVersion
  let (snapVersion, snapDoc) = case mSnap of
        Just (v, d) -> (v, d)
        Nothing     -> (0, mempty)
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


getSubHistoryApi
  :: ( MonadDb env m
     , MonadLogger m
     )
  => AuthUser
  -> ActionT m ()
getSubHistoryApi user = do
  docId    <- captureParam "documentId"
  startVer <- captureParam "startVersion"
  endVer   <- captureParam "endVersion"
  doc      <- NotFound.handleNotFound (E.getByIdForUser @Doc.GetDoc user) docId
  unless (user.userId `elem` fmap (.userId) doc.editors) $ do
    status status403
    Scotty.text "Forbidden"
    finish
  groups <- lift $ getSubHistoryGroups docId user.userId startVer endVer
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
