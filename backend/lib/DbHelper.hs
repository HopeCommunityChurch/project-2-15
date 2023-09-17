{-# OPTIONS_GHC -fno-warn-orphans #-}

module DbHelper
  ( HasDbConn(..)
  , MonadDb
  , DbConn
  , createPool
  , ConnectInfo(..)
  , withConnection
  , withRawConnection
  , withNewConnection
  , withTransaction
  , runAfterTransaction
  , runBeam
  , addComment
  , jsonBuildObject
  , jsonArraryOf
  , jsonArrayAgg

  , asJustUnsafe_
  , asJust_
  ) where


import Control.Lens
import Data.Aeson (FromJSON (..))
import Database.Beam.Postgres qualified as Db
import Database.Beam.Postgres qualified as Pg
import Database.Beam.Postgres.Syntax (PgExpressionSyntax (..))
import Database.Beam.Postgres.Syntax qualified as PgS
import Database.Beam.Query qualified as BQ
import Database.Beam.Query.Internal (
  QAgg,
  TablePrefix,
 )
import Database.PostgreSQL.Simple (ConnectInfo (..), Connection)
import Database.PostgreSQL.Simple qualified as PgS
import EnvFields (EnvType (..), HasEnvType)
import GHC.Generics ((:*:) (..))
import GHC.Generics qualified as G
import GHC.Stack qualified as Stack
import GHC.TypeLits (KnownSymbol, symbolVal)
import UnliftIO.Pool qualified as Pool
import Unsafe.Coerce (unsafeCoerce)
import Database.Beam (guard_, isJust_)


class HasDbConn env where
  dbConn :: Lens' env DbConn


type MonadDb env m =
  ( MonadUnliftIO m
  , MonadLogger m
  , HasEnvType env
  , HasDbConn env
  , MonadReader env m
  , MonadFail m
  )


data DbConn
  = DbPool (Pool.Pool Connection)
  | DbConn Connection (Pool.Pool Connection)
  | DbTrans Connection (IORef [IO ()])


createPool :: ConnectInfo -> IO DbConn
createPool connInfo =
  DbPool
  <$> Pool.createPool
    (Db.connect connInfo)
    Db.close
    2
    timeout
    11
  where
    timeout = 30


withConnection
  :: forall env m b
   . ( MonadDb env m)
  => m b
  -> m b
withConnection action = do
  conn <- asks (view dbConn)
  case conn of
    DbPool pool -> do
      Pool.withResource
        pool
        (\conn' -> local (set dbConn (DbConn conn' pool)) action)
    DbConn _ _ -> action
    DbTrans _ _ -> action


withNewConnection
  :: forall env m b
   . (MonadDb env m)
  => m b
  -> m b
withNewConnection action = do
  conn <- asks (view dbConn)
  case conn of
    DbPool pool -> do
      Pool.withResource
        pool
        (\conn' -> local (set dbConn (DbConn conn' pool)) action)
    DbConn _ pool ->
      Pool.withResource
        pool
        (\conn' -> local (set dbConn (DbConn conn' pool)) action)
    DbTrans _ _ -> action


withRawConnection
  :: forall env b m
   . ( MonadDb env m)
  => (Connection -> m b)
  -> m b
withRawConnection action = do
  conn <- asks (view dbConn)
  case conn of
    DbPool pool -> do
      Pool.withResource
        pool
        (\conn' ->
          local
            (set dbConn (DbConn conn' pool))
            (action conn')
        )
    DbConn conn' _ ->
      action conn'
    DbTrans conn' _ ->
      action conn'


withTransaction
  :: forall m a env . (MonadDb env m)
  => m a
  -> m a
withTransaction action = do
  dbConn' <- asks (view dbConn)
  case dbConn' of
    (DbPool pool) -> do
      ref <- newIORef []
      result <-
        Pool.withResource pool $ \conn ->
          withRunInIO $ \ runInIO ->
            PgS.withTransaction
              conn
              (runInIO (local (set dbConn (DbTrans conn ref)) action))
      actions <- readIORef ref
      liftIO $ sequence_ actions
      pure result
    (DbConn conn _) -> do
      ref <- newIORef []
      result <- withRunInIO $ \ runInIO ->
        PgS.withTransaction conn
          $ runInIO (local (set dbConn (DbTrans conn ref)) action)
      actions <- readIORef ref
      liftIO $ sequence_ actions
      pure result
    DbTrans conn _  ->
      withRunInIO $ \ runInIO ->
        PgS.withSavepoint conn $ runInIO action


runAfterTransaction
  :: (MonadDb env m)
  => IO ()
  -> m ()
runAfterTransaction action = do
  conn <- asks (view dbConn)
  case conn of
    DbPool _ ->
      liftIO action
    DbConn _ _ ->
      liftIO action
    DbTrans _ ref ->
      atomicModifyIORef ref (\a -> (snoc a action, ()))


runBeam
  :: (MonadDb env m)
  => Pg.Pg a
  -> m a
runBeam pg = do
  env <- asks (.envType)
  withRawConnection $ \conn -> do
    case env of
      Prod ->
        liftIO $ Pg.runBeamPostgres conn pg
      Dev _ ->
        withRunInIO $ \runInIO ->
          Pg.runBeamPostgresDebug (runInIO . logDebug . toText) conn pg


-- Adds a comment to the beginning of the query saying where the query is
addComment
  :: HasCallStack
  => BQ.SqlSelect Pg.Postgres a
  -> BQ.SqlSelect Pg.Postgres a
addComment (BQ.SqlSelect pgSelect) =
  let callstack = Stack.callStack
      r = getCallStack callstack
  in
      case r of
        ((_, loc): _ ) ->
          let str = encodeUtf8
                      $ Stack.srcLocPackage loc <> ":"
                      <> Stack.srcLocModule loc <> ":"
                      <> show (Stack.srcLocStartLine loc)
              select = PgS.PgSelectSyntax (PgS.emit "/*" <> PgS.emit str <> PgS.emit "*/" <> PgS.fromPgSelect pgSelect)
          in BQ.SqlSelect select
        _ -> BQ.SqlSelect pgSelect




class GJSONBuildObject f where
  gJsonBuildObject :: TablePrefix -> f p -> Pg.PgSyntax

instance GJSONBuildObject G.V1 where
  gJsonBuildObject _ _ = mempty

instance KnownSymbol fieldName => GJSONBuildObject (G.S1 ('G.MetaSel ('Just fieldName) su ss ds) (G.Rec0 (BQ.QGenExpr ctxt Pg.Postgres s a))) where
  gJsonBuildObject tbl (G.M1 (G.K1 (BQ.QExpr a))) =
    PgS.emit "'"
    <> PgS.escapeString (fromString (symbolVal ( Proxy :: Proxy fieldName)))
    <> PgS.emit "', "
    <> fromPgExpression (a tbl)

instance (GJSONBuildObject a, GJSONBuildObject b) => GJSONBuildObject (a :*: b) where
  gJsonBuildObject tbl (a :*: b) =
    gJsonBuildObject tbl a <> PgS.emit ", " <> gJsonBuildObject tbl b

instance (GJSONBuildObject f) => GJSONBuildObject (G.D1 c f) where
  gJsonBuildObject tbl (G.M1 a) = gJsonBuildObject tbl a

instance (GJSONBuildObject f) => GJSONBuildObject (G.C1 c f) where
  gJsonBuildObject tbl (G.M1 a) = gJsonBuildObject tbl a

instance FromJSON a => FromJSON (Pg.PgJSONB a) where
  parseJSON a = Pg.PgJSONB <$> parseJSON a


jsonBuildObject
  :: ( Generic (table (BQ.QGenExpr ctxt Pg.Postgres s))
     , GJSONBuildObject (G.Rep (table (BQ.QGenExpr ctxt Pg.Postgres s)))
     , FromJSON (table Identity)
     )
  => table (BQ.QGenExpr ctxt Pg.Postgres s)
  -> BQ.QGenExpr ctxt Pg.Postgres s (Pg.PgJSONB (table Identity))
jsonBuildObject table =
  BQ.QExpr $ \tbl ->
    PgExpressionSyntax
      ( PgS.emit "jsonb_build_object("
      <> gJsonBuildObject tbl (G.from table)
      <> PgS.emit ")"
      )

jsonArrayAgg
  :: BQ.QExpr Pg.Postgres s (Pg.PgJSONB a)
  -> QAgg Pg.Postgres s (Pg.PgJSONB (Vector a))
jsonArrayAgg (BQ.QExpr a) =
  BQ.QExpr $
    fmap (PgExpressionSyntax . mappend (PgS.emit "jsonb_agg") . PgS.pgParens . fromPgExpression) a


toJsonArray
  :: BQ.QExpr Pg.Postgres s (Vector (Pg.PgJSONB a))
  -> BQ.QGenExpr ctxt Pg.Postgres s (Pg.PgJSONB (Vector a))
toJsonArray (BQ.QExpr a) =
  BQ.QExpr $ \tbl ->
    PgExpressionSyntax
      ( PgS.emit "to_jsonb("
      <> fromPgExpression (a tbl)
      <> PgS.emit ")"
      )


jsonArraryOf
  :: BQ.Q Pg.Postgres db s (BQ.QExpr Pg.Postgres s (Pg.PgJSONB a))
  -> BQ.QExpr Pg.Postgres s (Pg.PgJSONB (Vector a))
jsonArraryOf q =
  toJsonArray (Pg.arrayOf_ q)


asJustUnsafe_
  :: BQ.QExpr Pg.Postgres s (Maybe a)
  -> BQ.QExpr Pg.Postgres s a
asJustUnsafe_ = unsafeCoerce


asJust_
  :: BQ.QExpr Pg.Postgres s (Maybe a)
  -> BQ.Q Pg.Postgres db s (BQ.QExpr Pg.Postgres s a)
asJust_ a = do
  guard_ $ isJust_ a
  pure $ asJustUnsafe_ a
