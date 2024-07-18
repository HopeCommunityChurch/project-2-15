module LateralLeftJoin where

import Control.Monad.Free.Church (liftF)
import Database.Beam (
  Columnar',
  Nullable,
  Projectible,
  Q,
  QExpr,
  QGenExpr (QExpr),
 )
import Database.Beam.Postgres (Postgres)
import Database.Beam.Postgres.Syntax (
  PgFromSyntax (PgFromSyntax, fromPgFrom),
  emit,
 )
import Database.Beam.Query.Internal (
  Q (Q),
  QF (QArbitraryJoin),
  QNested,
  ThreadRewritable (..),
 )
import Database.Beam.Schema.Tables (
  Columnar' (..),
  Retaggable (..),
 )


lateralLeft_ :: forall s a b db
          . ( ThreadRewritable s a
            , ThreadRewritable (QNested s) b
            , Projectible Postgres b
            , Retaggable (QExpr Postgres s) (WithRewrittenThread (QNested s) s b)
            )
         => a -> (WithRewrittenThread s (QNested s) a -> Q Postgres db (QNested s) b)
         -> Q Postgres db s (Retag Nullable (WithRewrittenThread (QNested s) s b))
lateralLeft_ using mkSubquery = do
  let Q subquery = mkSubquery (rewriteThread (Proxy @(QNested s)) using)
  Q (liftF (QArbitraryJoin subquery
                           "lat_"
                           (\a b _ ->
                                PgFromSyntax $
                                fromPgFrom a <> emit " LEFT JOIN LATERAL " <> fromPgFrom b <> emit " ON TRUE")
                           (\_ -> Nothing)
                           (\r -> retag (\(Columnar' (QExpr e) :: Columnar' (QExpr be s) g) ->
                                          Columnar' (QExpr e) :: Columnar' (Nullable (QExpr be s)) g
                                        )
                                  $ rewriteThread (Proxy @s) r))
                           )
