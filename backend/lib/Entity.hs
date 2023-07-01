{-# LANGUAGE UndecidableInstances #-}

module Entity where


import DbHelper
import           Data.Aeson                      (FromJSON(..))
import           Data.Typeable
    ( tyConName
    , tyConPackage
    , typeRep
    , typeRepTyCon
    )
import qualified Database.Beam                   as B
import           Database.Beam.Backend.SQL.SQL92 (HasSqlValueSyntax)
import qualified Database.Beam.Backend.Types     as BT
import qualified Database.Beam.Postgres          as Pg
import           Database.Beam.Postgres.Syntax   (PgExpressionSyntax(..))
import qualified Database.Beam.Postgres.Syntax   as PgS
import qualified Database.Beam.Query             as BQ
import           Database.Beam.Query.Internal
    ( ContextRewritable(..)
    , ProjectibleWithPredicate(..)
    , QAgg
    , TablePrefix
    , ThreadRewritable(..)
    )
import           GHC.Generics                    ((:*:)(..))
import qualified GHC.Generics                    as G
import qualified GHC.Stack                       as Stack
import           GHC.TypeLits                    (KnownSymbol, symbolVal)
import           Unsafe.Coerce                   (unsafeCoerce)
import UnliftIO (MonadUnliftIO(..))


type family DbUser (db :: (Type -> Type) -> Type) :: Type

-- | What is an entity? It's basically some type in Haskell that we presist in
-- the database.
class Entity e where
  -- | The database type for the entity. A lot of them exists on more than one
  -- table which is why we need to say the type.
  data DbEntity e (f :: Type -> Type)

  -- | The database that the entity exists in. Just realized these name are
  -- horrible.
  type EntityDatabase e :: (Type -> Type) -> Type

  -- | A function that takes the type that beam spits out for our entity and
  -- converts it to our entity. There isn't any room for failures, might need to
  -- change this in the future.
  toEntity :: HsEntity e -> e

  -- | How to get an entity from the database. With limits if the user is given
  queryEntity
    :: Maybe (DbUser (EntityDatabase e))
    -> BQ.Q Pg.Postgres (EntityDatabase e) s (PgEntity e s)


instance (Entity e, Typeable e, B.Beamable (DbEntity e)) => B.Table (DbEntity e) where
  data PrimaryKey (DbEntity e) f = EntitiesHaveNoId
    deriving (Generic)
    deriving anyclass (B.Beamable)
  primaryKey _ = EntitiesHaveNoId


type HsEntity e = BQ.QExprToIdentity (PgEntity e BQ.QBaseScope)
type PgEntity e s = DbEntity e (BQ.QExpr Pg.Postgres s)


-- TODO: figure out how to replace this with Generics
class Entity e => NullEntity e where
  -- | Convert a nullable value to a maybe value for the entity
  fromNullable :: DbEntity e (B.Nullable Identity) -> Maybe (HsEntity e)


toNullableEntity
  :: ( NullEntity e
     , B.Beamable (DbEntity e)
     , Generic (DbEntity e Identity)
     , Generic (DbEntity e BT.Exposed)
     )
  => DbEntity e (B.Nullable Identity)
  -> Maybe e
toNullableEntity = fmap toEntity . fromNullable




addCommentE
  :: forall e a
   . (HasCallStack, Typeable e, Entity e)
  => BQ.SqlSelect Pg.Postgres a
  -> BQ.SqlSelect Pg.Postgres a
addCommentE (BQ.SqlSelect pgSelect) =
  let callstack = Stack.callStack
      tyCon = typeRepTyCon (typeRep (Proxy @e))
      name = "(" <> tyConPackage tyCon <> ":" <> tyConName tyCon <> ")"
      r = getCallStack callstack
  in case r of
      ((_, loc): _ ) ->
        let str = encodeUtf8 $ Stack.srcLocModule loc <> ":" <> show (Stack.srcLocStartLine loc) <> " " <> name
            select = PgS.PgSelectSyntax (PgS.emit "/*" <> PgS.emit str <> PgS.emit "*/" <> PgS.fromPgSelect pgSelect)
        in BQ.SqlSelect select
      _ -> BQ.SqlSelect pgSelect


