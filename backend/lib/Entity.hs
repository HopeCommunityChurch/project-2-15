{-# LANGUAGE UndecidableInstances #-}

module Entity where


import DbHelper (MonadDb, runBeam)
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
import qualified Database.Beam.Postgres.Syntax   as PgS
import qualified Database.Beam.Query             as BQ
import qualified GHC.Stack                       as Stack


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


-- | Limit an entity by some value in a query.
class Entity entity => GuardValue entity value where
  guardValues
    :: [BQ.QExpr Pg.Postgres s value]
    -> PgEntity entity s
    -> BQ.Q Pg.Postgres (EntityDatabase entity) s ()


class GuardValue entity (EntityId entity) => EntityWithId entity where
  type EntityId entity

  entityId :: PgEntity entity s -> BQ.QExpr Pg.Postgres s (EntityId entity)


guardValue
  :: GuardValue entity value
  => BQ.QExpr Pg.Postgres s value
  -> PgEntity entity s
  -> BQ.Q Pg.Postgres (EntityDatabase entity) s ()
guardValue v entity =
  guardValues (pure v) entity


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


queryEntityBy
  :: forall entity value s
   . ( GuardValue entity value )
  => Maybe (DbUser (EntityDatabase entity))
  -> BQ.QExpr Pg.Postgres s value
  -> BQ.Q Pg.Postgres (EntityDatabase entity) s (PgEntity entity s)
queryEntityBy user value = do
  entity <- queryEntity user
  guardValue value entity
  pure entity


getOneEntityBy
  :: forall entity value env m
   . ( GuardValue entity value
     , Entity entity
     , Typeable entity
     , B.Beamable (DbEntity entity)
     , HasSqlValueSyntax PgS.PgValueSyntax value
     , B.FromBackendRow Pg.Postgres (HsEntity entity)
     , MonadDb env m
     )
  => value
  -> m (Maybe entity)
getOneEntityBy value =
  fmap
    (fmap (toEntity @entity))
    (runBeam
      $ B.runSelectReturningOne
      $ addCommentE @entity
      $ B.select $ do
        entity <- queryEntity @entity Nothing
        guardValue @entity (B.val_ value) entity
        pure entity
    )


getById
  :: forall entity env m
   . ( EntityWithId entity
     , Entity entity
     , Typeable entity
     , B.Beamable (DbEntity entity)
     , HasSqlValueSyntax PgS.PgValueSyntax (EntityId entity)
     , B.FromBackendRow Pg.Postgres (HsEntity entity)
     , MonadDb env m
     )
  => EntityId entity
  -> m (Maybe entity)
getById id =
  getOneEntityBy id


getForUserBy
  :: forall entity value env m
   . ( Entity entity
     , Typeable entity
     , B.FromBackendRow Pg.Postgres (HsEntity entity)
     , B.Beamable (DbEntity entity)
     , MonadDb env m
     , HasSqlValueSyntax PgS.PgValueSyntax value
     , GuardValue entity value
     )
  => DbUser (EntityDatabase entity)
  -> value
  -> m [entity]
getForUserBy user value =
  fmap
    (fmap (toEntity @entity))
    (runBeam
      $ B.runSelectReturningList
      $ addCommentE @entity
      $ B.select $ do
        entity <- queryEntity @entity (Just user)
        guardValue @entity (B.val_ value) entity
        pure entity
    )


getAllForUser
  :: forall entity env m
   . ( Entity entity
     , Typeable entity
     , B.FromBackendRow Pg.Postgres (HsEntity entity)
     , B.Beamable (DbEntity entity)
     , MonadDb env m
     )
  => DbUser (EntityDatabase entity)
  -> m [entity]
getAllForUser user =
  fmap
    (fmap (toEntity @entity))
    (runBeam
      $ B.runSelectReturningList
      $ addCommentE @entity
      $ B.select
      $ queryEntity @entity (Just user)
    )


getOneForUserBy
  :: forall entity value env m
   . ( Entity entity
     , Typeable entity
     , B.FromBackendRow Pg.Postgres (HsEntity entity)
     , B.Beamable (DbEntity entity)
     , MonadDb env m
     , HasSqlValueSyntax PgS.PgValueSyntax value
     , GuardValue entity value
     )
  => DbUser (EntityDatabase entity)
  -> value
  -> m (Maybe entity)
getOneForUserBy user value =
  fmap
    (fmap (toEntity @entity))
    (runBeam
      $ B.runSelectReturningOne
      $ addCommentE @entity
      $ B.select $ do
        entity <- queryEntity @entity (Just user)
        guardValue @entity (B.val_ value) entity
        pure entity
    )


getByIdForUser
  :: forall entity env m
   . ( EntityWithId entity
     , Entity entity
     , Typeable entity
     , B.Beamable (DbEntity entity)
     , HasSqlValueSyntax PgS.PgValueSyntax (EntityId entity)
     , B.FromBackendRow Pg.Postgres (HsEntity entity)
     , MonadDb env m
     )
  => DbUser (EntityDatabase entity)
  -> EntityId entity
  -> m (Maybe entity)
getByIdForUser = getOneForUserBy

