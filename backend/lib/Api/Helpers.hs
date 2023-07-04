
module Api.Helpers where

import Api.Errors qualified as Errs
import Database.Beam qualified as B
import Database.Beam.Backend.SQL.SQL92 (HasSqlValueSyntax)
import Database.Beam.Postgres qualified as Pg
import Database.Beam.Postgres.Syntax qualified as PgS
import DbHelper
import Entity qualified as E




getOneForUserBy
  :: forall entity value env m
   . ( E.Entity entity
     , Typeable entity
     , B.FromBackendRow Pg.Postgres (E.HsEntity entity)
     , B.Beamable (E.DbEntity entity)
     , MonadDb env m
     , HasSqlValueSyntax PgS.PgValueSyntax value
     , E.GuardValue entity value
     , Show value
     , ToJSON value
     )
  => E.DbUser (E.EntityDatabase entity)
  -> value
  -> m entity
getOneForUserBy user =
  Errs.handleNotFound (E.getOneForUserBy user)


getByIdForUser
  :: forall entity env m
   . ( E.EntityWithId entity
     , E.Entity entity
     , Typeable entity
     , B.Beamable (E.DbEntity entity)
     , HasSqlValueSyntax PgS.PgValueSyntax (E.EntityId entity)
     , B.FromBackendRow Pg.Postgres (E.HsEntity entity)
     , MonadDb env m
     , Show (E.EntityId entity)
     , ToJSON (E.EntityId entity)
     )
  => E.DbUser (E.EntityDatabase entity)
  -> E.EntityId entity
  -> m entity
getByIdForUser user = Errs.handleNotFound (E.getOneForUserBy user)

