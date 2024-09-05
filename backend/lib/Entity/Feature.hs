module Entity.Feature where

import Database qualified as Db
import Database.Beam (
  all_,
  delete,
  guard_,
  in_,
  insertValues,
  primaryKey,
  runDelete,
  runInsert,
  runSelectReturningList,
  select,
  val_,
  (&&.),
  (==.),
 )
import Database.Beam.Backend.SQL.BeamExtensions (
  conflictingFields,
  insertOnConflict,
  onConflictUpdateAll,
 )
import DbHelper (MonadDb, runBeam)
import Types qualified as T


getFeaturesForUser
  :: MonadDb env m
  => T.UserId
  -> m [T.Feature]
getFeaturesForUser userId =
  runBeam
  $ runSelectReturningList
  $ select
  $ do
    feature <- all_ Db.db.userFeature
    guard_ $ feature.userId ==. val_ userId
    pure feature.feature


data FeatureWithDescription = MkFeatureWithDescription
  { feature :: T.Feature
  , description :: Text
  }
  deriving (Generic, Show)
  deriving anyclass (ToJSON)


getAllFeatures :: [FeatureWithDescription]
getAllFeatures =
  fmap
    (\ f -> MkFeatureWithDescription f (T.featureDescription f))
    T.allFeatures


addFeatures
  :: MonadDb env m
  => T.UserId
  -> [T.Feature]
  -> m ()
addFeatures userId features =
  runBeam
  $ runInsert
  $ insertOnConflict Db.db.userFeature
    (insertValues (features <&> (\ feature -> Db.MkUserFeatureT userId feature)))
    (conflictingFields primaryKey)
    onConflictUpdateAll

removeFeatures
  :: MonadDb env m
  => T.UserId
  -> [T.Feature]
  -> m ()
removeFeatures userId features =
  runBeam
  $ runDelete
  $ delete
      Db.db.userFeature
      (\ uf -> uf.userId ==. val_ userId
            &&. uf.feature `in_` fmap val_ features
      )
