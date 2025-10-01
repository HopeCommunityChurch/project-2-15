module Api.Htmx.Profile where

import Api.Htmx.AuthHelper (AuthUser (..), getUserWithRedirect)
import Api.Htmx.Ginger (basicTemplate)
import Data.Aeson qualified as Aeson
import Data.HashMap.Strict qualified as HMap
import Data.List ((\\))
import Data.List qualified as L
import DbHelper (MonadDb)
import Entity.Feature qualified as Feature
import Entity.User qualified as User
import Text.Ginger
import Types qualified as T
import Web.Scotty.Trans hiding (scottyT)
import Prelude hiding ((**))




getProfile
  :: ( MonadDb env m
     , MonadLogger m
     )
  => AuthUser
  -> ActionT m ()
getProfile user = do
  userFeatures <- lift $ Feature.getFeaturesForUser user.userId
  let allFeatures = Feature.getAllFeatures
  basicTemplate
    "profile.html"
     ( HMap.insert "user" (toGVal (Aeson.toJSON user))
     . HMap.insert "userFeatures" (toGVal (Aeson.toJSON userFeatures))
     . HMap.insert "allFeatures" (toGVal (Aeson.toJSON allFeatures))
     )


putProfile
  :: ( MonadDb env m
     , MonadLogger m
     )
  => AuthUser
  -> ActionT m ()
putProfile user = do
  email <- formParam "email"
  name <- formParam "name"
  let up = User.MkUpdateUser
                email
                name
  lift $ logInfoSH up
  lift $ User.updateUser user.userId up
  user' <- getUserWithRedirect
  basicTemplate
    "profile/form.html"
    (HMap.insert "user" (toGVal (Aeson.toJSON user'))
    . HMap.insert "wasSaved" (toGVal True)
    )


toListOfFeatures :: [Param] -> [T.Feature]
toListOfFeatures =
  L.nub
  . fmap (\ (name, _) ->
    view T.featureIso name
  )


postFeatures
  :: ( MonadDb env m
     , MonadLogger m
     )
  => AuthUser
  -> ActionT m ()
postFeatures user = do
  let allFeatures = Feature.getAllFeatures
  enabledFeatures <- toListOfFeatures <$> formParams
  let disabledFeatures = fmap (.feature) allFeatures \\ enabledFeatures
  lift $ Feature.addFeatures user.userId enabledFeatures
  lift $ Feature.removeFeatures user.userId disabledFeatures
  userFeatures <- lift $ Feature.getFeaturesForUser user.userId
  basicTemplate
    "profile/featureForm.html"
    ( HMap.insert "user" (toGVal (Aeson.toJSON user))
    . HMap.insert "userFeatures" (toGVal (Aeson.toJSON userFeatures))
    . HMap.insert "allFeatures" (toGVal (Aeson.toJSON allFeatures))
    . HMap.insert "wasSaved" (toGVal True)
    )
