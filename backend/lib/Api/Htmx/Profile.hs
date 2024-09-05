module Api.Htmx.Profile where

import Api.Htmx.AuthHelper (AuthUser (..), getUserWithRedirect)
import Api.Htmx.Ginger (baseContext, gvalHelper, readFromTemplates)
import Data.Aeson qualified as Aeson
import Data.HashMap.Strict qualified as HMap
import Data.List ((\\))
import Data.List qualified as L
import DbHelper (MonadDb)
import Entity.Feature qualified as Feature
import Entity.User qualified as User
import Text.Ginger
import Text.Ginger.Html (htmlSource)
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
  result <- readFromTemplates "profile.html"
  userFeatures <- lift $ Feature.getFeaturesForUser user.userId
  let allFeatures = Feature.getAllFeatures
  case result of
    Right template -> do
      let context = baseContext
                    & HMap.insert "user" (toGVal (Aeson.toJSON user))
                    & HMap.insert "userFeatures" (toGVal (Aeson.toJSON userFeatures))
                    & HMap.insert "allFeatures" (toGVal (Aeson.toJSON allFeatures))
      let content = makeContextHtml (gvalHelper context)
      let h = runGinger content template
      html $ toLazy (htmlSource h)
    Left err -> html (show err)


putProfile
  :: ( MonadDb env m
     , MonadLogger m
     )
  => AuthUser
  -> ActionT m ()
putProfile user = do
  result <- readFromTemplates "profile/form.html"
  email <- formParam "email"
  name <- formParam "name"
  let up = User.MkUpdateUser
                email
                name
  lift $ logInfoSH up
  lift $ User.updateUser user.userId up
  user' <- getUserWithRedirect
  case result of
    Right template -> do
      let context = baseContext
                    & HMap.insert "user" (toGVal (Aeson.toJSON user'))
      let content = makeContextHtml (gvalHelper context)
      let h = runGinger content template
      html $ toLazy (htmlSource h)
    Left err -> html (show err)


toListOfFeatures :: [Param] -> [T.Feature]
toListOfFeatures =
  L.nub
  . fmap (\ (name, _) ->
    view T.featureIso (toStrict name)
  )


postFeatures
  :: ( MonadDb env m
     , MonadLogger m
     )
  => AuthUser
  -> ActionT m ()
postFeatures user = do
  result <- readFromTemplates "profile/featureForm.html"
  let allFeatures = Feature.getAllFeatures
  enabledFeatures <- toListOfFeatures <$> formParams
  let disabledFeatures = fmap (.feature) allFeatures \\ enabledFeatures
  lift $ Feature.addFeatures user.userId enabledFeatures
  lift $ Feature.removeFeatures user.userId disabledFeatures
  userFeatures <- lift $ Feature.getFeaturesForUser user.userId
  case result of
    Right template -> do
      let context = baseContext
                    & HMap.insert "user" (toGVal (Aeson.toJSON user))
                    & HMap.insert "userFeatures" (toGVal (Aeson.toJSON userFeatures))
                    & HMap.insert "allFeatures" (toGVal (Aeson.toJSON allFeatures))
      let content = makeContextHtml (gvalHelper context)
      let h = runGinger content template
      html $ toLazy (htmlSource h)
    Left err -> html (show err)

