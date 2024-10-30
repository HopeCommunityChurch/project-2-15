module Api.Altcha where

import Servant
import Altcha qualified



createChallenge
  :: ( MonadReader env m
     , Altcha.HasAltchaKey env
     , MonadIO m
     )
  => m Altcha.Challenge
createChallenge = do
  key <- asks (.altchaKey)
  liftIO $ Altcha.createChallenge key 100_000 False


type Api =
  "challenge"
    :> Description "Altcha Challenge"
    :> Get '[JSON] Altcha.Challenge


server
  :: ( MonadReader env m
     , Altcha.HasAltchaKey env
     , MonadIO m
     )
  => ServerT Api m
server =
  createChallenge
