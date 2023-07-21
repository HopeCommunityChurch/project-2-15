module Api.User where


import Api.Auth (AuthUser)
import Api.Helpers (getOneForUserBy)
import DbHelper (MonadDb)
import Entity.User qualified as User
import Servant
import Types qualified as T


getUser
  :: MonadDb env m
  => AuthUser
  -> T.UserId
  -> m User.GetUser
getUser = getOneForUserBy


type Api =
  AuthProtect "cookie"
    :> Capture "userId" T.UserId
    :> Description "Gets a user by it's id."
    :> Get '[JSON] User.GetUser

server
  :: MonadDb env m
  => ServerT Api m
server =
  getUser
