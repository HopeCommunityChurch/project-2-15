module Api.User where


import Api.Auth (AuthUser(..))
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


getMe
  :: MonadDb env m
  => AuthUser
  -> m User.GetUser
getMe user = do
  let userId = user.userId
  getOneForUserBy user (userId :: T.UserId)


type Api =
  AuthProtect "cookie"
    :> Capture "userId" T.UserId
    :> Description "Gets a user by it's id."
    :> Get '[JSON] User.GetUser
  :<|> "me"
    :> AuthProtect "cookie"
    :> Description "Get the currently signed in user"
    :> Get '[JSON] User.GetUser

server
  :: MonadDb env m
  => ServerT Api m
server =
  getUser
  :<|> getMe
