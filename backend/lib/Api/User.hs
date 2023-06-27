module Api.User where


import qualified Entity.User as User
import qualified Types as T
import Servant ( Capture, JSON, Description, type (:>), Get )

getUser
  :: Monad m
  => T.UserId
  -> m User.GetUser
getUser = undefined

type Api =
  Capture "userId" T.UserId
    :> Description "Gets a user by it's id."
    :> Get '[JSON] User.GetUser
