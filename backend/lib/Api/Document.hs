module Api.Document where

import Data.Aeson (Object)
import Api.Auth (AuthUser)
import DbHelper (MonadDb)
import Types qualified as T
import Entity.Document qualified as Doc
import Entity.AuthUser qualified as AuthUser
import Entity.User qualified as User
import Servant
import Api.Helpers (getByIdForUser)


updateDocument
  :: MonadDb env m
  => AuthUser
  -> T.DocId
  -> Object
  -> m Doc.GetDoc
updateDocument user docId obj = do
  doc <- getByIdForUser @Doc.GetDoc user docId
  when (user.userId `elem` fmap (.userId) doc.editors) $ do
    Doc.updateDocument docId obj
  getByIdForUser user docId



type Api =
  AuthProtect "cookie"
    :> Summary "Gets all the studies for a user"
    :> Description "Gets all the studies for a user"
    :> Capture "documentId" T.DocId
    :> Get '[JSON] Doc.GetDoc
  :<|> AuthProtect "cookie"
    :> Summary "Update document"
    :> Description "Update document"
    :> Capture "documentId" T.DocId
    :> ReqBody '[JSON] Object
    :> Put '[JSON] Doc.GetDoc



server
  :: MonadDb env m
  => ServerT Api m
server =
  getByIdForUser
  :<|> updateDocument

