module Api.Document where

import Api.Auth (AuthUser)
import Api.Errors qualified as Errs
import Api.Helpers (getByIdForUser)
import Data.Aeson (Object)
import DbHelper (MonadDb)
import Entity.AuthUser qualified as AuthUser
import Entity.Document qualified as Doc
import Entity.User qualified as User
import Servant
import Types qualified as T


updateDocument
  :: MonadDb env m
  => AuthUser
  -> T.DocId
  -> Object
  -> m Doc.GetDoc
updateDocument user docId obj = do
  doc <- getByIdForUser @Doc.GetDoc user docId
  unless (user.userId `elem` fmap (.userId) doc.editors) $ do
    Errs.throwAuthErr
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

