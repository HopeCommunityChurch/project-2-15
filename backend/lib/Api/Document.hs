module Api.Document where

import Api.Auth (AuthUser)
import Api.Errors qualified as Errs
import DbHelper (MonadDb)
import Entity qualified as E
import Types qualified as T
import Entity.Document qualified as Doc
import Entity.AuthUser qualified as AuthUser
import Servant
import Api.Helpers (getByIdForUser)



getDocument
  :: MonadDb env m
  => AuthUser
  -> T.DocId
  -> m Doc.GetDoc
getDocument user docId =
  getByIdForUser user docId


-- createStudy
--   :: MonadDb env m
--   => AuthUser
--   -> Study.CrStudy
--   -> m Study.GetStudy
-- createStudy authUser crStudy = do
--   studyId <- Study.addStudy authUser.userId crStudy
--   Errs.handleNotFound (E.getById @Study.GetStudy) studyId


type Api =
  AuthProtect "cookie"
    :> Summary "Gets all the studies for a user"
    :> Description "Gets all the studies for a user"
    :> Capture "DocumentId" T.DocId
    :> Get '[JSON] Doc.GetDoc
  -- :<|> AuthProtect "cookie"
  --   :> ReqBody '[JSON] Study.CrStudy
  --   :> Summary "Adds a study into the database"
  --   :> Description "Adds a study into the database"
  --   :> Post '[JSON] Study.GetStudy



server
  :: MonadDb env m
  => ServerT Api m
server =
  getDocument
  -- :<|> createStudy

