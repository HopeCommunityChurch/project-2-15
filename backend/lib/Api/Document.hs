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


data CrDoc = MkCrDoc
  { studyTemplateId :: Maybe T.StudyTemplateId
  , name :: Text
  , document :: Object
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)


createDocument
  :: MonadDb env m
  => AuthUser
  -> CrDoc
  -> m Doc.GetDoc
createDocument user cr = do
  let cr2 = Doc.CrDoc cr.studyTemplateId cr.name cr.document user.userId
  docId <- Doc.crDocument cr2
  getByIdForUser user docId


type Api =
  AuthProtect "cookie"
    :> Summary "Gets all documents for a user"
    :> Description "Gets all documents for a user"
    :> Get '[JSON] [Doc.GetDoc]
  :<|> AuthProtect "cookie"
    :> Summary "Get document"
    :> Description "Get document"
    :> Capture "documentId" T.DocId
    :> Get '[JSON] Doc.GetDoc
  :<|> AuthProtect "cookie"
    :> Summary "Update document"
    :> Description "Update document"
    :> Capture "documentId" T.DocId
    :> ReqBody '[JSON] Object
    :> Put '[JSON] Doc.GetDoc
  :<|> AuthProtect "cookie"
    :> Summary "Create a document, it's not in a study"
    :> Description "Create a document, it's not in a study"
    :> ReqBody '[JSON] CrDoc
    :> Post '[JSON] Doc.GetDoc



server
  :: MonadDb env m
  => ServerT Api m
server =
  Doc.getAllDocs
  :<|> getByIdForUser
  :<|> updateDocument
  :<|> createDocument

