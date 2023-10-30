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

data UpdateDocument = MkUpdateDocument
  { document :: Object
  , lastUpdated :: UTCTime
  }
  deriving (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)


data DocumentUpdatedNotMatch = MkDocumentUpdated
  deriving (Show, Generic)
  deriving anyclass (Exception, ToJSON, Errs.ApiException)


updateDocument
  :: MonadDb env m
  => AuthUser
  -> T.DocId
  -> UpdateDocument
  -> m Doc.GetDoc
updateDocument user docId update = do
  doc <- getByIdForUser @Doc.GetDoc user docId
  unless (user.userId `elem` fmap (.userId) doc.editors)
    Errs.throwAuthErr
  unless (doc.updated == update.lastUpdated) $
    Errs.throwApi MkDocumentUpdated
  Doc.updateDocument docId update.document
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
    :> ReqBody '[JSON] UpdateDocument
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

