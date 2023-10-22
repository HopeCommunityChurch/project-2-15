module Api.GroupStudy where


import Api.Auth (AuthUser)
import Api.Errors qualified as Errs
import Api.Helpers (getByIdForUser)
import DbHelper (MonadDb)
import Emails.ShareGroupStudy qualified
import Entity qualified as E
import Entity.AuthUser qualified as AuthUser
import Entity.GroupStudy qualified as GroupStudy
import Entity.Document qualified as Document
import Entity.Shares qualified as Shares
import Entity.User (GetUser (..))
import EnvFields (HasUrl)
import Mail qualified
import Servant
import Types qualified as T

newtype GetStudies = GetStudies (List GroupStudy.GetGroupStudy)
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)


getStudies
  :: MonadDb env m
  => AuthUser
  -> m GetStudies
getStudies user =
  GetStudies <$> E.getAllForUser user


createStudy
  :: MonadDb env m
  => AuthUser
  -> GroupStudy.CrStudy
  -> m GroupStudy.GetGroupStudy
createStudy authUser crStudy = do
  studyId <- GroupStudy.addStudy authUser.userId crStudy
  Errs.handleNotFound (E.getById @GroupStudy.GetGroupStudy) studyId


getGroupStudyMeta
  :: MonadDb env m
  => AuthUser
  -> T.GroupStudyId
  -> m GroupStudy.GetGroupStudy
getGroupStudyMeta = getByIdForUser


shareGroupStudy
  :: ( MonadDb env m
     , Mail.HasSmtp env
     , HasUrl env
     )
  => AuthUser
  -> T.GroupStudyId
  -> [Shares.ShareUnit]
  -> m NoContent
shareGroupStudy user gsId shares = do
  groupStudy <- getByIdForUser @GroupStudy.GetGroupStudy user gsId
  unless (user.userId `elem` fmap (.userId) groupStudy.owners)
    Errs.throwAuthErr
  stuff <- Shares.addShares gsId shares
  url <- asks (.url)
  for_ stuff $ \ (share, token) ->  do
    let email = Emails.ShareGroupStudy.mail share groupStudy.name token url
    Mail.sendMail email
  pure NoContent


data AcceptShare = MkAcceptShare
  { token :: T.ShareToken
  , documentId :: T.DocId
  }
  deriving (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)


acceptShare
  :: MonadDb env m
  => AuthUser
  -> AcceptShare
  -> m NoContent
acceptShare user MkAcceptShare{token, documentId} = do
  document <- getByIdForUser @Document.GetDoc user documentId
  unless (user.userId `elem` fmap (.userId) document.editors)
    Errs.throwAuthErr
  didWork <- Shares.acceptShare user.userId token documentId
  unless didWork Errs.throwAuthErr
  pure NoContent


type Api =
  AuthProtect "cookie"
    :> Summary "Gets all the studies for a user"
    :> Description "Gets all the studies for a user"
    :> Get '[JSON] GetStudies
  :<|> AuthProtect "cookie"
    :> ReqBody '[JSON] GroupStudy.CrStudy
    :> Summary "Adds a group study into the database"
    :> Description "Adds a group study into the database"
    :> Post '[JSON] GroupStudy.GetGroupStudy
  :<|> AuthProtect "cookie"
    :> Summary "Gets all the studies for a user"
    :> Description "Gets all the studies for a user"
    :> Capture "studyId" T.GroupStudyId
    :> Get '[JSON] GroupStudy.GetGroupStudy
  :<|> "share"
    :> AuthProtect "cookie"
    :> Summary "shares the study with other people"
    :> Description "shares the study with other people"
    :> Capture "studyId" T.GroupStudyId
    :> ReqBody '[JSON] [Shares.ShareUnit]
    :> PostNoContent
  :<|> "share"
    :> "accept"
    :> AuthProtect "cookie"
    :> Summary "Accept the share"
    :> Description "Accept the share"
    :> ReqBody '[JSON] AcceptShare
    :> PostNoContent




server
  :: ( MonadDb env m
     , Mail.HasSmtp env
     , HasUrl env
     )
  => ServerT Api m
server =
  getStudies
  :<|> createStudy
  :<|> getGroupStudyMeta
  :<|> shareGroupStudy
  :<|> acceptShare
