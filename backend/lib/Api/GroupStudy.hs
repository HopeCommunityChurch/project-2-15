module Api.GroupStudy where


import Api.Auth (AuthUser)
import Api.Errors qualified as Errs
import Api.Helpers (getByIdForUser)
import DbHelper (MonadDb)
import Entity qualified as E
import Entity.AuthUser qualified as AuthUser
import Entity.GroupStudy qualified as GroupStudy
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


server
  :: MonadDb env m
  => ServerT Api m
server =
  getStudies
  :<|> createStudy
  :<|> getGroupStudyMeta
