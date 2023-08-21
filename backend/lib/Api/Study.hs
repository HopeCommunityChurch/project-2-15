module Api.Study where


import Api.Auth (AuthUser)
import Api.Errors qualified as Errs
import Api.Helpers (getByIdForUser)
import DbHelper (MonadDb)
import Entity qualified as E
import Entity.AuthUser qualified as AuthUser
import Entity.Study qualified as Study
import Servant
import Types qualified as T

newtype GetStudies = GetStudies (List Study.GetStudy)
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
  -> Study.CrStudy
  -> m Study.GetStudy
createStudy authUser crStudy = do
  studyId <- Study.addStudy authUser.userId crStudy
  Errs.handleNotFound (E.getById @Study.GetStudy) studyId


getStudyMeta
  :: MonadDb env m
  => AuthUser
  -> T.StudyId
  -> m Study.GetStudy
getStudyMeta = getByIdForUser

type Api =
  AuthProtect "cookie"
    :> Summary "Gets all the studies for a user"
    :> Description "Gets all the studies for a user"
    :> Get '[JSON] GetStudies
  :<|> AuthProtect "cookie"
    :> ReqBody '[JSON] Study.CrStudy
    :> Summary "Adds a study into the database"
    :> Description "Adds a study into the database"
    :> Post '[JSON] Study.GetStudy
  :<|> AuthProtect "cookie"
    :> Summary "Gets all the studies for a user"
    :> Description "Gets all the studies for a user"
    :> Capture "studyId" T.StudyId
    :> Get '[JSON] Study.GetStudy



server
  :: MonadDb env m
  => ServerT Api m
server =
  getStudies
  :<|> createStudy
  :<|> getStudyMeta
