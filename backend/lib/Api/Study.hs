module Api.Study where


import Api.Auth (AuthUser)
import Api.Errors qualified as Errs
import DbHelper (MonadDb)
import Entity qualified as E
import Entity.Study qualified as Study
import Servant

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
createStudy _ crStudy = do
  studyId <- Study.addStudy crStudy
  Errs.handleNotFound (E.getById @Study.GetStudy) studyId


type Api =
  AuthProtect "cookie"
    :> Description "Gets all the studies for a user"
    :> Get '[JSON] GetStudies
  :<|> AuthProtect "cookie"
    :> ReqBody '[JSON] Study.CrStudy
    :> Description "Adds a study into the database"
    :> Get '[JSON] Study.GetStudy



server
  :: MonadDb env m
  => ServerT Api m
server =
  getStudies
  :<|> createStudy
