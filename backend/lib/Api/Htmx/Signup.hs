module Api.Htmx.Signup where

import Api.Auth (setCookie')
import Api.Htmx.Ginger (baseUrl, basicTemplate)
import Data.Aeson qualified as Aeson
import Data.HashMap.Strict qualified as HMap
import Data.List qualified as L
import Data.Text qualified as T
import Database qualified as Db
import Database.Beam (
  all_,
  guard_,
  runSelectReturningOne,
  select,
  val_,
  (==.),
 )
import DbHelper (MonadDb, runBeam)
import Emails.Welcome qualified
import Entity.User qualified as User
import Mail qualified
import Network.HTTP.Types.Status (status200)
import Password (newPassword)
import Text.Ginger
import Types qualified as T
import Web.Cookie qualified as Cookie
import Web.Scotty.Trans hiding (scottyT)
import Prelude hiding ((**))
import Altcha qualified




getSignup
  :: ( MonadIO m
     , MonadLogger m
     )
  => ActionT m ()
getSignup = do
  mRedirect <- L.lookup "redirect" <$> queryParams
  basicTemplate
    "signup.html"
    (HMap.insert "redirect" (toGVal mRedirect))


checkEmail
  :: MonadDb env m
  => T.Email
  -> m Bool
checkEmail email =
  fmap isNothing
  $ runBeam
  $ runSelectReturningOne
  $ select
  $ do
    user <- all_ Db.db.user
    guard_ $ user.email ==. val_ email
    pure user.userId


getChurch
  :: MonadDb env m
  => m (Maybe T.ChurchId)
getChurch =
  runBeam
  $ runSelectReturningOne
  $ select
  $ do
    church <- all_ Db.db.church
    guard_ $ church.name ==. val_ "HCC"
    pure church.churchId


signupForm
  :: ( MonadIO m
     , MonadLogger m
     )
  => Maybe Text
  -> SignupErrors
  -> ActionT m ()
signupForm mRedirect errors = do
  basicTemplate
    "signup/form.html"
    ( HMap.insert "errors" (toGVal (Aeson.toJSON errors))
    . HMap.insert "wasCorrect" "False"
    . HMap.insert "redirect" (toGVal mRedirect)
    )


data SignupErrors = MkSignupErrors
  { isNotTaken :: Bool
  , passwordsMatch :: Bool
  , passwordLength :: Bool
  , nameNotNull :: Bool
  , name :: Text
  , email :: T.Email
  , password :: Text
  , password2 :: Text
  , verified :: Bool
  }
  deriving (Generic, Show)
  deriving anyclass (ToJSON)


signup
  :: ( MonadDb env m
     , MonadLogger m
     , Mail.HasSmtp env
     , Altcha.HasAltchaKey env
     )
  => ActionT m ()
signup = do
  email <- formParam "email"
  name <- formParam "name"
  logInfoSH name
  password <- formParam "password"
  password2 <- formParam "password2"
  altcha <- formParam "altcha"
  key <- lift (asks (.altchaKey))
  let verified = Altcha.verifyChallenge key altcha
  mRedirect <- L.lookup "redirect" <$> formParams
  isNotTaken <- lift $ checkEmail email
  let passwordsMatch = password == password2
  let passwordLength = T.length password > 8
  let nameNotNull = not (T.null name)
  (Just churchId) <- lift getChurch
  let allGood = isNotTaken
                  && passwordsMatch
                  && passwordLength
                  && nameNotNull
                  && verified
  if allGood then do
    let newUser = User.MkNewUser
                    email
                    name
                    (newPassword password)
                    churchId
    userId <- lift $ User.createUser newUser
    lift $ Mail.sendMail (Emails.Welcome.mail newUser.email)
    let url = case mRedirect of
                Just re ->
                  if re == "" then baseUrl <> "/studies" else re
                Nothing  -> baseUrl <> "/studies"
    setHeader "HX-Redirect" url
    cookie <- lift $ setCookie' userId
    let cookieTxt = toLazy (decodeUtf8 (Cookie.renderSetCookieBS cookie))
    setHeader "Set-Cookie" cookieTxt
    status status200
  else do
    let errors = MkSignupErrors
                  isNotTaken
                  passwordsMatch
                  passwordLength
                  nameNotNull
                  name
                  email
                  password
                  password2
                  verified
    signupForm (fmap toStrict mRedirect) errors
