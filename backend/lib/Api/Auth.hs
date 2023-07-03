module Api.Auth (Api, server) where

import Database qualified as Db
import Database.Beam (all_, guard_, runSelectReturningOne, select, val_, (==.), runInsert, insert, insertValues)
import DbHelper (MonadDb, runBeam)
import Password (Password, PasswordHash, comparePassword)
import Servant
import Types qualified as T
import Web.Cookie qualified as Cookie
import Data.Time.Lens qualified as TL
import Api.Errors qualified as Errs


data PassLogin = MkPassLogin
  { email :: T.Email
  , password :: Password
  }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON, ToSchema)


type CookieHeader = Headers '[ Header "Set-Cookie" Cookie.SetCookie]


getPasswordHash
  :: MonadDb env m
  => T.Email
  -> m (Maybe (T.UserId, PasswordHash))
getPasswordHash email =
  runBeam
  $ runSelectReturningOne
  $ select
  $ do
    user <- all_ Db.db.user
    guard_ $ user.email ==. val_ email
    p <- all_ Db.db.userPassword
    guard_ $ p.userId ==. user.userId
    pure (p.userId, p.password)


mkCookie
  :: (MonadDb env m)
  => T.UserId
  -> m (T.CookieToken, UTCTime)
mkCookie userId = do
  token <- T.genCookieToken
  now <- getCurrentTime
  let expiresAt = now & (TL.flexDT . TL.days) +~ 10
  runBeam
    $ runInsert
    $ insert Db.db.userSession
    $ insertValues
      [ Db.MkUserSessionT
          userId
          token
          expiresAt
          now
      ]
  pure (token, expiresAt)



passwordLogin
  :: MonadDb env m
  => PassLogin
  -> m (CookieHeader ())
passwordLogin MkPassLogin{email, password} = do
  mHash <- getPasswordHash email
  case mHash of
    Nothing -> Errs.throwAuthErr
    Just (userId, hash) ->
      if comparePassword password hash
        then do
          (token, expiresAt) <- mkCookie userId
          let setCookie = Cookie.defaultSetCookie
                          { Cookie.setCookieName = "p215-auth"
                          , Cookie.setCookieValue = encodeUtf8 (unwrap token)
                          , Cookie.setCookieExpires = Just expiresAt
                          , Cookie.setCookieHttpOnly = True
                          , Cookie.setCookieSecure = True
                          , Cookie.setCookieSameSite = Just Cookie.sameSiteNone
                          , Cookie.setCookiePath = Just "/"
                          }
          pure $ addHeader setCookie ()
        else Errs.throwAuthErr


type Api =
  "password"
    :> Description "Password login"
    :> ReqBody '[JSON] PassLogin
    :> Verb 'POST 204 '[JSON] (CookieHeader ())


server
  :: MonadDb env m
  => ServerT Api m
server =
  passwordLogin
