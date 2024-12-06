module Mail where

import Network.HaskellNet.SMTP qualified as Smtp
import Network.Socket (PortNumber)
import GHC.Records (HasField)
import Network.Mail.Mime (Mail(..))

data Auth = MkAuth
  { username :: Text
  , password :: Text
  }

data Smtp = MkSmtp
  { host :: String
  , port :: Int
  , auth :: Maybe Auth
  }

type HasSmtp env = HasField "smtp" env Smtp

doSMTPPort
  :: MonadUnliftIO m
  => String
  -> PortNumber
  -> (Smtp.SMTPConnection -> m b)
  -> m b
doSMTPPort host port action =
  withRunInIO $ \ runInIO ->
    Smtp.doSMTPPort host port (runInIO . action)

sendMail
  :: ( HasSmtp env
     , MonadUnliftIO m
     , MonadReader env m
     , MonadLogger m
     )
  => Mail
  -> m ()
sendMail m = do
  logInfo $ "sending email to: " <> show m.mailTo
  smtp <- asks (.smtp)
  doSMTPPort smtp.host (fromIntegral smtp.port) $ \ conn -> do
    logInfo $ "connected to " <> toText smtp.host <> ":" <> show smtp.port
    authResult <- forM smtp.auth $ \ auth -> do
      logInfo $ "auth with " <> auth.username
      liftIO $ Smtp.authenticate
                  Smtp.LOGIN
                  (toString auth.username)
                  (toString auth.password)
                  conn
    if authResult == Just False
      then logInfo "Auth Failed"
      else liftIO $ Smtp.sendMail m conn
