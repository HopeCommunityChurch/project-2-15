module Mail where

import Network.HaskellNet.SMTP qualified as Smtp
import Network.HaskellNet.SMTP.SSL qualified as Smtp
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


doSMTPSTARTTLSWithSettings
  :: MonadUnliftIO m
  => String
  -> Smtp.Settings
  -> (Smtp.SMTPConnection -> m b)
  -> m b
doSMTPSTARTTLSWithSettings host settings action =
  withRunInIO $ \ runInIO ->
    Smtp.doSMTPSTARTTLSWithSettings host settings (runInIO . action)


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

  case smtp.auth of
    Nothing ->
      doSMTPPort smtp.host (fromIntegral smtp.port) $ \ conn -> do
        logInfo $ "connected to " <> toText smtp.host <> ":" <> show smtp.port
        liftIO $ Smtp.sendMail m conn
    Just auth -> do
      let settings = Smtp.defaultSettingsSMTPSTARTTLS
                        { Smtp.sslPort = fromIntegral smtp.port
                        }
      doSMTPSTARTTLSWithSettings smtp.host settings $ \ conn -> do
        logInfo $ "connected to " <> toText smtp.host <> ":" <> show smtp.port
        authResult <- liftIO $ Smtp.authenticate
                        Smtp.LOGIN
                        (toString auth.username)
                        (toString auth.password)
                        conn
        if authResult
          then liftIO $ Smtp.sendMail m conn
          else logInfo "Auth Failed"
