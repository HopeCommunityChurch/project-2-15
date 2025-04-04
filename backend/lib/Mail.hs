module Mail where

import Network.Mail.SMTP qualified as Smtp
import Network.Socket (PortNumber)
import GHC.Records (HasField)
import Network.Mail.Mime (Mail(..))

data Auth = MkAuth
  { username :: Text
  , password :: Text
  }

data SSLSettings
  = NoSSL
  | StartTsl
  | SSL

data Smtp = MkSmtp
  { host :: String
  , port :: PortNumber
  , auth :: Maybe Auth
  , ssl :: SSLSettings
  }

type HasSmtp env = HasField "smtp" env Smtp


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

  let func = case smtp.ssl of
                NoSSL ->
                  bracket
                    (liftIO (Smtp.connectSMTP' smtp.host smtp.port))
                    (liftIO . Smtp.closeSMTP)
                StartTsl ->
                  bracket
                    (liftIO (Smtp.connectSMTPSTARTTLS' smtp.host smtp.port))
                    (liftIO . Smtp.closeSMTP)
                SSL ->
                  bracket
                    (liftIO (Smtp.connectSMTPS' smtp.host smtp.port))
                    (liftIO . Smtp.closeSMTP)


  func $ \ conn -> do
    logInfo $ "connected to " <> toText smtp.host <> ":" <> show smtp.port
    authResult <- forM smtp.auth $ \ auth ->
       liftIO $ Smtp.login
                  conn
                  (toString auth.username)
                  (toString auth.password)
    case authResult of
      Nothing -> liftIO $ Smtp.renderAndSend conn m
      Just (replyCode, bs) -> do
        if replyCode == 0 then
          liftIO $ Smtp.renderAndSend conn m
        else do
          logInfo $ "Auth Failed with code: " <> show replyCode
          logInfoSH bs
