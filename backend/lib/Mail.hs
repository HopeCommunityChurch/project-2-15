module Mail where

import Network.HaskellNet.SMTP qualified as Smtp
import Network.Socket (PortNumber)
import GHC.Records (HasField)
import Network.Mail.Mime (Mail)

data Smtp = MkSmtp
  { host :: String
  , port :: PortNumber
  }

type HasSmtp env = HasField "smtp" env Smtp


sendMail
  :: ( HasSmtp env
     , MonadUnliftIO m
     , MonadReader env m
     )
  => Mail
  -> m ()
sendMail m = do
  smtp <- asks (.smtp)
  liftIO $ Smtp.doSMTPPort smtp.host smtp.port $ Smtp.sendMail m
