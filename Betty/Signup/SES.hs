module Betty.Signup.SES
       (
         sendVerificationEmail
       )
       where

import ClassyPrelude.Yesod   hiding (newManager)

import Network.Mail.Mime
import Network.Mail.Mime.SES
import Yesod.Auth.Email

import Network.HTTP.Conduit  (newManager, tlsManagerSettings)

import Betty.SESCreds        (access, secret, sender)
import Betty.Signup.MailText (verHeaders, verHtml, verText)

ses :: Email -> SES
ses email = SES { sesFrom       = sender
                , sesTo         = [encodeUtf8 email]
                , sesAccessKey  = access
                , sesSecretKey  = secret
                , sesRegion     = "us-east-1"
                }

-- TODO: catch exception when renderSendMailSES fails.
sendVerificationEmail :: Email -> VerKey -> VerUrl -> HandlerT site IO ()
sendVerificationEmail email _ verurl = do
  let ses' = ses email
      mail = formMail email verurl

  runResourceT $ do
      manager <- liftIO $ newManager tlsManagerSettings
      renderSendMailSES manager ses' mail

formMail :: Email -> VerUrl -> Mail
formMail email verurl = Mail
    { mailFrom    = Address Nothing sender
    , mailTo      = [Address Nothing email]
    , mailCc      = []
    , mailBcc     = []
    , mailHeaders = verHeaders
    , mailParts   = return $ map ($ verurl) [verText, verHtml]
    }
