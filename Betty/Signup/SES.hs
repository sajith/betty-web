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

------------------------------------------------------------------------

ses :: Email -> SES
ses email = SES { sesFrom       = sender
                , sesTo         = [encodeUtf8 email]
                , sesAccessKey  = access
                , sesSecretKey  = secret
                , sesRegion     = "us-east-1"
                }

------------------------------------------------------------------------

-- TODO: catch exception when renderSendMailSES fails.
sendVerificationEmail :: Email -> VerKey -> VerUrl -> HandlerT site IO ()
sendVerificationEmail email _ verurl = do
  let ses' = ses email
      mail = formMail email verurl

  runResourceT $ do
      manager <- liftIO $ newManager tlsManagerSettings
      catchAny (renderSendMailSES manager ses' mail)
          (\_ -> do
                -- TODO: This will likely be a SESException, but I
                -- don't want to expose visitors to the details.  So,
                -- (1) I do have to log this event somewhere; (2) User
                -- experience here is less than optimal because the
                -- visitor will see two conflicting messages on the
                -- same result page: "Error sending message" and "A
                -- confirmation email has been sent to
                -- you@example.net"  That sucks.
                lift $ setMessageI ("Error sending email." :: Text)
                return ())

------------------------------------------------------------------------

formMail :: Email -> VerUrl -> Mail
formMail email verurl = Mail
    { mailFrom    = Address Nothing sender
    , mailTo      = [Address Nothing email]
    , mailCc      = []
    , mailBcc     = []
    , mailHeaders = verHeaders
    , mailParts   = return $ map ($ verurl) [verText, verHtml]
    }

------------------------------------------------------------------------

