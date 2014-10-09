module Betty.Signup.SES
       (
         sendVerificationEmail
       )
       where

import Prelude
import Yesod

import Network.Mail.Mime
import Network.Mail.Mime.SES
import Yesod.Auth.Email

import Data.Text.Encoding    (encodeUtf8)
import Network.HTTP.Conduit  (withManager)

import Betty.SESCreds        (access, secret, sender)
import Betty.Signup.MailText (verHeaders, verHtml, verText)

ses :: Email -> SES
ses email = SES { sesFrom       = sender
                , sesTo         = [encodeUtf8 email]
                , sesAccessKey  = access
                , sesSecretKey  = secret
                , sesRegion     = "us-east-1"
                }

-- TODO: catch exception when withManager fails.
sendVerificationEmail :: Email -> VerKey -> VerUrl -> HandlerT site IO ()
sendVerificationEmail email _ verurl = do
  let ses' = ses email
      mail = formMail email verurl
  withManager $ \manager ->
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
