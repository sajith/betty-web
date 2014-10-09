module Betty.Signup.Sendmail
       (
         sendVerificationEmail
       )
       where

import Prelude
import Yesod

import Yesod.Auth.Email
import Network.Mail.Mime

import Betty.Signup.MailText      (verHeaders, verText, verHtml)


sendVerificationEmail :: Email -> VerKey -> VerUrl -> HandlerT site IO ()
sendVerificationEmail email _ verurl =
  liftIO $ renderSendMail (emptyMail $ Address Nothing "noreply")
                { mailTo      = [Address Nothing email]
                , mailHeaders = verHeaders
                , mailParts   = [map ($ verurl) [verText, verHtml]]
                }
