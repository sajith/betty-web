module Betty.Signup.Sendmail
       (
         sendVerificationEmail
       )
       where

import ClassyPrelude.Yesod

import Network.Mail.Mime
import Yesod.Auth.Email

import Betty.Signup.MailText (verHeaders, verHtml, verText)


sendVerificationEmail :: Email -> VerKey -> VerUrl -> HandlerT site IO ()
sendVerificationEmail email _ verurl =
  liftIO $ renderSendMail (emptyMail $ Address Nothing "noreply")
                { mailTo      = [Address Nothing email]
                , mailHeaders = verHeaders
                , mailParts   = [map ($ verurl) [verText, verHtml]]
                }
