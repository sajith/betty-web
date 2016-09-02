module Betty.Signup.MailText
       (
         verHeaders,
         verText,
         verHtml
       )
       where

import ClassyPrelude.Yesod
import Network.Mail.Mime             (Encoding (None), Part (..))
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Shakespeare.Text         (stext)
import Yesod.Auth.Email              (VerUrl)

------------------------------------------------------------------------

verHeaders :: (IsString t, IsString t1) => [(t, t1)]
verHeaders =  [("Subject", "Please verify your email address")]

------------------------------------------------------------------------

-- TODO: externalize the message itself.
verText :: VerUrl -> Part
verText verurl = Part { partType = "text/plain; charset=utf-8"
                      , partEncoding = None
                      , partFilename = Nothing
                      , partContent = encodeUtf8 [stext|
Hello,

Someone (possibly you?) has requested for an account with our service
using your email address.

If it was indeed you, please confirm your email address by visiting the
link below.

#{verurl}

If you did not create an account with us, please ignore this email.

Thank you!
|]
                      , partHeaders = []
                      }

------------------------------------------------------------------------

-- TODO: externalize the message itself.
verHtml :: VerUrl -> Part
verHtml verurl = Part { partType = "text/html; charset=utf-8"
                      , partEncoding = None
                      , partFilename = Nothing
                      , partContent = renderHtml [shamlet|
<p>Hello,

<p>Someone (possibly you?) has requested for an account with our service using your email address.

<p>If it was indeed you, please confirm your email address by visiting the link below.

<p>
  <a href=#{verurl}>#{verurl}

<p>If you did not request for an account with us, please ignore this email.

<p>Thank you!
|]
                      , partHeaders = []
                      }

------------------------------------------------------------------------

