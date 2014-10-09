module Betty.Signup.MailText
       (
         verHeaders,
         verText,
         verHtml
       )
       where

import Prelude
import Yesod                            (shamlet)
import Yesod.Auth.Email                 (VerUrl)
import Network.Mail.Mime                (Part(..), Encoding(None))
import Text.Shakespeare.Text            (stext)
import Data.Text.Lazy.Encoding          (encodeUtf8)
import Text.Blaze.Html.Renderer.Utf8    (renderHtml)
import Data.String                      (IsString)

verHeaders :: (IsString t, IsString t1) => [(t, t1)]
verHeaders =  [("Subject", "Please verify your email address")]

-- TODO: externalize the message itself.
verText :: VerUrl -> Part
verText verurl = Part { partType = "text/plain; charset=utf-8"
                      , partEncoding = None
                      , partFilename = Nothing
                      , partContent = encodeUtf8 [stext|
Please confirm your email address by visiting the link below.

 #{verurl}

Thank you!
                        |]
                      , partHeaders = []
                      }

-- TODO: externalize the message itself.
verHtml :: VerUrl -> Part
verHtml verurl = Part { partType = "text/html; charset=utf-8"
                      , partEncoding = None
                      , partFilename = Nothing
                      , partContent = renderHtml [shamlet|
<p>Please confirm your email address by visiting the link below.
<p>
  <a href=#{verurl}>#{verurl}
<p>Thank you!
                        |]
                      , partHeaders = []
                      }
