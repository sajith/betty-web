-- TODO: Refactor Betty.Signup, make it testable, and actually test
-- that piece of code instead of this isolated code.

module SESSpec (spec) where

import Yesod

import Betty.SESCreds                (access, ender, secret, sender)
import Control.Monad.Trans.Resource  (runResourceT)
import Network.HTTP.Conduit          (newManager, tlsManagerSettings)
import Network.Mail.Mime
import Network.Mail.Mime.SES
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import TestImport

-- TODO: refactor to correctly use Betty.Signup.SES

spec :: Spec
spec = withApp $ do

    describe "SES Email test" $ do

#if USE_AWS_SES        
        -- TODO: this is not particularly useful when SES credentials
        -- aren't available; fix.
        it "Trying to send email using SES" $ do
            runResourceT $ do
                manager <- liftIO $ newManager tlsManagerSettings
                renderSendMailSES manager ses mail

            -- TODO: this line does nothing except pleasing the type
            -- checker; make it do useful work, by checking results of
            -- running the above code.
            assertEqual "Nothing" True $ not False
#else
        it "Not configured to use SES, skipping test" $ do
            assertEqual "Nothing" True $ not False
#endif

ses :: SES
ses = SES { sesFrom = sender
          , sesTo = [ender]
          , sesAccessKey = access
          , sesSecretKey = secret
          , sesRegion    = "us-east-1"
          }

mail :: Mail
mail = Mail { mailHeaders =
                 [("Subject", "Testing email")]
            , mailFrom = Address Nothing sender
            , mailTo = [Address Nothing ender]
            , mailCc = []
            , mailBcc = []
            , mailParts = return [textpart, htmlpart]
            }

textpart :: Part
textpart = Part "text/plain" None Nothing [] $
           "Just some text"

htmlpart :: Part
htmlpart = Part "text/html" None Nothing [] $
           renderHtml $ toHtml ("Just some html" :: String)
