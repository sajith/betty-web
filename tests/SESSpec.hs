-- TODO: Refactor Betty.Signup, make it testable, and actually test
-- that piece of code instead of this isolated code.

module SESSpec (spec) where

import TestImport

#if USE_AWS_SES

import Yesod
import Betty.SESCreds                (access, ender, secret, sender)
import Control.Monad.Trans.Resource  (runResourceT)
import Network.HTTP.Conduit          (newManager, tlsManagerSettings)
import Network.Mail.Mime
import Network.Mail.Mime.SES
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

-- TODO: refactor to correctly use Betty.Signup.SES

spec :: Spec
spec = withApp $ do

    describe "SES Email test" $ do


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

ses :: SES
ses = SES { sesFrom      = sender
          , sesTo        = [ender]
          , sesAccessKey = access
          , sesSecretKey = secret
          , sesRegion    = "us-east-1"
          }

mail :: Mail
mail = Mail { mailHeaders = [("Subject", "Testing SES")]
            , mailFrom    = Address Nothing sender
            , mailTo      = [Address Nothing ender]
            , mailCc      = []
            , mailBcc     = []
            , mailParts   = return [textpart, htmlpart]
            }

textpart :: Part
textpart = Part "text/plain" None Nothing [] $
           "This is the text part."

htmlpart :: Part
htmlpart = Part "text/html" None Nothing [] $
           renderHtml $ toHtml ("This is the HTML part." :: String)

#else

spec :: Spec
spec = withApp $ do
    describe "SES Email test" $ do
        it "Not configured to use SES, skipping test" $ do
            assertEqual "Nothing" True $ not False

#endif
