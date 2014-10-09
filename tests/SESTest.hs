{-# LANGUAGE OverloadedStrings #-}

-- TODO: Refactor Betty.Signup, make it testable, and actually test
-- that piece of code instead of this isolated code.

module SESTest (sesMailSpecs) where

import Yesod

import Betty.SESCreds                (access, ender, secret, sender)
import Network.HTTP.Conduit          (withManager)
import Network.Mail.Mime
import Network.Mail.Mime.SES
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import TestImport

-- TODO: refactor to correctly use Betty.Signup.SES

sesMailSpecs :: Spec
sesMailSpecs = ydescribe "SES Email test" $ do
  yit "Does nothing in particular" $ do
    assertEqual "Nothing" True $ not False

  yit "Try SES" $ do
    withManager $ \manager ->
      renderSendMailSES manager ses mail
    assertEqual "Nothing" True $ not False

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


