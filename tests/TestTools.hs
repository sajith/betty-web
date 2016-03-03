--
-- Code from Yesod wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Performing-Authentication-during-Testing
--

module TestTools
       ( assertFailure
       , urlPath
       , needsLogin
       , doLogin
       , StdMethod(..)
       ) where

import TestImport

import Yesod.Core           (RedirectUrl)
import Network.URI          (URI(uriPath), parseURI)
import Network.HTTP.Types   (StdMethod(..), renderStdMethod, Status(..))
import Network.Wai.Test     (SResponse(..))

import Data.ByteString   as B hiding (elem) 
import Data.Text         as T

testRoot :: ByteString
testRoot = "http://localhost:3000"

afterLogin :: ByteString
afterLogin = "/"

assertFailure :: String -> YesodExample App ()
assertFailure msg = assertEqual msg True False

urlPath :: Text -> Text
urlPath = T.pack . maybe "" uriPath . parseURI . T.unpack

urlPathB :: ByteString -> Text
urlPathB = urlPath . decodeUtf8

firstRedirect :: RedirectUrl App url =>
                 StdMethod -> url -> YesodExample App (Maybe ByteString)
firstRedirect method url = do
    request $ do
        setMethod $ renderStdMethod method
        setUrl url
    extractLocation

assertLoginPage :: ByteString -> YesodExample App ()
assertLoginPage loc = do
    assertEqual "Right login redirection location" (testRoot `B.append` "/auth/login") loc
    get $ urlPathB loc
    statusIs 200
    bodyContains "Login"

submitLogin :: Text -> Text -> YesodExample App (Maybe ByteString)
submitLogin user pass = do
    request $ do
        setMethod "POST"
        setUrl $ urlPathB $ testRoot `B.append` "/auth/page/email/login"
        addPostParam "username" user
        addPostParam "password" pass
    extractLocation

extractLocation :: YesodExample App (Maybe ByteString)
extractLocation = do
    withResponse (\ SResponse {simpleStatus = s, simpleHeaders = h} -> do
                       let code = statusCode s
                       assertEqual ("Expected 302/303, received " ++ show code)
                           (code `elem` [302,303])
                           True
                       return $ lookup "Location" h
                 )

needsLogin :: RedirectUrl App url =>
              StdMethod -> url -> YesodExample App ()
needsLogin method url = do
    mbloc <- firstRedirect method url
    maybe (assertFailure "Should have location header") assertLoginPage mbloc

doLogin :: Text -> Text -> YesodExample App ()
doLogin user pass = do
    mbloc <- firstRedirect GET $ urlPathB testRoot
    maybe (assertFailure "Should have location header") assertLoginPage mbloc
    mbloc2 <- submitLogin user pass
    maybe (assertFailure "Should have second location header")
        (assertEqual "Check post-login redirection" (testRoot `B.append` afterLogin))
        mbloc2
