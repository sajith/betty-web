{-# LANGUAGE RankNTypes #-}

module Betty.Token where

-- TODO: we should not have to use this.
import           Prelude               as P

-- TODO: we should not have to use this.
#if __GLASGOW_HASKELL__ > 708
import           Data.Monoid          ((<>))
#endif

import           Control.Monad         (when)
import           Data.ByteString.Char8 as B (split)
import           Data.Maybe            (isJust, fromJust)
import           Data.String           (IsString)
import           Data.Text.Encoding    (decodeLatin1)
import           System.Random         (StdGen, randomRIO, randomRs)
import qualified Data.List             as L
import           Data.Text             as T

import           Database.Persist.Sql  (SqlBackend (..))
import           Network.HTTP.Types    (status401)
import           Network.Wai           (requestHeaders)

import           Model

import           Yesod.Core            (logDebug, HandlerT,
                                        sendResponseStatus, waiRequest)
import           Yesod.Persist.Core    (runDB, YesodPersist,
                                        YesodPersistBackend)

import           Database.Persist.Class (getBy, upsert)
import           Database.Persist.Types (Key, Entity, entityVal, entityKey)

------------------------------------------------------------------------

-- custom header bearing email:token pair.
hAuthToken :: forall a. IsString a => a
hAuthToken = "X-Auth-Token"

------------------------------------------------------------------------

-- failure message when supplied auth token doesn't match the real
-- auth token.
msgTokenWrong :: Text
msgTokenWrong = "incorrect auth token"

-- error message when auth token header isn't present in the request.
msgTokenNotFound :: Text
msgTokenNotFound = "auth token header not found"

-- error when auth token header cannot be parsed.
msgTokenCorrupt :: Text
msgTokenCorrupt = "auth token appears to be corrupt"

------------------------------------------------------------------------

-- TODO: replace this with something better thought out.

-- generate a token (a random mix of uppercase and lowercase letters,
-- and digits.)
makeToken :: StdGen -> IO Text
makeToken g = fmap T.pack $ scramble $ P.concat [p1, p2, p3]
    where
        p1 = makeStr 4 ('A', 'Z')
        p2 = makeStr 4 ('a', 'z')
        p3 = makeStr 4 ('0', '9')

        makeStr :: Int -> (Char, Char) -> String
        makeStr len range = P.take len $ randomRs range g

        -- TODO: benchmark this.
        scramble :: String -> IO String
        scramble [] = return []
        scramble xs = do
            n <- randomRIO (0, P.length xs - 1)
            let e = xs !! n
            result <- scramble (L.delete e xs)
            return (e:result)

------------------------------------------------------------------------

-- TODO: move this somewhere else.
txt :: forall a. Show a => a -> Text
txt = T.pack . show

------------------------------------------------------------------------

getRealToken :: forall site.
                (YesodPersist site,
                 YesodPersistBackend site ~ SqlBackend) =>
                Text -> HandlerT site IO (Maybe Text)
getRealToken email = do
    $(logDebug) ("getRealToken: " <> email)
    t <- runDB $ getBy $ UniqueAuthTokens email
    case t of
        Nothing -> do
            $(logDebug) ("getRealToken: no token found for " <> email)
            return Nothing
        Just t' -> do
            let token = (authTokensToken . entityVal) t'
            $(logDebug) ("getRealToken: Token "
                         <> txt token <> " found for "
                         <> email)
            return token

------------------------------------------------------------------------

isTokenSet :: forall site.
              (YesodPersist site,
               YesodPersistBackend site ~ SqlBackend) =>
              Text -> HandlerT site IO Bool
isTokenSet email = fmap isJust $ runDB $ getBy $ UniqueAuthTokens email

------------------------------------------------------------------------

setToken :: forall site.
            (YesodPersist site,
             YesodPersistBackend site ~ SqlBackend) =>
            Key User -> Text -> Text -> HandlerT site IO (Entity AuthTokens)
setToken uid email token = runDB $ do
    $logDebug ("setToken: " <> email <> " " <> token <> "\n")
    upsert (AuthTokens uid email (Just token)) []

------------------------------------------------------------------------

-- TODO: is there a better way to do this?
isTokenValid :: forall site.
                (YesodPersist site,
                 YesodPersistBackend site ~ SqlBackend) =>
                Text -> Text -> HandlerT site IO Bool
isTokenValid email token = do
    t <- getRealToken email
    case t of
        Nothing -> do
            $(logDebug) ("isTokenValid: no token for " <> email)
            return False
        Just realtoken -> do
            $(logDebug) ("realToken: " <> realtoken
                         <> ", given:" <> token)
            return (token == realtoken)

------------------------------------------------------------------------

-- TODO: send JSON-encoded error codes + messages when appropriate.
maybeUidFromHeader :: forall site.
                      (YesodPersist site,
                       YesodPersistBackend site ~ SqlBackend) =>
                      HandlerT site IO (Maybe (Key User))
maybeUidFromHeader = do

    request <- waiRequest

    case lookup hAuthToken $ requestHeaders request of
        Just hdr -> do
            $(logDebug) (hAuthToken <> ": " <> txt hdr)

            -- TODO: this is silly; do real parsing here.
            let xs = B.split ':' hdr

            when (P.length xs /= 2) $ do
                $(logDebug) msgTokenCorrupt
                sendResponseStatus status401 msgTokenCorrupt

            let email  = xs !! 0
                token  = xs !! 1
                email' = decodeLatin1 email
                token' = decodeLatin1 token

            $(logDebug) ("email: " <> email' <> ", token: " <> token')

            valid <- isTokenValid email' token'

            if valid
               then do
                    -- TODO: handle 'Nothing'
                    u <- runDB $ getBy $ UniqueUser email'
                    let user = fmap entityKey u
                        u'   = T.pack $ show $ fromJust user
                    $(logDebug) ("Found user from token: user " <> u')
                    return user
               else do
                    $(logDebug) msgTokenWrong
                    -- _ <- sendResponseStatus status401 msgTokenWrong
                    return Nothing

        Nothing  -> do
            $(logDebug) msgTokenNotFound
            return Nothing

------------------------------------------------------------------------

