{-# LANGUAGE RankNTypes #-}

module Betty.Token where

import           Import

import qualified Data.List            as L
import           Data.Maybe           (isJust)
import           Data.String          (IsString)
import qualified Data.Text            as T

import           System.Random        (StdGen, randomRIO, randomRs)

import           Database.Persist.Sql (SqlBackend (..))

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
msgTokenNotFound = "auth token not found"

------------------------------------------------------------------------

-- TODO: replace this with something better thought out.

-- generate a token (a random mix of uppercase and lowercase letters,
-- and digits.)
makeToken :: StdGen -> IO Text
makeToken g = fmap T.pack $ scramble $ concat [p1, p2, p3]
    where
        p1 = makeStr 4 ('A', 'Z')
        p2 = makeStr 4 ('a', 'z')
        p3 = makeStr 4 ('0', '9')

        makeStr :: Int -> (Char, Char) -> String
        makeStr len range = take len $ randomRs range g

        -- TODO: benchmark this.
        scramble :: String -> IO String
        scramble [] = return []
        scramble xs = do
            n <- randomRIO (0, length xs - 1)
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
