{-# LANGUAGE RankNTypes #-}

module Betty.Token
       ( newToken
       , newToken'
       , maybeUidFromHeader
       , getToken
       , setToken
       , isTokenSet
       ) where

import           Import

import qualified Data.List                        as L
import           Data.Maybe                       (fromJust)
import           Data.Text                        as T

import qualified Data.Vector.Unboxed              as V (Vector, map, toList)
import           System.Random.MWC                (asGenST, uniformVector,
                                                   withSystemRandom)

import           Data.Attoparsec.ByteString.Char8 as C
import           Data.ByteString.Char8            as B

import           Network.Wai                      as W (requestHeaders)

import           Betty.Helpers                    (sendJson)

------------------------------------------------------------------------

-- custom header bearing email:token pair.
hAuthToken :: forall a. IsString a => a
hAuthToken = "X-Auth-Token"

------------------------------------------------------------------------

tokenLength :: Int
tokenLength = 12

------------------------------------------------------------------------

-- TODO: replace newToken (and newToken') with something better
-- thought out.
--
-- `newToken` generates a token (a random mix of uppercase and
-- lowercase letters, and digits.)

newToken :: IO Text
newToken = newToken' tokenLength

newToken' :: Int -> IO Text
newToken' len = do

    v <- withSystemRandom . asGenST $ \gen -> uniformVector gen len
    return $ T.pack $ V.toList $ V.map toChar (v :: V.Vector Word8)

    where

        toChar :: Enum a => a -> Char
        toChar i = chars L.!! (fromEnum i `mod` L.length chars)

        chars :: String
        chars = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']

------------------------------------------------------------------------

getToken :: forall site.
            (YesodPersist site,
             YesodPersistBackend site ~ SqlBackend) =>
            Text -> HandlerT site IO (Maybe Text)
getToken email = do
    $(logDebug) ("getToken: " <> email)
    t <- runDB $ getBy $ UniqueUser email
    case t of
        Nothing -> do
            $(logDebug) ("getToken: no token found for " <> email)
            return Nothing
        Just t' -> do
            let token = (userToken . entityVal) t'
            $(logDebug) ("getToken: Token "
                         <> tshow token <> " found for "
                         <> email)
            return token

------------------------------------------------------------------------

isTokenSet :: forall site.
              (YesodPersist site,
               YesodPersistBackend site ~ SqlBackend) =>
              Text -> HandlerT site IO Bool
isTokenSet email = isJust <$> getToken email

------------------------------------------------------------------------

setToken :: forall site.
            (YesodPersist site,
             YesodPersistBackend site ~ SqlBackend) =>
            Key User -> Text -> Text -> HandlerT site IO ()
setToken uid email token = runDB $ do
    $logDebug ("setToken: " <> email <> " " <> token)
    update uid [UserToken =. Just token]

------------------------------------------------------------------------

-- TODO: is there a better way to do this?
isTokenValid :: forall site.
                (YesodPersist site,
                 YesodPersistBackend site ~ SqlBackend) =>
                Text -> Text -> HandlerT site IO Bool
isTokenValid email token = do
    t <- getToken email
    case t of
        Nothing -> do
            $(logDebug) ("isTokenValid: no token for " <> email)
            return False
        Just realtoken -> do
            $(logDebug) ("realToken: " <> realtoken
                         <> ", given:" <> token)
            return (token == realtoken)

------------------------------------------------------------------------

type AuthIdent  = Text
type AuthToken  = Text

authParser :: Parser (AuthIdent, AuthToken)
authParser = do

    email <- takeWhile1 (/= ':')
    _     <- char8 ':'
    token <- many1 anyChar

    return (decodeUtf8 email, T.pack token)

str2auth :: ByteString -> Either String (AuthIdent, AuthToken)
str2auth str = eitherResult $ feed (parse authParser str) B.empty

------------------------------------------------------------------------

-- TODO: Handle "Accept:" header, before sending JSON, maybe?
maybeUidFromHeader :: forall site.
                      (YesodPersist site,
                       YesodPersistBackend site ~ SqlBackend) =>
                      HandlerT site IO (Maybe (Key User))
maybeUidFromHeader = do

    request <- waiRequest

    case lookup hAuthToken $ W.requestHeaders request of

        Just hdr -> do

            $(logDebug) (hAuthToken <> ": " <> tshow hdr)

            case str2auth hdr of

                Left err -> do
                    $(logDebug) ("Error: " <> tshow err)
                    _ <- sendJson status401 msgTokenCorrupt
                    return Nothing

                Right (email, token) -> do
                    $(logDebug) ("Auth email: " <> email <>
                                 " token: " <> token)

                    valid <- isTokenValid email token

                    if valid
                        then do
                            -- TODO: handle 'Nothing'
                            u <- runDB $ getBy $ UniqueUser email
                            let user = fmap entityKey u

                            let u' = T.pack $ show $ fromJust user
                            $(logDebug) ("Found token user: " <> u')

                            return user
                        else do
                            _ <- sendJson status401 msgTokenWrong
                            return Nothing

        Nothing  -> do
            -- NOTE: calling any 'sendStatus' (sendResponseStatus,
            -- sendStatusJSON, sendJson etc) here will break the
            -- regular web auth flow.  Would have been nice to return
            -- actual error code, but meh.
            $(logDebug) msgTokenNotFound
            return Nothing

    where

        -- failure message when supplied auth token doesn't match the
        -- real auth token.
        msgTokenWrong :: Text
        msgTokenWrong = "incorrect auth token"

        -- error message when auth token header isn't present in the
        -- request.
        msgTokenNotFound :: Text
        msgTokenNotFound =
            "auth token header (" <> hAuthToken <> ") not found"

        -- error when auth token header cannot be parsed.
        msgTokenCorrupt :: Text
        msgTokenCorrupt = "auth token appears to be corrupt"

------------------------------------------------------------------------

