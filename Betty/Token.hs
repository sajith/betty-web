{-# LANGUAGE RankNTypes #-}

module Betty.Token
       ( newToken
       , newToken'
       , maybeAuthToken
       , getToken
       , setToken
       , setNewToken
       , hasToken
       ) where

import           ClassyPrelude.Yesod as P

import           Data.Either         (isLeft)
import qualified Data.List           as L
import           Data.Text           as T

import qualified Data.Vector.Unboxed as V (Vector, map, toList)
import           System.Random.MWC   (asGenST, uniformVector,
                                      withSystemRandom)

import           Network.Wai         as W (requestHeaders)

import           Betty.Helpers       (sendJson)
import           Model

------------------------------------------------------------------------

-- custom header bearing email:token pair.
authTokenHeader :: forall a. IsString a => a
authTokenHeader = "X-Auth-Token"

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
            Key User -> HandlerT site IO (Maybe Text)
getToken uid = runDB $ do
    $logDebug ("getToken, uid: " <> tshow uid)
    t <- selectFirst [AuthTokenUid ==. uid] []
    case t of
      Nothing -> return Nothing
      Just (Entity _ v) -> return $ Just $ authTokenToken v

------------------------------------------------------------------------

-- TODO: write unit tests.
hasToken :: (YesodPersist site,
             YesodPersistBackend site ~ SqlBackend) =>
            Key User -> HandlerT site IO Bool
hasToken uid = runDB $ do
    res <- selectFirst [AuthTokenUid ==. uid] []
    return (isJust res)

------------------------------------------------------------------------

setNewToken :: (YesodPersist site,
                YesodPersistBackend site ~ SqlBackend) =>
               Key User -> HandlerT site IO ()
setNewToken uid = do
    token <- lift newToken
    $logDebug ("setNewToken: uid: " <> tshow (unUserKey uid)
               <> ", new token:" <> token)
    setToken uid token

------------------------------------------------------------------------

setToken :: forall site.
            (YesodPersist site,
             YesodPersistBackend site ~ SqlBackend) =>
            Key User -> Text -> HandlerT site IO ()
setToken uid token = runDB $ do
    $logDebug ("setToken: uid " <> tshow uid <> " token: " <> token)
    -- Try to insert (uid, token) to AuthToken record.  If that fails,
    -- try to update.  Using `repsert` would be nice, but we have two
    -- unique constraints on AuthToken record...
    res <- insertBy $ AuthToken uid token
    when (isLeft res) $
        updateWhere [AuthTokenUid ==. uid] [AuthTokenToken =. token]

------------------------------------------------------------------------

-- TODO: Handle "Accept:" header before sending JSON, maybe?
maybeAuthToken :: forall site.
                  (YesodPersist site,
                   YesodPersistBackend site ~ SqlBackend) =>
                  HandlerT site IO (Maybe (Key User))
maybeAuthToken = do

    request <- waiRequest

    case lookup authTokenHeader $ W.requestHeaders request of

        Just token -> do

            let tokenTxt = decodeUtf8 token
            $logDebug (authTokenHeader <> ": " <> tokenTxt)

            -- lookup auth token in AuthToken; return uid or
            -- nothing depending on lookup result.
            record <- runDB $ getBy (UniqueToken tokenTxt)

            case record of
              Nothing -> do
                  $logDebug ("Token " <> tokenTxt <>
                             "not found in DB")
                  _ <- sendJson status401 msgTokenWrong
                  return Nothing
              Just (Entity _ v) -> do
                  let uid = authTokenUid v
                  $logDebug ("Uid " <> tshow uid <>
                             "found for token " <> tokenTxt)
                  return $ Just uid

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
            "auth token header (" <> authTokenHeader <> ") not found"

------------------------------------------------------------------------
