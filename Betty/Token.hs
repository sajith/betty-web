{-# LANGUAGE RankNTypes #-}

module Betty.Token where

import           Import

import           Control.Monad        (liftM)
import qualified Data.List            as L
import           Data.Maybe           (isJust)
import qualified Data.Text            as T

import           System.Random        (StdGen, randomRIO, randomRs)

import           Database.Persist.Sql (SqlBackend (..))

------------------------------------------------------------------------R

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

-- TODO: return appropriate error string with 404.
getToken :: forall site.
            (YesodPersist site,
             YesodPersistBackend site ~ SqlBackend) =>
            Text -> HandlerT site IO (Maybe Text)
getToken email = runDB $ do
    $logDebug ("getToken: " <> email <> "\n")
    -- v <- getBy404 $ UniqueAuthTokens email
    -- return $ authTokensToken $ entityVal v
    liftM (authTokensToken . entityVal) (getBy404 (UniqueAuthTokens email))

------------------------------------------------------------------------

-- TODO: use a better name for this function.
-- TODO: improve error messages.
getToken' :: forall site.
             (YesodPersist site,
              YesodPersistBackend site ~ SqlBackend) =>
             Text -> HandlerT site IO Text
getToken' email = do
    t <- runDB $ getBy $ UniqueAuthTokens email
    let token = case t of
            Nothing -> "no token (email not found)"
            Just t' -> case authTokensToken $ entityVal t' of
                Just t'' -> t''
                Nothing  -> "no token"
    return token

------------------------------------------------------------------------

isTokenSet :: forall site.
              (YesodPersist site,
               YesodPersistBackend site ~ SqlBackend) =>
              Text -> HandlerT site IO Bool
isTokenSet email = liftM isJust (getToken email)

------------------------------------------------------------------------

setToken :: forall site.
            (YesodPersist site,
             YesodPersistBackend site ~ SqlBackend) =>
            Key User -> Text -> Text -> HandlerT site IO (Entity AuthTokens)
setToken uid email token = runDB $ do
    $logDebug ("setToken: " <> email <> " " <> token <> "\n")
    upsert (AuthTokens uid email (Just token)) []

------------------------------------------------------------------------
