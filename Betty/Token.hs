{-# LANGUAGE RankNTypes #-}

module Betty.Token where

import           Import

import           Control.Monad        (liftM)
import           Data.Maybe           (isJust)
import qualified Data.Text            as T

import           System.IO.Unsafe     (unsafePerformIO)
import           System.Random        (newStdGen, randomRs)

import           Database.Persist.Sql (SqlBackend (..))
import           Yesod.Auth.Email     (saltPass)

------------------------------------------------------------------------

-- TODO: for salt+hash functions, use Crypto.PasswordStore from
-- pwstore-fast, rather than Yesod.Auth.Email

------------------------------------------------------------------------

makeToken :: IO Text
makeToken =  saltPass $ scramble $ T.pack $ concat [p1, p2, p3]
    where
        p1 = makeStr 4 ('A', 'Z')
        p2 = makeStr 4 ('a', 'z')
        p3 = makeStr 4 ('0', '9')

        -- TODO: remove use of unsafePerformIO here, by using
        -- mwc-random or some such, perhaps?
        makeStr :: Int -> (Char, Char) -> String
        makeStr len range = take len $
                            randomRs range $
                            unsafePerformIO newStdGen

        -- TODO: write this.
        scramble :: Text -> Text
        scramble = id

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

-- TODO: handle the case when token is already set.
-- TODO: handle the case when token param is Nothing.
setToken :: forall site.
            (YesodPersist site,
             YesodPersistBackend site ~ SqlBackend)
            => Key User -> Text -> Maybe Text
            -> HandlerT site IO (Key AuthTokens)
setToken uid email token = runDB $ do
    -- $logDebug ("setToken: " <> email <> " " <> token <> "\n")
    insert $ AuthTokens uid email token

------------------------------------------------------------------------
