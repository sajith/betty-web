{-# LANGUAGE RankNTypes #-}

module Handler.Api.V0.User where

import           Import

import           Data.Maybe                      (fromJust, isJust)
import           Data.String                     (IsString)
import qualified Data.Text                       as T
import           Data.Text.Encoding              (decodeUtf8)
import           Database.Persist.Sql            (SqlBackend (..))
import           Network.HTTP.Types              (hAuthorization)
import           Network.Wai                     (requestHeaders)
import           Network.Wai.Middleware.HttpAuth

import           Yesod.Auth.Email                (isValidPass, saltPass)

import           System.IO.Unsafe                (unsafePerformIO)
import           System.Random                   (newStdGen, randomRs)

import           Betty.Model

------------------------------------------------------------------------

-- TODO:
-- 1. Refactor to Betty/Token.hs + Betty/BasicAuth.hs
-- 2. Make sure regular auth flow continues to work
-- 3. Write tests
-- 4. Fix the TODO items.
-- 5. Carry on my wayward son

------------------------------------------------------------------------

-- TODO: get this from env.
realm :: IsString a => a
realm = "betty"

------------------------------------------------------------------------

getApiV0UserR :: Handler Value
getApiV0UserR = do

    request <- waiRequest

    (email, token, user, profile) <- lookupAuth request
                                     >>= decodeAuth
                                     >>= verifyAuth
                                     >>= getToken
                                     >>= getUserInfo

    let u = case user of
            Nothing -> undefined
            Just u' -> entityVal u'

    let p = case profile of
            Nothing -> undefined
            Just p' -> entityVal p'

    $(logDebug) $ T.pack $ "User" ++ show u
    $(logDebug) $ T.pack $ "Profile" ++ show p

    let todo    = ("TODO" :: Text)
        unknown = ("unknown" :: Text)

    return $ object $ [ "uid"                .= todo -- TODO
                      , "email"              .= userEmail u
                      , "auth_token"         .= token
                      , "verified"           .= userVerified u
                      , "timezone"           .= formatTZ p
                      , "diabetes_type"      .= userProfileDtype p
                      , "blood_glucose_unit" .= formatBg p
                      , "weight_unit"        .= formatWt p
                      , "birth_year"         .= formatBY p
                      , "diagnosed_year"     .= formatDY p
                      ]

        where

            unknown = ("unknown" :: Text)

            -- TODO: replace with something better.
            txt     = T.pack . show

            -- TODO: format to more useful timezone representation.
            formatTZ p = case userProfileTimezone p of
                Just y -> y
                _      -> unknown

            formatBg p = case userProfileBgunits p of
                Just MgDL -> "mg/dL"
                Just Mmol -> "mmol"
                _         -> unknown

            formatWt p = case userProfileWtunits p of
                Just Kg -> "kilograms"
                Just Lb -> "pounds"
                _       -> unknown

            formatBY p = case userProfileBirthYear p of
                Just y  -> if y == 0 then unknown else txt y
                Nothing -> unknown

            formatDY p = case userProfileDiagnosedYear p of
                Just y  -> if y == 0 then unknown else txt y
                Nothing -> unknown

------------------------------------------------------------------------

getUserInfo (email, token) = runDB $ do

    -- TODO: consider using Esqueleto instead of bare-bone persistent
    -- for joins, such as:
    --
    -- SELECT "user".id, email, verified, timezone, dtype, bgunits,
    -- wtunits, insunits, birth_year, diagnosed_year FROM "user",
    -- user_profile WHERE "user".email = 'email' AND "user".id =
    -- user_profile.uid;

    user <- selectFirst [UserEmail ==. email] []

    let user' = entityKey . fromJust $ user
    profile <- selectFirst [UserProfileUid ==. user'] []

    return (email, token, user, profile)

------------------------------------------------------------------------

lookupAuth request =
    case lookup hAuthorization $ requestHeaders request of
        Just enc -> return enc
        Nothing  -> denyMessage realm "Authentication is required."

------------------------------------------------------------------------

--  returns (email, pass) pair

decodeAuth enc =
    case extractBasicAuth enc of
        Just auth -> return auth
        Nothing   -> denyMessage realm "Need Basic Auth."

------------------------------------------------------------------------

verifyAuth (email, pass) = do
    realpass <- getPassword (decodeUtf8 email)
    case realpass of
        Just p ->
            if isValidPass (decodeUtf8 pass) p
            then return $ decodeUtf8 email
            else denyMessage realm "Wrong password."
        Nothing ->
            denyMessage realm "No password set."

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
            Text -> HandlerT site IO (Text, Maybe Text)
getToken email = runDB $ do
    $(logDebug) $ T.pack $ "at getToken" ++ show email ++ "\n"
    v <- getBy404 $ UniqueAuthTokens email
    return (email, authTokensToken $ entityVal v)

------------------------------------------------------------------------

isTokenSet :: forall site.
              (YesodPersist site,
               YesodPersistBackend site ~ SqlBackend) =>
              Text -> HandlerT site IO Bool
isTokenSet email = do
    (_, token) <- getToken email
    return $ isJust token

------------------------------------------------------------------------

-- TODO: handle the case when token is already set.
setToken :: forall site.
            (YesodPersist site,
             YesodPersistBackend site ~ SqlBackend)
            => Key User -> Text -> Maybe Text
            -> HandlerT site IO (Key AuthTokens)
setToken uid email token = runDB $
    insert $ AuthTokens uid email token

------------------------------------------------------------------------

getPassword :: forall site.
               (YesodPersist site,
                YesodPersistBackend site ~ SqlBackend)
               => Text -> HandlerT site IO (Maybe Text)
getPassword email = runDB $ do
    u <- getBy404 $ UniqueUser email
    return $ userPassword $ entityVal u

------------------------------------------------------------------------

denyMessage :: forall (m :: * -> *) b.
               MonadHandler m =>
               Text -> Text -> m b
denyMessage rlm msg = do
    addHeader "WWW-Authenticate" (makeRealm rlm)
    permissionDenied msg
    where
        makeRealm r = T.concat ["Basic realm=\"", r , "\""]

------------------------------------------------------------------------
