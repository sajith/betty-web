{-# LANGUAGE RankNTypes #-}

module Handler.Api.V0.User where

import           Import

-- TODO: we should not have to use this.
#if __GLASGOW_HASKELL__ > 708
import           Data.Monoid                     ((<>))
#endif

import           Control.Monad                   (liftM)
import           Data.ByteString                 (ByteString)
import           Data.String                     (IsString)
import qualified Data.Text                       as T
import           Data.Text.Encoding              (decodeUtf8)

import           Database.Persist.Sql            (SqlBackend (..),
                                                  unSqlBackendKey)
import           Network.HTTP.Types              (hAuthorization)
import           Network.Wai                     (Request (..),
                                                  requestHeaders)
import           Network.Wai.Middleware.HttpAuth (extractBasicAuth)
import           Yesod.Auth.Email                (isValidPass)

import           Betty.Model
import           Betty.Token

------------------------------------------------------------------------

-- TODO:
-- 1. Move basic auth to Betty/BasicAuth.hs or some such.
-- 2. Make sure regular auth flow continues to work.
-- 3. Write tests.
-- 4. Fix the TODO items.
-- 5. Carry on my wayward son.

------------------------------------------------------------------------

-- TODO: get this from env.
realm :: IsString a => a
realm = "betty"

------------------------------------------------------------------------

getApiV0UserR :: Handler Value
getApiV0UserR = do

    request <- waiRequest
    email   <- lookupAuth request >>= decodeAuth >>= verifyAuth
    token   <- getRealToken email
    user    <- getUserInfo email
    profile <- getUserProfile $ entityKey user

    let u   = entityVal user
        uid = getUid $ entityKey user
        p   = entityVal profile

    $logDebug ("User: " <> txt u)
    $logDebug ("Profile: " <> txt p)

    return $ object [ "uid"                .= show uid
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

            getUid = unSqlBackendKey . unUserKey

            fmtUid = T.pack . show . getUid

            getUserInfo email = runDB $ do
                $logInfo ("getUserInfo: " <> email <> "\n")
                getBy404 $ UniqueUser email

            getUserProfile uid = runDB $ do
                $logInfo ("getUserProfile: " <> fmtUid uid <> "\n")
                getBy404 $ UniqueUserProfile uid

            -- TODO: move the format helpers somewhere more general;
            -- use a typeclass ('PPrint' or 'ToJSON' or some such?),
            -- perhaps?

            unknown = "unknown" :: Text

            -- TODO: replace with something better.
            -- txt     = T.pack . show

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

lookupAuth :: forall (m :: * -> *).
              MonadHandler m =>
              Request -> m ByteString
lookupAuth request =
    case lookup hAuthorization $ requestHeaders request of
        Just enc -> return enc
        Nothing  -> denyMessage realm "Authentication is required."

------------------------------------------------------------------------

type Email      = ByteString
type Passord    = ByteString

--  returns (email, pass) pair
decodeAuth :: forall (m :: * -> *).
              MonadHandler m =>
              ByteString -> m (Email, Passord)
decodeAuth enc =
    case extractBasicAuth enc of
        Just auth -> return auth
        Nothing   -> denyMessage realm "Need Basic Auth."

------------------------------------------------------------------------

verifyAuth :: forall site.
              (YesodPersist site,
               YesodPersistBackend site ~ SqlBackend) =>
              (Email, Passord) -> HandlerT site IO Text
verifyAuth (email, passwd) = do
    let emailBs  = decodeUtf8 email
        passwdBs = decodeUtf8 passwd
    realpass <- getPassword emailBs
    case realpass of
        Just p ->
            if isValidPass passwdBs p
            then return emailBs
            else denyMessage realm "Wrong password."
        Nothing ->
            denyMessage realm "No password set."

------------------------------------------------------------------------

getPassword :: forall site.
               (YesodPersist site,
                YesodPersistBackend site ~ SqlBackend)
               => Text -> HandlerT site IO (Maybe Text)
getPassword email = runDB $ do
    $logDebug ("getPassword: " <> email <> "\n")
    liftM (userPassword . entityVal) (getBy404 (UniqueUser email))

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
