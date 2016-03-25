module Handler.ProfileStart where

import Import

import Betty.Model

------------------------------------------------------------------------

getProfileStartR :: Handler Html
getProfileStartR = do
    Entity uid u <- requireAuth

    -- when there's data in UserProfile for this UID, merely show the
    -- completion message.

    profile <- runDB $ count [UserProfileUid ==. uid]

    if profile < 1
        then
            defaultLayout $ do
                setTitle "Betty: Set up your profile"
                $(widgetFile "profile.start")
        else
            defaultLayout $ do
                setTitle "You're good to go"
                $(widgetFile "profile.finish")

    -- For testing, comment the above if-then-else block and uncomment
    -- the below block.

    -- defaultLayout $ do
    --     setTitle "Betty: Set up your profile"
    --     $(widgetFile "profile.start")

------------------------------------------------------------------------

data ProfileData =
    ProfileData { timezone      :: Text
                , bgUnit        :: Text
                , wtUnit        :: Text
                , diabetesType  :: Maybe Text
                , diagnosedYear :: Maybe Int
                , birthYear     :: Maybe Int
                }

------------------------------------------------------------------------

postProfileStartR :: Handler Html
postProfileStartR = do
    uid <- requireAuthId

    input <- runInputPost $ ProfileData
                <$> ireq textField "timezone"
                <*> ireq textField "bgUnit"
                <*> ireq textField "wtUnit"
                <*> iopt textField "diabetesType"
                <*> iopt intField  "diagnosedYear"
                <*> iopt intField  "birthYear"

    let tz = timezone input

        bu = case bgUnit input of
                "mgdl" -> MgDL
                "mmol" -> Mmol
                _      -> MgDL

        wu = case wtUnit input of
                "kg" -> Kg
                "lb" -> Lb
                _    -> Lb

        ty = case diabetesType input of
                Just t  -> t
                Nothing -> "unknown"

        dy = case diagnosedYear input of
                Just y  -> y
                Nothing -> 0

        by = case birthYear input of
                Just y  -> y
                Nothing -> 0

    let record = UserProfile
                 uid
                 (Just tz)   -- Timezone
                 (Just ty)   -- Diabetes type
                 (Just bu)   -- Blood sugar units
                 (Just wu)   -- Weight units
                 Nothing     -- Insulin units
                 (Just by)   -- Year of birth
                 (Just dy)   -- Year diagnosed

    _ <- runDB $ insert record

    -- $(logDebug) (pack $ show record)

    defaultLayout $ do
        setTitle "Thank you"
        $(widgetFile "profile.finish")

------------------------------------------------------------------------
