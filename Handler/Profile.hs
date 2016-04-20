module Handler.Profile where

import Import

import Betty.Model
import Betty.Token (getToken)

------------------------------------------------------------------------

getProfileR :: Handler Html
getProfileR = do
    Entity uid u <- requireAuth

    t <- getToken $ userEmail u
    let token = fromMaybe "not set" t

    -- TODO: refactor this.
    userProfile <- runDB $ selectFirst [UserProfileUid ==. uid] []
    let bgUnitPref = case userProfile of
            Just p  -> fromMaybe MgDL (userProfileBgunits $ entityVal p)
            Nothing -> MgDL
    $(logDebug) ("bgUnitPref: " <> tshow bgUnitPref)

    defaultLayout $ do
        setTitle "Betty: Your Profile"
        $(widgetFile "profile")
        $(widgetFile "profile.password")
        $(widgetFile "profile.email")
        $(widgetFile "profile.timezone")
        $(widgetFile "profile.units")
        $(widgetFile "profile.token")
        $(widgetFile "profile.delete")

------------------------------------------------------------------------

-- TODO: implement this.
postProfileR :: Handler Html
postProfileR = error "Not yet implemented: postProfileR"

------------------------------------------------------------------------
