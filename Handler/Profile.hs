module Handler.Profile where

import Import

import Betty.Token (getToken)

------------------------------------------------------------------------

getProfileR :: Handler Html
getProfileR = do
    Entity _ u <- requireAuth

    t <- getToken $ userEmail u
    let token = fromMaybe "not set" t

    defaultLayout $ do
        setTitle "Project D: Your Profile"
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
