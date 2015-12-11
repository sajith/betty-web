module Handler.Profile where

import Import

import Betty.Token (getToken)
import Yesod.Auth  (requireAuth)

------------------------------------------------------------------------

getProfileR :: Handler Html
getProfileR = do
    Entity _ u <- requireAuth
    token <- getToken $ userEmail u

    defaultLayout $ do
        setTitle "Project D: Your Profile"
        $(widgetFile "profile")
        $(widgetFile "profile.password")
        $(widgetFile "profile.email")
        $(widgetFile "profile.timezone")
        $(widgetFile "profile.units")
        $(widgetFile "profile.apikey")
        $(widgetFile "profile.delete")

------------------------------------------------------------------------

postProfileR :: Handler Html
postProfileR = error "Not yet implemented: postProfileR"

------------------------------------------------------------------------
