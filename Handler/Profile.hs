module Handler.Profile where

import Import
import Yesod.Auth (requireAuth)

------------------------------------------------------------------------

getProfileR :: Handler Html
getProfileR = do
    Entity _ u <- requireAuth

    defaultLayout $ do
        setTitle "Project D: Your Profile"
        $(widgetFile "profile")
        $(widgetFile "profile.password")
        $(widgetFile "profile.email")
        $(widgetFile "profile.timezone")
        $(widgetFile "profile.units")
        $(widgetFile "profile.delete")

------------------------------------------------------------------------

postProfileR :: Handler Html
postProfileR = error "Not yet implemented: postProfileR"

------------------------------------------------------------------------
