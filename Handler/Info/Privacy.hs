module Handler.Info.Privacy where

import Import

------------------------------------------------------------------------

getPrivacyR :: Handler Html
getPrivacyR = defaultLayout $ do
    setTitle "Project D : Privacy Policy"
    $(widgetFile "info.privacy")

------------------------------------------------------------------------
