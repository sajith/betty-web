module Handler.Info.Privacy where

import Import

------------------------------------------------------------------------

getPrivacyR :: Handler Html
getPrivacyR = defaultLayout $ do
    setTitle "Betty : Privacy Policy"
    $(widgetFile "info.privacy")

------------------------------------------------------------------------
