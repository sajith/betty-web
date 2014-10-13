module Handler.About where

import Import

------------------------------------------------------------------------

getAboutR :: Handler Html
getAboutR = defaultLayout $ do
    setTitle "Betty : About"
    $(widgetFile "info.about")

------------------------------------------------------------------------
