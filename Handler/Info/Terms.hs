module Handler.Terms where

import Import

------------------------------------------------------------------------

getTermsR :: Handler Html
getTermsR = defaultLayout $ do
    setTitle "Betty : Terms of Use"
    $(widgetFile "info.terms")

------------------------------------------------------------------------
