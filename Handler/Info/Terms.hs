{-# LANGUAGE NoDisambiguateRecordFields, NoRecordWildCards #-}

module Handler.Info.Terms where

import Import

------------------------------------------------------------------------

getTermsR :: Handler Html
getTermsR = defaultLayout $ do
    setTitle "Betty : Terms of Use"
    $(widgetFile "info.terms")

------------------------------------------------------------------------
