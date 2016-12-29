{-# LANGUAGE NoDisambiguateRecordFields, NoRecordWildCards #-}

module Handler.Info.Contact where

import Import

------------------------------------------------------------------------

getContactR :: Handler Html
getContactR = defaultLayout $ do
    setTitle "Betty : Contact"
    $(widgetFile "info.contact")

------------------------------------------------------------------------
