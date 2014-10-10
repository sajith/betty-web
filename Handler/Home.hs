{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Handler.Home where

import Import
import Yesod.Auth           (maybeAuth, requireAuth)
import Yesod.Default.Config (appExtra)

getHomeR :: Handler Html
getHomeR = do
    ma <- maybeAuth
    case ma of
        Just _  -> loggedInPage
        Nothing -> loggedOutPage

loggedOutPage :: Handler Html
loggedOutPage = defaultLayout $ do
    master <- getYesod
    setTitle "Betty: Welcome!"
    $(widgetFile "homepage")

loggedInPage :: Handler Html
loggedInPage = do
    Entity uid u <- requireAuth

    defaultLayout $ do
        setTitle "Betty: Welcome!"
        $(widgetFile "homepage.signed-in")

