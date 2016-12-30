module Handler.Home where

-- TODO: use maximum from Import
import Data.List    (maximum)
import Import       hiding (maximum)
import Numeric      (showFFloat)

import Betty.Vendor

------------------------------------------------------------------------

getHomeR :: Handler Html
getHomeR = do
    ma <- maybeAuth
    case ma of
        Just _  -> loggedInPage
        Nothing -> loggedOutPage

------------------------------------------------------------------------

loggedOutPage :: Handler Html
loggedOutPage = defaultLayout $ do
    master <- getYesod
    setTitle "Betty: Welcome!"
    $(widgetFile "homepage")

------------------------------------------------------------------------

loggedInPage :: Handler Html
loggedInPage = do
    Entity uid _ <- requireAuth

    -- TODO: Get the past week's story, not just the past 10 entries
    sugars <- fmap (map entityVal) $
              runDB $
              selectList [BloodGlucoseHistoryUid ==. uid] [LimitTo 10]

    let sns   = map bloodGlucoseHistoryValue sugars
        maxbg = max' sns
        minbg = min' sns
        avgbg = avg' sns

    defaultLayout $ do
        setTitle "Betty: Welcome!"
        $(widgetFile "homepage.signed-in")

        -- TODO: device a better way to handle paths like these.
        addStylesheet $ StaticR timepickerThemeCss
        addStylesheet $ StaticR timepickerCss
        $(widgetFile "bg.entry")

        -- addStylesheet $ StaticR jqPlotCss
        -- addScript $ StaticR jqPlotJs
        -- addScript $ StaticR jqPlotDateAxisJs

        addScript $ StaticR d3Js
        addScript $ StaticR dimpleJs

        let bloodGlucoseHistoryDate _ = "TODO" :: Text -- TODO
            bloodGlucoseHistoryTime _ = "TODO" :: Text -- TODO

        $(widgetFile "bg.trends")
        $(widgetFile "bg.history")

------------------------------------------------------------------------

max' :: (Ord a, Show a) => [a] -> String
max' sv = case sv of
    [] -> "_"
    _  -> show $ maximum sv

min' :: (Ord a, Show a) => [a] -> String
min' sv = case sv of
    [] -> "_"
    _  -> show $ maximum sv

-- showFFloat lets us choose the number of decimal places displayed.
avg' :: RealFloat a => [a] -> String
avg' sv = case sv of
    [] -> "_"
    _  -> showFFloat (Just 2) (sum sv / fromIntegral (length sv)) ""

------------------------------------------------------------------------
