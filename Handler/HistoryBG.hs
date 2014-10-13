module Handler.HistoryBG where

import Import
import Yesod.Auth (requireAuth)

------------------------------------------------------------------------

getHistoryBGR :: Handler Html
getHistoryBGR = do
    Entity uid u <- requireAuth

    -- TODO: pagination
    sugars <- fmap (map entityVal) $
              runDB $ selectList [BloodGlucoseHistoryUid ==. uid] [LimitTo 10]

    defaultLayout $ do
        setTitle "Project D : Blood Sugar Logs"
        $(widgetFile "bg.history")

------------------------------------------------------------------------
