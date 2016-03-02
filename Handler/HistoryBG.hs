module Handler.HistoryBG where

import Import

------------------------------------------------------------------------

getHistoryBGR :: Handler Html
getHistoryBGR = do
    Entity uid _ <- requireAuth

    -- TODO: pagination
    sugars <- fmap (map entityVal) $
              runDB $ selectList [BloodGlucoseHistoryUid ==. uid] [LimitTo 10]

    defaultLayout $ do
        setTitle "Project D : Blood Sugar Logs"
        $(widgetFile "bg.history")

------------------------------------------------------------------------
