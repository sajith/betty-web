module Handler.HistoryBG where

import Import

------------------------------------------------------------------------

getHistoryBGR :: Handler Html
getHistoryBGR = do
    Entity uid _ <- requireAuth

    -- TODO: pagination
    sugars <- fmap (map entityVal) $
              runDB $ selectList [BloodGlucoseHistoryUid ==. uid] [LimitTo 10]

    let bloodGlucoseHistoryDate _ = "TODO" :: Text -- TODO
        bloodGlucoseHistoryTime _ = "TODO" :: Text -- TODO

    defaultLayout $ do
        setTitle "Betty : Blood Sugar Logs"
        $(widgetFile "bg.history")

------------------------------------------------------------------------
