{-# OPTIONS_GHC -fno-warn-orphans #-}

-- TODO: It would be nice to not to have an orphan instance here.
-- Find out how is this usually done.

-- TODO: handle (range etc) parameters.

module Handler.ApiV0SugarGet where

import Import
import Yesod.Auth

import Control.Monad      (when)
import Data.Maybe         (fromJust)
import Network.HTTP.Types (status401)

import Betty.Model        (BGUnit (..))
import Data.Text          (pack)

instance ToJSON BloodGlucoseHistory where
    toJSON (BloodGlucoseHistory uid utctime date time tz value unit notes)
        = object [ "uid"       .= uid
                 , "utctime"   .= utctime
                 -- , "localtime" .= localtime
                 , "date"      .= show date
                 , "time"      .= show time
                 , "tz"        .= tz
                 , "value"     .= value
                 , "unit"      .= format unit
                 , "notes"     .= notes
                 ]
        where
            format :: Maybe BGUnit -> String
            format (Just MgDL) = show MgDL
            format (Just Mmol) = show Mmol
            format Nothing     = "unknown"

getApiV0SugarGetR :: Handler Value
getApiV0SugarGetR = do
    mid <- maybeAuthId

    when (mid == Nothing) $ sendResponseStatus status401 ()

    let uid = fromJust mid

    $(logDebug) $ pack $ "uid: " ++ show uid

    sugars <- fmap (map entityVal) $
              runDB $ selectList [BloodGlucoseHistoryUid ==. uid][LimitTo 10]

    $(logDebug) $ pack $ "Returning " ++
            (show $ object [ "count"  .= length sugars
                           , "sugars" .= sugars
                           ])

    return $ object [ "count"  .= length sugars
                    , "sugars" .= sugars
                    ]

