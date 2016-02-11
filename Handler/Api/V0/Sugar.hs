{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- TODO: It would be nice to not to have an orphan instance here.
-- Find out how is this usually done.

-- TODO: handle (range etc) parameters.

module Handler.Api.V0.Sugar where

import           Import

import qualified Data.Text            as T
import           Data.Time.Clock      (getCurrentTime, utctDay, utctDayTime)
import           Data.Time.LocalTime  (timeToTimeOfDay)

import           Database.Persist.Sql (fromSqlKey)
import           Yesod.Auth           (requireAuthId)

import           Betty.Model          (BGUnit (..))

------------------------------------------------------------------------

postApiV0SugarAddR :: Handler Value
postApiV0SugarAddR = do

    uid <- requireAuthId

    utctime <- liftIO getCurrentTime

    params <- reqGetParams <$> getRequest

    let value = lookup "value" params :: Maybe Text
        unit  = lookup "unit" params  :: Maybe Text
        date  = lookup "date" params  :: Maybe Text
        time  = lookup "time" params  :: Maybe Text
        tz    = lookup "tz" params    :: Maybe Text
        notes = lookup "notes" params :: Maybe Text

    v' <- case value of
        Nothing -> invalidArgs ["No blood sugar value given."] -- 400 Error
        Just v  -> return v

    unit' <- case unit of
        Nothing -> return MgDL
        Just u | T.toLower u == "mgdl" -> return MgDL
               | T.toLower u == "mmol" -> return Mmol
               | otherwise -> invalidArgs ["Invalid unit."]

    date' <- case date of
        Nothing -> return $ utctDay utctime
        Just d  -> return d'
            where d' = case parseDate $ T.unpack d of
                           Right dt  -> dt
                           Left  _   -> utctDay utctime

    time' <- case time of
        Nothing -> return $ timeToTimeOfDay $ utctDayTime utctime
        Just t  -> return t'
            where t' = case parseTime t of
                           Right tm -> tm
                           Left  _  -> timeToTimeOfDay $ utctDayTime utctime

    let record = BloodGlucoseHistory
                 uid
                 utctime                -- utc time
                 date'                  -- date as set by client
                 time'                  -- time as set by client
                 tz                     -- tz as set by client
                 (read $ T.unpack v')   -- blood sugar value
                 (Just unit')           -- blood sugar unit
                 notes                  -- notes, if any

    $(logDebug) $ T.pack $ "Record: " ++ show record

    result <- runDB $ insert record

    let key = fromSqlKey result

    $(logDebug) $ T.pack $ "Result key: " ++ show key

    -- TODO: return a useful value.
    -- return $ show key

    return "OK."

------------------------------------------------------------------------

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

------------------------------------------------------------------------

getApiV0SugarGetR :: Handler Value
getApiV0SugarGetR = do

    $(logDebug) "in getApiV0SugarGetR"

    uid <- requireAuthId

    $(logDebug) $ T.pack $ "uid: " ++ show uid

    sugars <- fmap (map entityVal) $
              runDB $ selectList [BloodGlucoseHistoryUid ==. uid][LimitTo 10]

    $(logDebug) $ T.pack $ "Returning " ++
        show (object [ "count"  .= length sugars
                     , "sugars" .= sugars
                     ])

    return $ object [ "count"  .= length sugars
                    , "sugars" .= sugars
                    ]

------------------------------------------------------------------------

putApiV0SugarEditR :: Handler Html
putApiV0SugarEditR = error "Not yet implemented: putApiV0SugarEditR"

------------------------------------------------------------------------

deleteApiV0SugarDeleteR :: Handler Html
deleteApiV0SugarDeleteR = error "Not yet implemented: deleteApiV0SugarDeleteR"

------------------------------------------------------------------------
