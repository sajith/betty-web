{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- TODO: It would be nice to not to have an orphan instance here.
-- Find out how is this usually done.

-- TODO: handle (range etc) parameters.

module Handler.Api.V0.Sugar where

-- TODO: resolve `parseTime` origin correctly.

import           Import               hiding (parseTime)

import qualified Data.Text            as T
import           Data.Time.LocalTime  (timeToTimeOfDay)

import           Database.Persist.Sql (fromSqlKey)

import           Yesod.Form.Fields    (parseTime)

import           Betty.Helpers        (sendJson)
import           Betty.Model          (BGUnit (..))

-- TODO: avoid this use of `read`.
import           Prelude              (read)

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

    $(logDebug) ("Record: " <> tshow record)

    result <- runDB $ insert record

    let key = fromSqlKey result

    $(logDebug) ("Result key: " <> tshow key)

    sendJson status200 "OK"

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
            format (Just u) = show u
            format Nothing  = "unknown"

------------------------------------------------------------------------

getApiV0SugarGetR :: Handler TypedContent
getApiV0SugarGetR = do

    uid <- requireAuthId

    $(logDebug) ("uid: " <> tshow uid)

    sugars <- fmap (map entityVal) $
              runDB $ selectList [BloodGlucoseHistoryUid ==. uid][LimitTo 10]

    $(logDebug) ("Returning " <> tshow (length sugars) <> " records")

    -- With `selectRep` and `provideRep`, we could return other
    -- representations (XML, for example) if we want to.  Neat.
    selectRep $ do
        provideRep $ return $ object [ "count"  .= length sugars
                                     , "sugars" .= sugars
                                     ]

------------------------------------------------------------------------

putApiV0SugarEditR :: Handler Html
putApiV0SugarEditR = error "Not yet implemented: putApiV0SugarEditR"

------------------------------------------------------------------------

deleteApiV0SugarDeleteR :: Handler Html
deleteApiV0SugarDeleteR = error "Not yet implemented: deleteApiV0SugarDeleteR"

------------------------------------------------------------------------
