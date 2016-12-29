{-# LANGUAGE NoDisambiguateRecordFields, NoRecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- TODO: It would be nice to not to have an orphan instance here.
-- Find out how is this usually done.

-- TODO: handle (range etc) parameters.

module Handler.Api.V0.Sugar where

-- TODO: resolve `parseTime` origin correctly.

import           Import               hiding (parseTime)

import qualified Data.Text            as T
-- import           Data.Time.LocalTime  (timeToTimeOfDay)

import           Database.Persist.Sql (fromSqlKey)

-- import           Yesod.Form.Fields    (parseTime)

import           Betty.Helpers        (sendJson)
import           Betty.Model          (BGUnit (..), TZ)

-- TODO: avoid this use of `read`.
import           Prelude              (read)

------------------------------------------------------------------------

postApiV0SugarAddR :: Handler Value
postApiV0SugarAddR = do

    uid <- requireAuthId

    serverTs <- liftIO getCurrentTime

    params <- reqGetParams <$> getRequest

    let value = lookup "value" params     :: Maybe Text
        unit  = lookup "unit" params      :: Maybe Text
        ts    = lookup "timestamp" params :: Maybe Text
        tz    = lookup "timezone" params  :: Maybe Text
        notes = lookup "notes" params     :: Maybe Text

    v' <- case value of
        Nothing -> invalidArgs ["No blood sugar value given."] -- 400 Error
        Just v  -> return v

    unit' <- case unit of
        Nothing -> return MgDL
        Just u | T.toLower u == "mgdl" -> return MgDL
               | T.toLower u == "mmol" -> return Mmol
               | otherwise -> invalidArgs ["Invalid unit."]

    -- TODO: parse client timestamp
    clientTs <- case ts of
                  Nothing -> return serverTs
                  Just _  -> error "TODO"

    -- TODO: parse client timestamp
    clientTz <- case tz of
                  Nothing -> error "TODO"
                  Just _  -> error "TODO"

    -- date' <- case date of
    --     Nothing -> return $ utctDay utctime
    --     Just d  -> return d'
    --         where d' = case parseDate $ T.unpack d of
    --                        Right dt  -> dt
    --                        Left  _   -> utctDay utctime

    -- time' <- case time of
    --     Nothing -> return $ timeToTimeOfDay $ utctDayTime utctime
    --     Just t  -> return t'
    --         where t' = case parseTime t of
    --                        Right tm -> tm
    --                        Left  _  -> timeToTimeOfDay $ utctDayTime utctime

    let record = BloodGlucoseHistory
                 uid                    -- User ID.
                 serverTs               -- Server timestamp
                 clientTs               -- Client timestamp
                 clientTz               -- Client timezone
                 (read $ T.unpack v')   -- Blood sugar value
                 (Just unit')           -- Blood sugar unit
                 notes                  -- Notes, if any

    $(logDebug) ("Record: " <> tshow record)

    result <- runDB $ insert record

    let key = fromSqlKey result

    $(logDebug) ("Result key: " <> tshow key)

    sendJson status200 "OK"

------------------------------------------------------------------------

instance ToJSON BloodGlucoseHistory where
    toJSON (BloodGlucoseHistory uid _ clientTs tz value unit notes)
        = object [ "uid"       .= uid
                 , "timestamp" .= clientTs
                 , "tz"        .= formatTz tz
                 , "value"     .= value
                 , "unit"      .= format unit
                 , "notes"     .= notes
                 ]
        where
            format :: Maybe BGUnit -> String
            format (Just MgDL) = "mg/dL"
            format (Just Mmol) = "mmol/L"
            format Nothing     = "unknown"

            formatTz :: Maybe TZ -> Text
            formatTz = error "TODO"

------------------------------------------------------------------------

--
-- TODO: select by these options:
--
--  * from/until timestamps: if `from` is omitted, default to 7 days
--  from `until`; if `until` is omitted, default to the current time.
--
--
--  * count: the last `n` records.
--
getApiV0SugarGetR :: Handler TypedContent
getApiV0SugarGetR = do

    uid <- requireAuthId

    $(logDebug) ("uid: " <> tshow uid)

    sugars <- fmap (map entityVal) $
              runDB $ selectList [BloodGlucoseHistoryUid ==. uid][LimitTo 10]

    $(logDebug) ("Returning " <> tshow (length sugars) <> " records")

    -- With `selectRep` and `provideRep`, we could return other
    -- representations (XML, for example) if we want to.  Neat.
    selectRep $ provideRep $ return $ object [ "count"  .= length sugars
                                             , "sugars" .= sugars
                                             ]

------------------------------------------------------------------------

putApiV0SugarEditR :: Handler Html
putApiV0SugarEditR = error "Not yet implemented: putApiV0SugarEditR"

------------------------------------------------------------------------

deleteApiV0SugarDeleteR :: Handler Html
deleteApiV0SugarDeleteR = error "Not yet implemented: deleteApiV0SugarDeleteR"

------------------------------------------------------------------------
