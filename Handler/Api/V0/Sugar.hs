{-# OPTIONS_GHC -fno-warn-orphans #-}

-- TODO: It would be nice to not to have an orphan instance here.
-- Find out how is this usually done.

-- TODO: handle (range etc) parameters.

module Handler.Api.V0.Sugar where

import Import
import Yesod.Auth           (maybeAuthId, AuthId(..))

import qualified Data.Text            as T
import Data.Time.Clock      (getCurrentTime, utctDay, utctDayTime)
import Data.Time.LocalTime  (timeToTimeOfDay)

import Database.Persist.Sql (fromSqlKey)

import Control.Monad        (when)
import Data.Maybe           (fromJust, isNothing)
import Network.HTTP.Types   (status401)

import Betty.Model          (BGUnit (..))

------------------------------------------------------------------------

postApiV0SugarAddR :: Handler Value
postApiV0SugarAddR = do

    mid <- maybeAuthId

    when (isNothing mid) $ sendResponseStatus status401 ()

    let uid = fromJust mid

    -- value <- lookupPostParam "value"

    -- -- TODO: use defaults for absent parameters.
    -- v' <- case value of
    --           -- no value param, return 400 error.
    --           Nothing -> invalidArgs [""]
    --           Just v  -> return v

    utctime <- liftIO getCurrentTime

    params <- reqGetParams <$> getRequest

    let value = lookup "value" params :: Maybe Text
        unit  = lookup "unit" params  :: Maybe Text
        date  = lookup "date" params  :: Maybe Text
        time  = lookup "time" params  :: Maybe Text
        tz    = lookup "tz" params    :: Maybe Text
        notes = lookup "notes" params :: Maybe Text

    v' <- case value of
        Nothing -> invalidArgs ["No value given."] -- 400 Error
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

getApiV0SugarGetR :: Handler Value
getApiV0SugarGetR = do
    mid <- maybeAuthId

    when (isNothing mid) $ sendResponseStatus status401 ()

    let uid = fromJust mid

    -- uid <- checkUid

    $(logDebug) $ T.pack $ "uid: " ++ show uid

    sugars <- fmap (map entityVal) $
              runDB $ selectList [BloodGlucoseHistoryUid ==. uid][LimitTo 10]

    $(logDebug) $ T.pack $ "Returning " ++
        show object [ "count"  .= length sugars
                    , "sugars" .= sugars
                    ]

    return $ object [ "count"  .= length sugars
                    , "sugars" .= sugars
                    ]


-- TODO: test this later.

checkUid :: -- forall master.
            (YesodAuth master, Eq (AuthId master)) =>
            HandlerT master IO (AuthId master)
checkUid = do
    mid <- maybeAuthId
    when (isNothing mid) $ sendResponseStatus status401 ()
    return $ fromJust mid

------------------------------------------------------------------------

putApiV0SugarEditR :: Handler Html
putApiV0SugarEditR = error "Not yet implemented: putApiV0SugarEditR"

------------------------------------------------------------------------

deleteApiV0SugarDeleteR :: Handler Html
deleteApiV0SugarDeleteR = error "Not yet implemented: deleteApiV0SugarDeleteR"

------------------------------------------------------------------------
