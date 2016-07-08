module Handler.AddBG where

------------------------------------------------------------------------

import Import

import Data.Time    (TimeOfDay)
-- import Data.Time.LocalTime (minutesToTimeZone, timeZoneOffsetString)

import Betty.Model
import Betty.Vendor

------------------------------------------------------------------------

-- In this handler, I'm choosing an input form over applicative or
-- monadic forms for the moment (although applicative or monadic form
-- would be elegant and better functioning), since that seems to give
-- better control over the UI.

-- TODO: Client side validation.  Monadic or Applicative forms might
-- be the way to go about it, rather than handcoding JS.

------------------------------------------------------------------------

data BGData = BGData { date  :: Day
                     , time  :: TimeOfDay
                     , tz    :: Maybe Int
                     , value :: Double
                     , notes :: Maybe Text
                     } deriving (Show)

------------------------------------------------------------------------

getAddBGR :: Handler Html
getAddBGR = do
    _ <- requireAuth

    defaultLayout $ do
        setTitle "Betty : Add Blood Sugar"

        -- TODO: We don't need of all the JQuery UI bells and whistles, just the
        -- datepicker.  Test a custom build: http://jqueryui.com/download/
        addStylesheet $ StaticR jQueryUiCss

        addStylesheet $ StaticR timepickerCss

        $(widgetFile "bg.entry")

------------------------------------------------------------------------

postAddBGR :: Handler Html
postAddBGR = do

    bgdata <- runInputPost $ BGData
             <$> ireq dayField "date"
             <*> ireq timeFieldTypeTime "time"
             <*> iopt intField "timezone"
             <*> ireq doubleField "value"
             <*> iopt textField "notes"

    Entity uid _ <- requireAuth

    serverTs <- liftIO getCurrentTime

    -- let tzs = (pack . timeZoneOffsetString . minutesToTimeZone) <$> tz bgdata

    let clientTs = error "TODO" -- TODO: form clientTs from date + time
        clientTz = error "TODO" -- TODO: parse timezone correctly.

    -- Mg/dL is the default blood sugar unit.
    -- TODO: there could be a better way of doing this. Revisit later.
    profile <- runDB $ selectFirst [UserProfileUid ==. uid] []
    let unit = case profile of
            Just p  -> fromMaybe MgDL ((userProfileBgunits . entityVal) p)
            Nothing -> MgDL

    let record = BloodGlucoseHistory
                 uid
                 serverTs        -- Server timestamp
                 clientTs        -- Client timestamp
                 clientTz        -- Client timezone
                 (value bgdata)  -- Blood sugar value
                 (Just unit)     -- Blood sugar unit
                 (notes bgdata)  -- Notes, if any

    _ <- runDB $ insert record

    -- TODO: log more stuff.
    $(logDebug) "[POST] Recorded blood glucose"

    -- TODO: avoid the redirect.
    redirect HistoryBGR

