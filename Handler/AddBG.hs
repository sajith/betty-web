module Handler.AddBG where

import Import
import Yesod.Auth          (requireAuth)

import Data.Text           (pack)
import Data.Time           (Day, TimeOfDay)
import Data.Time.Clock     (getCurrentTime)
import Data.Time.LocalTime (minutesToTimeZone, timeZoneOffsetString)

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

data BGData =
    BGData {
        date    :: Day
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

    utctime <- liftIO getCurrentTime

    let tzs = fmap (pack . timeZoneOffsetString . minutesToTimeZone) $ tz bgdata

    -- Mg/dL is the default blood sugar unit.
    -- TODO: there could be a better way of doing this. Revisit later.
    profile <- runDB $ selectFirst [UserProfileUid ==. uid] []
    let unit = case profile of
            Just p  -> case (userProfileBgunits . entityVal) p of
                Just un -> un
                Nothing -> MgDL
            Nothing -> MgDL

    let record = BloodGlucoseHistory
                 uid
                 utctime         -- utc time
                 -- Nothing      -- local zonedtime
                                 -- TODO: add this field
                 (date bgdata)   -- date as set by client
                 (time bgdata)   -- time as set by client
                 tzs             -- tz as set by client
                 (value bgdata)  -- blood sugar value
                 (Just unit)     -- blood sugar unit
                 (notes bgdata)  -- notes, if any

    _ <- runDB $ insert record

    -- TODO: log more stuff.
    $(logDebug) "[POST] Recorded blood glucose"

    -- TODO: avoid the redirect.
    redirect $ HistoryBGR

