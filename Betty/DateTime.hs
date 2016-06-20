-- | Date and time formatting routines.

module Betty.DateTime where

import Data.Time
import Import

------------------------------------------------------------------------

-- See `formatTime` documentation for format specifiers.

dateFormat :: String
dateFormat = "%e %b %Y"

dateFormatTZ :: String
dateFormatTZ = "%e %b %Y %Z"

timeFormat :: String
timeFormat = "%r"

timeFormatTZ :: String
timeFormatTZ = "%r %Z"

dateTimeFormat :: String
dateTimeFormat = "%e %b %Y %H:%M:%S"

dateTimeFormatTZ :: String
dateTimeFormatTZ = "%e %b %Y %H:%M:%S %Z"

------------------------------------------------------------------------

formatDay :: Day -> String
formatDay day = formatTime defaultTimeLocale dateFormat t
    where
        t :: UTCTime
        t = UTCTime day 0

formatDayTZ :: Day -> TimeLocale -> String
formatDayTZ day tz = formatTime tz dateFormat t
    where
        t :: UTCTime
        t = UTCTime day 0

------------------------------------------------------------------------

formatUTCTime :: UTCTime -> String
formatUTCTime = formatTime defaultTimeLocale timeFormat

formatUTCTimeTZ :: UTCTime -> TimeLocale -> String
formatUTCTimeTZ tm tz = formatTime tz timeFormat tm

------------------------------------------------------------------------
