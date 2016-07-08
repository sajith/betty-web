{-# LANGUAGE DeriveGeneric #-}

module Betty.Model where

import ClassyPrelude.Yesod

import Data.Time.LocalTime (TimeZone)

------------------------------------------------------------------------

-- Blood glucose units.
data BGUnit = MgDL | Mmol
            deriving (Show, Read, Eq, Enum, Bounded, Generic)

derivePersistField "BGUnit"

instance FromJSON BGUnit
instance ToJSON BGUnit

------------------------------------------------------------------------

-- Weight units.
data WtUnit = Kg | Lb
            deriving (Show, Read, Eq, Enum, Bounded, Generic)

derivePersistField "WtUnit"

instance FromJSON WtUnit
instance ToJSON WtUnit

------------------------------------------------------------------------

-- A custome timezone field to replace ZonedTime, which has been
-- deprecated as of Persistent 2.0.  We'll use UTCTime + TZ instead of
-- ZonedTime.
newtype TZ = TZ TimeZone
             deriving (Show, Read, Eq, Generic)

derivePersistField "TZ"

------------------------------------------------------------------------
