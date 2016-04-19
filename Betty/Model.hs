{-# LANGUAGE DeriveGeneric #-}

module Betty.Model where

import ClassyPrelude.Yesod

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
