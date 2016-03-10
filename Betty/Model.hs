{-# LANGUAGE DeriveGeneric #-}

module Betty.Model where

import ClassyPrelude.Yesod

------------------------------------------------------------------------

-- Blood glucose units.
data BGUnit = MgDL | Mmol
            deriving (Read, Eq, Enum, Bounded, Generic)

derivePersistField "BGUnit"

instance FromJSON BGUnit
instance ToJSON BGUnit

instance Show BGUnit where
    show MgDL = "mg/dL"
    show Mmol = "mmol"

------------------------------------------------------------------------

-- Weight units.
data WtUnit = Kg | Lb
            deriving (Read, Eq, Enum, Bounded, Generic)

derivePersistField "WtUnit"

instance FromJSON WtUnit
instance ToJSON WtUnit

instance Show WtUnit where
    show Kg = "kilogram"
    show Lb = "pound"

------------------------------------------------------------------------
