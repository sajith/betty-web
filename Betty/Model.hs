module Betty.Model where

import ClassyPrelude.Yesod

------------------------------------------------------------------------

-- Blood glucose units.
data BGUnit = MgDL | Mmol
            deriving (Show, Read, Eq, Enum, Bounded)

derivePersistField "BGUnit"

------------------------------------------------------------------------

-- Weight units.
data WtUnit = Kg | Lb
     deriving (Show, Read, Eq, Enum, Bounded)

derivePersistField "WtUnit"

------------------------------------------------------------------------
