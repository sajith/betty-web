module Betty.Model where

import Database.Persist.TH
import Prelude

-- Blood glucose units.
data BGUnit = MgDL | Mmol
            deriving (Show, Read, Eq, Enum, Bounded)

derivePersistField "BGUnit"

data WtUnit = Kg | Lb
     deriving (Show, Read, Eq, Enum, Bounded)

derivePersistField "WtUnit"

