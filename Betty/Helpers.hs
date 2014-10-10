module Betty.Helpers
       ( entityUserEmail
       , entityUserEmail'
       , projectName
       ) where

import Data.Text (Text)
import Model
import Prelude
import Yesod

------------------------------------------------------------------------

-- TODO: Use this everywhere instead of hardcoding.
projectName :: Text
projectName = "Betty"

------------------------------------------------------------------------

-- Use this to extract email when calling requireAuth
entityUserEmail :: Entity User -> Text
entityUserEmail (Entity _ u) = userEmail u

------------------------------------------------------------------------

-- Use this to extract email when calling maybeAuth
entityUserEmail' :: Maybe (Entity User) -> Text
entityUserEmail' eu = case eu of
    Just u  -> entityUserEmail u
    Nothing -> "email unknown"

