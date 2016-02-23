module Handler.ResetToken where

import Import

-- TODO: we should not have to use this.
#if __GLASGOW_HASKELL__ > 704
import Data.Monoid   ((<>))
#endif

import Betty.Token   (makeToken, setToken)
import Yesod.Auth    (requireAuth)

-- TODO: write tests

postResetTokenR :: Handler Html
postResetTokenR = do

    -- no representation without authorization
    Entity k u <- requireAuth
    let email = userEmail u

    -- generate new api key
    token <- lift makeToken

    $(logDebug) ("postResetTokenR: email: " <> email
                 <> ",token:" <> token <> "\n")

    -- update database
    _ <- setToken k email token

    -- return to /profile page.
    redirect ProfileR
