module Handler.ResetToken where

import Import

import Betty.Token (setNewToken)

------------------------------------------------------------------------

-- TODO: write tests

postResetTokenR :: Handler Html
postResetTokenR = do

    -- no representation without authorization
    uid <- requireAuthId

    -- generate and set new token.
    setNewToken uid

    -- return to /profile page.
    redirect ProfileR

------------------------------------------------------------------------
