module Handler.ResetToken where

import Import

import Betty.Token (newToken, setToken)

------------------------------------------------------------------------

-- TODO: write tests

postResetTokenR :: Handler Html
postResetTokenR = do

    -- no representation without authorization
    uid <- requireAuthId

    -- generate new api key
    token <- lift newToken

    $logDebug ("postResetTokenR: uid: " <> tshow (unUserKey uid)
               <> ", new token:" <> token)

    -- update database
    _ <- setToken uid token

    -- return to /profile page.
    redirect ProfileR

------------------------------------------------------------------------
