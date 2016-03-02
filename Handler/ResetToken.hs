module Handler.ResetToken where

import Import

import Betty.Token (newToken, setToken)

-- TODO: write tests

postResetTokenR :: Handler Html
postResetTokenR = do

    -- no representation without authorization
    Entity k u <- requireAuth
    let email = userEmail u

    -- generate new api key
    token <- lift newToken

    $(logDebug) ("postResetTokenR: email: " <> email
                 <> ",token:" <> token <> "\n")

    -- update database
    _ <- setToken k email token

    -- return to /profile page.
    redirect ProfileR
