module Handler.ResetToken where

import Import

import Betty.Token
import System.Random (newStdGen)
import Yesod.Auth    (requireAuth)

-- TODO: write tests

postResetTokenR :: Handler Html
postResetTokenR = do

    Entity k u <- requireAuth
    let email = userEmail u

    -- generate new api key
    g <- lift newStdGen
    token <- lift $ makeToken g

    $(logDebug) ("postResetTokenR: email: " <> email
                 <> ",token:" <> token <> "\n")

    -- update database
    _ <- setToken k email token

    -- return to /profile page.
    redirect ProfileR
