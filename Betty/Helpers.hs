{-# LANGUAGE RankNTypes #-}

module Betty.Helpers
       ( entityUserEmail
       , entityUserEmail'
       , projectName
       , sendJson
       ) where

import ClassyPrelude.Yesod
import Model

import Data.Text.Encoding  (decodeLatin1)

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

------------------------------------------------------------------------

sendJson :: forall (m :: * -> *).
            (MonadHandler m, MonadLogger m) =>
            Status -> Text -> m Value
sendJson status hint = do

    $(logDebug) hint

    -- _ <- sendResponseStatus status message

    let code    = statusCode status
        message = decodeLatin1 $ statusMessage status
        json    = object [ "code"    .= code
                         , "message" .= message
                         , "hint"    .= hint
                         ]

    sendStatusJSON status json

------------------------------------------------------------------------
