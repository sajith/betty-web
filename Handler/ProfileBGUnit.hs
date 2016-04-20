module Handler.ProfileBGUnit where

import Betty.Model
import Import

------------------------------------------------------------------------

-- TODO: rewrite this; write unit tests.

postProfileBGUnitR :: Handler Html
postProfileBGUnitR = do

    uid <- requireAuthId

    bgunit <- runInputPost $ ireq textField "bgunit"

    $(logDebug) ("uid: " <> tshow uid <> ", bgunit: " <> tshow bgunit)

    let unit = case bgunit of
            "MgdL" -> Just MgDL
            "Mmol" -> Just Mmol
            _      -> Nothing

    when (isNothing unit) $
        $(logError) ("User " <> tshow uid <> " chose " <>
                     tshow unit <> " from settings")

    runDB $ updateWhere [UserProfileUid ==. uid] [UserProfileBgunits =. unit]

    redirect ProfileR

------------------------------------------------------------------------
