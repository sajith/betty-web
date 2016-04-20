module Handler.ProfileBGUnit where

import Betty.Model
import Import

postProfileBGUnitR :: Handler Html
postProfileBGUnitR = do

    uid <- requireAuthId

    unit <- runInputPost $ ireq textField "bgunits"

    let bgunit = case unit of
            "mgdl" -> Just MgDL
            "mmol" -> Just Mmol
            _      -> Nothing

    when (isNothing bgunit) $
        $(logError) ("User " <> tshow uid <>
                     " chose unknown bg unit selected in settings")

    runDB $ updateWhere [UserProfileUid ==. uid] [UserProfileBgunits =. bgunit]

    redirect ProfileR
