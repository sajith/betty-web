module Handler.ProfileBGUnitSpec (spec) where

import TestImport
import TestTools

spec :: Spec
spec = withApp $

    describe "test /profile/bgunit" $ do
        it "get /profile/bgunit without authorization" $ do
            get ProfileBGUnitR
            statusIs 405

        it "post /profile/bgunit without authorization" $
            needsLogin POST ProfileBGUnitR

        -- TODO: other tests
