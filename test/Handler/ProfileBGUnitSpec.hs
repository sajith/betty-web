module Handler.ProfileBGUnitSpec (spec) where

import TestImport
import TestTools

spec :: Spec
spec = withApp $ do

    describe "test /profile/bgunit" $ do
        it "get /profile/bgunit without authorization" $ do
            get ProfileBGUnitR
            statusIs 405

        it "post /profile/bgunit without authorization" $ do
            needsLogin POST ProfileBGUnitR

        -- TODO: other tests
