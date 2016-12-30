module Handler.ProfileBGUnitSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $

    describe "test /profile/bgunit" $ do
        it "get /profile/bgunit without authorization" $ do
            get ProfileBGUnitR
            statusIs 405

        it "post /profile/bgunit without authorization, with CSRF token" $ do

            -- When `defaultCsrfMiddleware` is enabled, `needsLogin`
            -- won't work as intended, because then it will receive a
            -- 403, not an auth redirect.  Maybe there is a valid test
            -- case to be had from this.

            -- needsLogin POST ProfileBGUnitR

            -- We need a cookie for `addTokenFromCookie` to work,
            -- hence this GET request.
            get ProfileBGUnitR

            request $ do
                addTokenFromCookie
                setMethod "POST"
                setUrl ProfileBGUnitR

            -- Result should be a redirect.
            statusIs 303

        it "post /profile/bgunit without authorization, no CSRF token" $ do
            request $ do
                setMethod "POST"
                setUrl ProfileBGUnitR

            -- result should be 403 forbidden.
            statusIs 403
            bodyContains "<title>Permission Denied</title>"
            bodyContains "<h1>Permission denied</h1>"

        -- TODO: other tests
