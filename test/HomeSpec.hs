module HomeSpec
    ( spec
    ) where

import TestImport
import TestTools

spec :: Spec
spec = withApp $ do
    describe "test homepage for unauthenticated user" $ do

        it "loads the index and checks it looks right" $ do
            get HomeR
            statusIs 200
            htmlAllContain "h1" "Betty is a blood sugar tracker."

            htmlAnyContain "a" "Betty"
            htmlAnyContain "a" "Log in"
            htmlAnyContain "a" "Register"

            htmlAnyContain "a" "About"
            htmlAnyContain "a" "Contact"
            htmlAnyContain "a" "Terms of Use"
            htmlAnyContain "a" "Privacy Policy"

        it "Requires login" $ do
            needsLogin GET ("/history/bg" :: Text)

{--
            request $ do
                setMethod "POST"
                setUrl HomeR
                addNonce
                fileByLabel "Choose a file" "tests/main.hs" "text/plain" -- talk about self-reference
                byLabel "What's on the file?" "Some Content"

            statusIs 200
            printBody
            htmlCount ".message" 1
            htmlAllContain ".message" "Some Content"
            htmlAllContain ".message" "text/plain"

--}

        -- This is a simple example of using a database access in a test.  The
        -- test will succeed for a fresh scaffolded site with an empty database,
        -- but will fail on an existing database with a non-empty user table.
        it "leaves the user table empty" $ do
            get HomeR
            statusIs 200
            users <- runDB $ selectList ([] :: [Filter User]) []
            assertEqual "user table empty" 0 $ length users

