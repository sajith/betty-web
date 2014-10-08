--
-- Running Setup.hs (like so: "runhaskell Setup.hs build") creates
-- Betty/Version.hs, which has functions to show version and time
-- stamp of the build.  These strings are displayed at the footer of
-- the site.
--
-- Surely enough, 'buildTime' timestamps are approximations at best.
--

import           Control.Monad                   (liftM)
import           Data.Time.Clock                 (getCurrentTime)
import           Distribution.PackageDescription (emptyHookedBuildInfo)
import           Distribution.PackageDescription (HookedBuildInfo)
import           Distribution.Simple
import           Prelude
import           System.Process                  (readProcess)

main :: IO ()
main = defaultMainWithHooks myHooks
    where myHooks = simpleUserHooks { preConf = bettyPreConf }

bettyPreConf :: t -> t1 -> IO HookedBuildInfo
bettyPreConf _ _ = do
    putStrLn "Generating Betty/Version.hs"

    desc <- liftM (filter (/= '\'') . filter (/= '\n')) $
            readProcess "git" ["show", "HEAD", "-s", "--format='%h'"] ""
    now  <- return . show =<< getCurrentTime

    writeFile "Betty/Version.hs" $ unlines
        [ "module Betty.Version where"
        , ""
        , "import Data.String"
        , ""
        , "gitCommitHash :: String"
        , "gitCommitHash = " ++ show (init desc)
        , ""
        , "buildTime :: String"
        , "buildTime = " ++ show now
        ]
    return emptyHookedBuildInfo
