module Betty.Pid ( writePidFile ) where

------------------------------------------------------------------------

import ClassyPrelude.Yesod

import System.Directory     (createDirectoryIfMissing)
import System.IO            (IOMode (WriteMode), hPrint, withFile)
import System.Posix.Process (getProcessID)

------------------------------------------------------------------------

-- Write PID to /opt/keter/var/betty.pid so that monitoring apps can
-- monitor.
writePidFile :: IO ()
writePidFile = do

    -- TODO: hardcoded app name, fix.
    let appDir  = "/opt/keter/var"
        pidFile = appDir </> "betty.pid"

    createDirectoryIfMissing True appDir

    pid <- getProcessID
    withFile pidFile WriteMode $ \h -> hPrint h pid

------------------------------------------------------------------------
