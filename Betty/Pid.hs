module Betty.Pid ( writePidFile ) where

------------------------------------------------------------------------

import ClassyPrelude.Yesod
import System.Directory     (createDirectoryIfMissing,
                             getAppUserDataDirectory)
import System.IO            (IOMode (WriteMode), hPrint, withFile)
import System.Posix.Process (getProcessID)

------------------------------------------------------------------------

-- Write PID to $HOME/.betty/betty.pid so that monitoring apps can
-- monitor.
writePidFile :: IO ()
writePidFile = do

    -- TODO: catch errors from System.Directory functions.
    -- TODO: check permissions for appDir.
    -- TODO: hardcoded app name, fix.

    appDir <- getAppUserDataDirectory "betty"
    let pidFile = appDir </> "betty.pid"

    createDirectoryIfMissing True appDir

    pid <- getProcessID
    withFile pidFile WriteMode $ \h -> hPrint h pid

------------------------------------------------------------------------
