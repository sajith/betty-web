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
    -- TODO: log errors.
    -- TODO: check permissions for appDir.
    -- TODO: hardcoded app name, fix.

    appDir <- getAppUserDataDirectory "betty" `catch` handleEx

    let pidFile = appDir </> "betty.pid"

    createDirectoryIfMissing True appDir

    pid <- getProcessID
    withFile pidFile WriteMode $ \h -> hPrint h pid

    where handleEx e
              | isDoesNotExistError e = do
                    -- $(logError) "getAppUserDataDirectory failed with"
                    --     <> "isDoesNotExistError. "
                    --     <> "Using /home/betty/.betty as the fall back."
                    return "/home/betty/.betty"
              | otherwise             = do
                    -- $(logError) "getAppUserDataDirectory failed! "
                    --     <> " (UnsupportedOperation, perhaps)"
                    --     <> "Using /tmp/betty as the fall back."
                    return "/tmp/betty"

------------------------------------------------------------------------
