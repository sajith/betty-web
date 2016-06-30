module Betty.Pid ( writePidFile ) where

------------------------------------------------------------------------
--
-- Write process ID somewhere in the filesystem so that monitoring
-- apps can monitor.  If "/opt/keter/var" is writable or can be
-- created, write betty.pid there; do not write anything otherwise.
--
-- TODO: Ignoring failures silently is not a good idea! Fix.
--
------------------------------------------------------------------------

import ClassyPrelude.Yesod

import System.Directory     (createDirectoryIfMissing)
import System.IO            (IOMode (WriteMode), hPrint, withFile)
import System.Posix.Process (getProcessID)

------------------------------------------------------------------------

writePidFile :: IO ()
writePidFile = do

    let pidDir  = "/opt/keter/var"
        pidFile = pidDir </> "betty.pid"

    catchAny (do createDirectoryIfMissing True pidDir
                 pid <- getProcessID
                 withFile pidFile WriteMode $ \h -> hPrint h pid)
        (const $ return ())

------------------------------------------------------------------------
