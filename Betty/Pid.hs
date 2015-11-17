module Betty.Pid
       (
           writePidFile
       ) where

import Import

import Control.Monad        (when)
import System.Directory     (createDirectoryIfMissing)
import System.IO            (IOMode (WriteMode), hPrint, withFile)
import System.Posix.Process (getProcessID)

------------------------------------------------------------------------

-- Write a PID file so that monitoring apps can monitor.
writePidFile :: IO ()
writePidFile = do
    let pidDir = if production
                  then "/opt/keter/var/"
                  else "./"

    when production $
        createDirectoryIfMissing True pidDir

    let pidFile = pidDir ++ "betty.pid"

    pid <- getProcessID

    withFile pidFile WriteMode $ \handle ->
        hPrint handle pid

------------------------------------------------------------------------
