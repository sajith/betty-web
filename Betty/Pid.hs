module Betty.Pid
       (
           writePidFile
       ) where

import Import

import Control.Monad             (when)
import Filesystem                (createTree)
import Filesystem.Path.CurrentOS (directory)
import System.IO                 (IOMode (WriteMode), hPrint, withFile)
import System.Posix.Process      (getProcessID)

------------------------------------------------------------------------

-- Write a PID file so that monitoring apps can monitor.
writePidFile :: IO ()
writePidFile = do
    when production $
        createTree $ directory "/opt/keter/var/"

    let pidFile = if production
                  then "/opt/keter/var/betty.pid"
                  else "betty.pid"

    pid <- getProcessID

    withFile pidFile WriteMode $ \handle ->
        hPrint handle pid

------------------------------------------------------------------------
