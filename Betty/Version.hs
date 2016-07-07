module Betty.Version where

------------------------------------------------------------------------

import ClassyPrelude.Yesod

import Data.Version        (showVersion)
import Development.GitRev  (gitHash)
import Paths_betty         (version)

------------------------------------------------------------------------

appVersion :: Text
appVersion = pack $ showVersion version

------------------------------------------------------------------------

gitCommitHash :: Text
gitCommitHash = pack $ take 7 $(gitHash)

------------------------------------------------------------------------
