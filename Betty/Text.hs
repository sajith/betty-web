{-# LANGUAGE RankNTypes #-}

module Betty.Text (txt) where

import Data.Text (Text, pack)
import Prelude

------------------------------------------------------------------------

-- TODO: move this somewhere else.
txt :: forall a. Show a => a -> Text
txt = pack . show

------------------------------------------------------------------------

