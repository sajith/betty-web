{-# LANGUAGE RankNTypes #-}

module Betty.Text (txt) where

import ClassyPrelude.Yesod

------------------------------------------------------------------------

-- TODO: move this somewhere else, or use one of the packages
-- (string-conversions, string-conv, string-convert) described here:
-- http://www.stephendiehl.com/posts/production.html (See section
-- "strings")
txt :: forall a. Show a => a -> Text
txt = pack . show

------------------------------------------------------------------------

