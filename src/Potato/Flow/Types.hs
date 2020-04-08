module Potato.Flow.Types (
  PChar
  , Renderer(..)
  , LRaycast
) where

import           Relude

import           Potato.Flow.Math

-- so we can switch to utf-8 or whatever later easily
type PChar = Char

data Renderer = Renderer LBox (LPoint -> Maybe PChar)

type LRaycast = LPoint -> Bool
