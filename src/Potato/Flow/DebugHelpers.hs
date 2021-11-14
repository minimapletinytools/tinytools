module Potato.Flow.DebugHelpers where

import           Relude

class PotatoShow a where
  potatoShow :: a -> Text

assertAndDump :: (HasCallStack, PotatoShow a) => a -> Bool -> b -> b
assertAndDump a v b = if v
  then b
  else error $ "assert failed:\n" <> potatoShow a
