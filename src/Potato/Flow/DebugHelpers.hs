module Potato.Flow.DebugHelpers where

import           Relude

-- prob just replace with show? Why do you ever want to not potato show?
class PotatoShow a where
  potatoShow :: a -> Text


assertShowAndDump  :: (HasCallStack, Show a) => a -> Bool -> b -> b
assertShowAndDump a v b = if v
  then b
  else error $ "assert failed:\n" <> show a


assertPotatoShowAndDump :: (HasCallStack, PotatoShow a) => a -> Bool -> b -> b
assertPotatoShowAndDump a v b = if v
  then b
  else error $ "assert failed:\n" <> potatoShow a
