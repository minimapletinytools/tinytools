module Potato.Flow.DebugHelpers where

import           Relude

-- prob just replace with show? Why do you ever want to not potato show?
-- the reason is becaues potatoshow doesn't show all information, but whatever it's fine
class PotatoShow a where
  potatoShow :: a -> Text

showFoldable :: (Foldable f, Show a) => f a -> Text
showFoldable = foldl' (\acc x -> acc <> show x <> "\n") ""

assertShowAndDump  :: (HasCallStack, Show a) => a -> Bool -> b -> b
assertShowAndDump a v b = if v
  then b
  else error $ "assert failed:\n" <> show a

assertPotatoShowAndDump :: (HasCallStack, PotatoShow a) => a -> Bool -> b -> b
assertPotatoShowAndDump a v b = if v
  then b
  else error $ "assert failed:\n" <> potatoShow a
