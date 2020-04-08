module Potato.Flow.Reflex.Tree (
  Tree(..)
  , Forest
) where

import           Relude

import           Reflex

type Forest t a = Dynamic t [Tree t a]

data Tree t a = Node {
  rootLabel   :: a
  , subForest :: Forest t a
}
