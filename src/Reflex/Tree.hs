module Reflex.Tree (
  Tree(..)
  , Forest
) where

import           Relude

import           Reflex

-- placeholders
data Full
data Zipper t r a

type Forest t a = Dynamic t [Tree t a]

data Tree t a = Node {
  rootLabel   :: a
  , subForest :: Forest t a
  -- zipper to this node, always contains the full tree
  , zipper    :: Behavior t (Zipper t Full a)
}

-- TODO some way to initialize tree with a zipper all at once
