module Reflex.Tree (
  Tree(..)
  , Forest
) where

import           Relude

import           Reflex


type Forest t a = Dynamic t [Tree t a]

data Tree t a = Node {
  rootLabel   :: a
  , subForest :: Forest t a

  -- TODO delete this, because we can't write fmap instance that respects functor laws
  , parent    :: Maybe (Tree t a)
}

{-
mapSetParent :: Functor (Dynamic t) => Maybe (Tree t a) -> (a -> b) -> Tree t a -> Tree t a
mapSetParent p f n = r where
  r = Node {
      rootLabel = f (rootLabel n)
      , subForest = fmap . fmap (mapSetParent r)
      , parent = p
    }

-- Note that fmap breaks parent relations D:
-- you could also just not make this functor instance but still have an fmap like function
-- OR if you add Top/NotTop type flag, then only Top is fmap instance, that works too
instance Functor (Dynamic t) => Functor (Tree t a) where
  fmap f a = r where
    r = mapSetParent Nothing f a
-}
