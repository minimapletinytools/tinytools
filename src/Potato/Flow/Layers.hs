module Potato.Flow.Layers (
  reindexSEltLayerPosForRemoval
  , reindexSEltLayerPosForInsertion
  , insertElts
  , insertElt
  , removeElts
  , insertEltList
  , removeEltList
  , moveEltList
) where

import           Relude

import           Potato.Flow.Reflex.Types

import           Control.Exception        (assert)
import           Data.List.Ordered        (isSorted)
import           Data.Sequence            ((><))
import qualified Data.Sequence            as Seq



-- | reindexes list of LayerPos such that each element is indexed as if all previous elements have been removed
-- O(n^2) lol
reindexSEltLayerPosForRemoval :: [LayerPos] -> [LayerPos]
reindexSEltLayerPosForRemoval [] = []
reindexSEltLayerPosForRemoval (r:xs) = r:reindexSEltLayerPosForRemoval rest where
  -- if this asserts that means you tried to remove the same index twice
  rest = map (\x -> assert (x /= r) $ if x > r then x-1 else x) xs

-- | inverse of reindexSEltLayerPosForRemoval
-- input indices are before any elements are inserted
-- O(n^2) lol
reindexSEltLayerPosForInsertion :: [LayerPos] -> [LayerPos]
reindexSEltLayerPosForInsertion = reverse . reindexSEltLayerPosForRemoval . reverse

-- | inserts ys at index i into xs
insertElts :: Int -> Seq a -> Seq a -> Seq a
insertElts i ys xs = newSeq where
  (l, r) = Seq.splitAt i xs
  newSeq = l >< ys >< r

-- | inserts y at index y into xs
insertElt :: Int -> a -> Seq a -> Seq a
insertElt i y xs = insertElts i (Seq.singleton y) xs

-- | removes n elts at index i from xs
removeElts :: Int -> Int -> Seq a -> Seq a
removeElts n i xs = newSeq where
  (keepl  , rs) = Seq.splitAt i xs
  (_, keepr) = Seq.splitAt n rs
  newSeq           = keepl >< keepr

-- | removes elt at index i from xs
removeElt :: Int -> Seq a -> Seq a
removeElt i xs = Seq.deleteAt i xs

-- | inserts ys into xs, positions are after insertion
insertEltList :: [(Int, a)] -> Seq a -> Seq a
insertEltList ys xs = newSeq where
  is' = map fst ys
  elts = map snd ys
  is = reindexSEltLayerPosForInsertion is'
  newSeq = foldr (uncurry insertElt) xs (zip is elts)

-- | removes is' from xs, positions are before removal
removeEltList :: [Int] -> Seq a -> Seq a
removeEltList is' xs = newSeq where
  is = reindexSEltLayerPosForRemoval is'
  newSeq = foldl' (flip removeElt) xs is

-- | moves all elts, new position is before removal, ys must be sorted
moveEltList :: [Int] -> Int -> Seq a -> Seq a
moveEltList is i xs = assert (isSorted is) $ newSeq where
  nBefore = length . filter (< i) $ is
  ys = map (Seq.index xs) is
  newSeq' = removeEltList is xs
  newSeq = insertElts (i-nBefore) (Seq.fromList ys) newSeq'
