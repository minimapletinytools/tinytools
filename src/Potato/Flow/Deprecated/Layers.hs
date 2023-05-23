-- DEPRECATED, we will switch to Owl :O

module Potato.Flow.Deprecated.Layers (
  reindexSEltLayerPosForRemoval
  , reindexSEltLayerPosForInsertion
  , hasScopingProperty
  , selectionHasScopingProperty
  , findMatchingScope
  , scopeSelection
  , insertElts
  , insertElt
  , removeElts
  , insertEltList_indexBeforeInsertion
  , insertEltList_indexAfterInsertion
  , removeEltList
  , moveEltList
  , undoMoveEltList
) where

import           Relude

import           Potato.Flow.Types

import           Control.Exception (assert)
import qualified Data.Bimap        as BM
import           Data.List.Ordered (isSorted)
import           Data.Sequence     ((><))
import qualified Data.Sequence     as Seq
import qualified Data.Set          as Set

-- copy pasta https://stackoverflow.com/questions/16108714/removing-duplicates-from-a-list-in-haskell-without-elem
sortUnique :: Ord a => [a] -> [a]
sortUnique = sort . rmdups' Set.empty where
  rmdups' _ [] = []
  rmdups' a (b : c) = if Set.member b a
    then rmdups' a c
    else b : rmdups' (Set.insert b a) c

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


hasScopingProperty :: (a -> Maybe Bool) -> Seq a -> Bool
hasScopingProperty scopeTypeFn xs = not finalFail && finalScope == 0 where
  foldfn x (scopes, didFail) = case scopeTypeFn x of
    Nothing -> (scopes, didFail)
    Just f -> case f of
      True -> case scopes of
        0 -> (scopes, True)
        _ -> (scopes-1, didFail)
      False -> (scopes+1, didFail)
  (finalScope, finalFail) = foldr foldfn (0 :: Int, False) xs

-- | assumes selection is ordered and is valid
selectionHasScopingProperty :: (a -> Maybe Bool) -> Seq a -> [Int] -> Bool
selectionHasScopingProperty scopeTypeFn xs is = hasScopingProperty scopeTypeFn subSeq where
  subSeq = Seq.fromList $ map (\i -> Seq.index xs i) is

makePairMap :: (a -> Maybe Bool) -> Seq a -> BM.Bimap Int Int
makePairMap scopeTypeFn xs = fst r where
  -- map folders from start to end index
  pairmapfoldfn i a (pairs, scopes) = case scopeTypeFn a of
    Nothing -> (pairs, scopes)
    Just True -> case scopes of
      []        -> error "mismatched scopes"
      x:scopes' -> (BM.insert i x pairs, scopes')
    Just False -> (pairs, i:scopes)
  r = Seq.foldrWithIndex pairmapfoldfn (BM.empty,[]) xs

-- assumes input sequence satisfies scoping property
-- assumes input index is actually a folder
findMatchingScope :: (a -> Maybe Bool) -> Seq a -> Int -> Int
findMatchingScope scopeTypeFn xs i = r where
  pairmap = makePairMap scopeTypeFn xs
  r = case scopeTypeFn (Seq.index xs i) of
    Nothing -> error "input index was not a folder"
    Just True -> case BM.lookup i pairmap of
      Nothing -> error "pairmap missing elements, this means scopes were mismatched"
      Just x -> x
    Just False -> case BM.lookupR i pairmap of
      Nothing -> error "pairmap missing elements, this means scopes were mismatched"
      Just x -> x

-- | converts selection so that it satisfies the scoping property by adding matching folders
-- assumes input sequence satisfies scoping property???
-- simple and inefficient implementation, do not use in prod
scopeSelection :: (a -> Maybe Bool) -> Seq a -> [Int] -> [Int]
scopeSelection scopeTypeFn xs is = r where
  pairmap = makePairMap scopeTypeFn xs
  -- go through and lookup matches
  foldfn i acc = case scopeTypeFn (Seq.index xs i) of
    Nothing -> acc
    Just True -> case BM.lookup i pairmap of
      Nothing -> error "pairmap missing elements, this means scopes were mismatched"
      Just x -> x:acc
    Just False -> case BM.lookupR i pairmap of
      Nothing -> error "pairmap missing elements, this means scopes were mismatched"
      Just x -> x:acc
  newElts = foldr foldfn [] is
  r = sortUnique (newElts <> is)


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

-- | inserts ys into xs, positions are before insertion
insertEltList_indexBeforeInsertion :: [(Int, a)] -> Seq a -> Seq a
insertEltList_indexBeforeInsertion ys xs = assert (isSorted is') $ newSeq where
  is' = map fst ys
  elts = map snd ys
  is = reindexSEltLayerPosForInsertion is'
  newSeq = foldr (uncurry insertElt) xs (zip is elts)

-- | inserts ys into xs, positions are after insertion
insertEltList_indexAfterInsertion :: [(Int, a)] -> Seq a -> Seq a
insertEltList_indexAfterInsertion ys xs = assert (isSorted is) $ newSeq where
  is = map fst ys
  newSeq = foldl' (flip (uncurry insertElt)) xs ys

-- | removes is' from xs, positions are before removal
removeEltList :: [Int] -> Seq a -> Seq a
removeEltList is' xs = assert (isSorted is) $ newSeq where
  is = reindexSEltLayerPosForRemoval is'
  newSeq = foldl' (flip removeElt) xs is

-- | moves all elts, new position is before removal, ys must be sorted
moveEltList :: [Int] -> Int -> Seq a -> Seq a
moveEltList is i xs = assert (isSorted is) $ newSeq where
  nBefore = length . filter (< i) $ is
  ys = map (Seq.index xs) is
  newSeq' = removeEltList is xs
  newSeq = insertElts (i-nBefore) (Seq.fromList ys) newSeq'

-- inverse of `moveEltList`
undoMoveEltList :: [Int] -> Int -> Seq a -> Seq a
undoMoveEltList is i xs = assert (isSorted is) $ newSeq where
  nMoved = length is
  moveToIndex = i - (length (takeWhile (\x -> x < i) is))
  (leftL,rightL') = Seq.splitAt moveToIndex xs
  (toMove,rightL) = Seq.splitAt nMoved rightL'
  newSeq' = leftL >< rightL
  newSeq = insertEltList_indexAfterInsertion (zip is (toList toMove)) newSeq'
