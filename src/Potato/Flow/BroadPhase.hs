{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.BroadPhase (
  AABB
  , BPTree(..)
  , bPTreeFromPFState
  , emptyBPTree
  , broadPhase_cull

  , BroadPhaseState(..)
  , emptyBroadPhaseState

  -- exposed for testing
  , update_bPTree
) where

import           Relude

import           Potato.Flow.Math
import           Potato.Flow.SEltMethods
import           Potato.Flow.SElts
import           Potato.Flow.State
import           Potato.Flow.Types

import qualified Data.IntMap.Strict      as IM

type AABB = LBox

-- TODO actual BroadPhase...
data BPTree = BPTree {
  _bPTree_potato_tree :: REltIdMap AABB
} deriving (Show, Eq)

emptyBPTree :: BPTree
emptyBPTree = BPTree IM.empty

bPTreeFromPFState :: PFState -> BPTree
bPTreeFromPFState PFState {..} = r where
  potato_tree = IM.mapMaybe (getSEltBox . _sEltLabel_sElt) _pFState_directory
  r = BPTree potato_tree

-- TODO split contents apart, these should be tracked separately
data BroadPhaseState = BroadPhaseState {
  _broadPhaseState_needsUpdate :: [AABB] -- this is what changed since last time
  , _broadPhaseState_bPTree    :: BPTree -- updated BPTree
} deriving (Show, Eq)

emptyBroadPhaseState :: BroadPhaseState
emptyBroadPhaseState = BroadPhaseState [] emptyBPTree




-- | updates a BPTree and returns list of AABBs that were affected
-- exposed for testing only, do not call this directly
update_bPTree :: REltIdMap (Maybe SEltLabel) -> BPTree -> BroadPhaseState
update_bPTree changes BPTree {..} = r where
  -- deletions
  deletefn (aabbs, im) rid = (newaabbs, newim) where
    (moldaabb, newim) = IM.updateLookupWithKey (\_ _ -> Nothing) rid im
    newaabbs = maybe aabbs (\oldaabb -> oldaabb:aabbs) moldaabb

  -- modify/insert
  insmodfn (aabbs, im) (rid, lbox) = (newaabbs, newim) where
    (moldaabb :: Maybe AABB, newim) = IM.insertLookupWithKey (\_ a _ -> a) rid lbox im
    newaabbs' = lbox:aabbs
    newaabbs = maybe newaabbs' (\oldaabb -> oldaabb:newaabbs') moldaabb

  (insmods, deletes) = foldl'
    (\(insmods',deletes') (rid, mseltl) -> case mseltl of
      Nothing    -> (insmods', rid:deletes')
      Just seltl -> case getSEltBox (_sEltLabel_sElt seltl) of
        Nothing   -> (insmods', rid:deletes')
        Just lbox -> ((rid,lbox):insmods', deletes'))
    ([],[])
    (IM.toList changes)
  (aabbs', nbpt) = foldl' insmodfn (foldl' deletefn ([], _bPTree_potato_tree) deletes) insmods
  r = BroadPhaseState aabbs' (BPTree nbpt)

-- TODO prob don't need this, DELETE
--update_bPTree' ::  (REltId, Maybe SEltLabel) -> BPTree -> BPTree
--update_bPTree' (rid, ms) BPTree {..} = BPTree $ IM.alter (const (ms >>= getSEltBox . _sEltLabel_sElt)) rid _bPTree_potato_tree

-- | returns list of REltIds that intersect with given AABB
broadPhase_cull :: AABB -> BPTree -> [REltId]
broadPhase_cull box BPTree {..} = r where
  foldfn rid aabb cols = if does_lBox_intersect box aabb then rid:cols else cols
  r = IM.foldrWithKey foldfn [] _bPTree_potato_tree
