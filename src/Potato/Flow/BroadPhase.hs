{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.BroadPhase (
  AABB
  , NeedsUpdateSet
  , BPTree(..)
  , bPTreeFromOwlPFState
  , emptyBPTree
  , broadPhase_cull
  , broadPhase_cull_includeZero

  , BroadPhaseState(..)
  , emptyBroadPhaseState

  -- exposed for testing
  , update_bPTree
) where

import           Relude

import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.OwlState
import           Potato.Flow.Owl
import           Potato.Flow.Types
import Potato.Flow.SEltMethods
import Potato.Flow.Methods.Types

import qualified Data.IntMap.Strict      as IM

type AABB = LBox

type NeedsUpdateSet = [AABB]

-- TODO actual BroadPhase...
data BPTree = BPTree {
  -- TODO you want something sortable too...
  _bPTree_potato_tree :: REltIdMap AABB
} deriving (Show, Eq)

emptyBPTree :: BPTree
emptyBPTree = BPTree IM.empty

-- TODO
--bPTreeFromPFState :: PFState -> BPTree
--bPTreeFromPFState PFState {..} = r where
--  potato_tree = IM.mapMaybe (getSEltBox . _sEltLabel_sElt) _pFState_directory
--  r = BPTree potato_tree

bPTreeFromOwlPFState :: OwlPFState -> BPTree
bPTreeFromOwlPFState OwlPFState {..} = r where
  potato_tree = IM.mapMaybe (\(_,oelt) -> getSEltBox . owlElt_toSElt_hack $ oelt) (_owlTree_mapping _owlPFState_owlTree)
  r = BPTree potato_tree

data BroadPhaseState = BroadPhaseState {
  _broadPhaseState_bPTree    :: BPTree -- updated BPTree
} deriving (Show, Eq)

emptyBroadPhaseState :: BroadPhaseState
emptyBroadPhaseState = BroadPhaseState emptyBPTree




-- | updates a BPTree and returns list of AABBs that were affected
-- exposed for testing only, do not call this directly
update_bPTree :: (HasOwlTree a, HasRenderCache a) => a -> SuperOwlChanges -> BPTree -> (NeedsUpdateSet, BroadPhaseState)
update_bPTree ot changes BPTree {..} = r where
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
    (\(insmods',deletes') (rid, msowl) -> case msowl of
      Nothing    -> (insmods', rid:deletes')

      -- TODO don't use getSEltBox like this come on -__-
      Just sowl -> case getSEltBox (superOwl_toSElt_hack sowl) of
        Nothing   -> (insmods', rid:deletes')
        Just _ -> ((rid,_sEltDrawer_box (getDrawer (superOwl_toSElt_hack sowl)) ot):insmods', deletes'))


    ([],[])
    (IM.toList changes)
  (aabbs', nbpt) = foldl' insmodfn (foldl' deletefn ([], _bPTree_potato_tree) deletes) insmods
  r = (aabbs', BroadPhaseState (BPTree nbpt))

-- TODO prob don't need this, DELETE
--update_bPTree' ::  (REltId, Maybe SEltLabel) -> BPTree -> BPTree
--update_bPTree' (rid, ms) BPTree {..} = BPTree $ IM.alter (const (ms >>= getSEltBox . _sEltLabel_sElt)) rid _bPTree_potato_tree

-- | returns list of REltIds that intersect with given AABB
broadPhase_cull :: AABB -> BPTree -> [REltId]
broadPhase_cull box BPTree {..} = r where
  foldfn rid aabb cols = if does_lBox_intersect box aabb then rid:cols else cols
  r = IM.foldrWithKey foldfn [] _bPTree_potato_tree

-- | same as above but also returns zero area elements for selecting
broadPhase_cull_includeZero :: AABB -> BPTree -> [REltId]
broadPhase_cull_includeZero box BPTree {..} = r where
  foldfn rid aabb cols = if does_lBox_intersect_include_zero_area box aabb then rid:cols else cols
  r = IM.foldrWithKey foldfn [] _bPTree_potato_tree
