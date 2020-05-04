{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.BroadPhase (
  AABB
  , BPTree(..)
  , BroadPhase(..)
  , BroadPhaseConfig(..)
  , holdBroadPhase
  , broadPhase_cull

  -- exposed for testing
  , update_bPTree
) where

import           Relude

import           Reflex

import           Potato.Flow.Math
import           Potato.Flow.Reflex.RElts
import           Potato.Flow.Reflex.Types
import           Potato.Flow.SElts

import           Control.Monad.Fix
import qualified Data.IntMap.Strict       as IM
import           Data.Tuple.Extra         (snd3)


data BroadPhase t = BroadPhase {
  -- | (list of AABBs that need to be updated, updated BPTree, changed SEltLabels from last updated)
  _broadPhase_render   :: Event t ([AABB], BPTree, REltIdMap (Maybe SEltLabel))
  , _broadPhase_bPTree :: Dynamic t BPTree
}

data BroadPhaseConfig t = BroadPhaseConfig {
  _broadPhaseConfig_change :: Event t (REltIdMap (Maybe SEltLabel))
}

holdBroadPhase :: forall t m. (Adjustable t m, MonadHold t m, MonadFix m)
  => BroadPhaseConfig t
  -> m (BroadPhase t)
holdBroadPhase BroadPhaseConfig {..} = mdo
  bptDyn <- foldDyn (\changes (_,bpt,_) -> update_bPTree changes bpt) ([], BPTree IM.empty, IM.empty) _broadPhaseConfig_change
  return
    BroadPhase {
      _broadPhase_render = updated bptDyn
      , _broadPhase_bPTree = fmap snd3 bptDyn
    }

type AABB = LBox

-- TODO actual BroadPhase...
data BPTree = BPTree {
  _bPTree_potato_tree :: REltIdMap AABB
}

-- | updates a BPTree and returns list of AABBs that were affected
-- exposed for testing only, do not call this directly
update_bPTree :: REltIdMap (Maybe SEltLabel) -> BPTree -> ([AABB], BPTree, REltIdMap (Maybe SEltLabel))
update_bPTree changes BPTree {..} = r where
  -- deletions
  deletefn (aabbs, im) rid = (newaabbs, newim) where
    (moldaabb, newim) = IM.updateLookupWithKey (\_ _ -> Nothing) rid im
    newaabbs = maybe aabbs (\oldaabb -> oldaabb:aabbs) moldaabb

  -- modify/insert
  insmodfn (aabbs :: [AABB], im) (rid, lbox) = (newaabbs, newim) where
    (moldaabb :: Maybe AABB, newim) = IM.insertLookupWithKey (\_ _ a -> a) rid lbox im
    newaabbs' = lbox:aabbs
    newaabbs :: [AABB] = maybe newaabbs' (\oldaabb -> oldaabb:newaabbs') moldaabb

  (insmods, deletes) = foldl'
    (\(insmods',deletes') (rid, mseltl) -> case mseltl of
      Nothing    -> (insmods', rid:deletes')
      Just seltl -> case getSEltBox (_sEltLabel_sElt seltl) of
        Nothing   -> (insmods', rid:deletes')
        Just lbox -> ((rid,lbox):insmods', deletes'))
    ([],[])
    (IM.toList changes)
  (aabbs, nbpt) = foldl' insmodfn (foldl' deletefn ([], _bPTree_potato_tree) deletes) insmods
  r = (aabbs, BPTree nbpt, changes)

-- TODO prob don't need this, DELETE
update_bPTree' ::  (REltId, Maybe SEltLabel) -> BPTree -> BPTree
update_bPTree' (rid, ms) BPTree {..} = BPTree $ IM.alter (const (ms >>= getSEltBox . _sEltLabel_sElt)) rid _bPTree_potato_tree

-- | returns list of REltIds that intersect with given AABB
broadPhase_cull :: AABB -> BPTree -> [REltId]
broadPhase_cull box BPTree {..} = r where
  foldfn rid aabb cols = if does_LBox_intersect box aabb then rid:cols else cols
  r = IM.foldrWithKey foldfn [] _bPTree_potato_tree
