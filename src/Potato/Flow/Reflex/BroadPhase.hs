-- TODO deprecate

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.BroadPhase (
  BroadPhase(..)
  , BroadPhaseConfig(..)
  , holdBroadPhase
) where

import           Relude

import           Reflex

import           Potato.Flow.BroadPhase
import           Potato.Flow.Math
import           Potato.Flow.Reflex.RElts
import           Potato.Flow.SElts
import           Potato.Flow.Types

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
