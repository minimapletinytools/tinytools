-- WIP

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.BroadPhase (
  BroadPhase(..)
  , BroadPhaseConfig(..)
  , holdBroadPhase
) where

import           Relude

import           Reflex

import           Potato.Flow.Math
import           Potato.Flow.Render

import           Control.Monad.Fix


data BroadPhase t = BroadPhase {

}

data BroadPhaseConfig t = BroadPhaseConfig {
  _broadPhaseConfig_something :: Event t ()
}

holdBroadPhase :: forall t m. (Adjustable t m, MonadHold t m, MonadFix m)
  => BroadPhaseConfig t
  -> m (BroadPhase t)
holdBroadPhase BroadPhaseConfig {..} = mdo
  return
    BroadPhase {
    }
