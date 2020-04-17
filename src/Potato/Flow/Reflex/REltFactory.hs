{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}
module Potato.Flow.Reflex.REltFactory (
  REltFactory(..)
  , REltFactoryConfig(..)
  , holdREltFactory
) where

import           Relude

import           Reflex
import           Reflex.Data.Directory

import           Potato.Flow.Math
import           Potato.Flow.Reflex.Layers
import           Potato.Flow.Reflex.RElts
import           Potato.Flow.SElts
import           Potato.Flow.Types

import           Control.Monad.Fix





data REltFactory t = REltFactor {
  _rEltFactory_rEltTree :: Event t (REltTree t)
}

data REltFactoryConfig t = REltFactoryConfig {
  -- connects to _pfc_addElt
  -- does not do any checking if the SEltTree is valid
  _rEltFactoryConfig_sEltTree         :: Event t SEltWithIdTree
  , _rEltFactoryConfig_doManipulate   :: Event t (ManipulatorWithId t)
  , _rEltFactoryConfig_undoManipulate :: Event t (ManipulatorWithId t)
}

holdREltFactory ::
  forall t m. (Reflex t, MonadHold t m, MonadFix m)
  => REltFactoryConfig t
  -> m (REltFactory t)
holdREltFactory REltFactoryConfig {..} = do
  let
    doev = _rEltFactoryConfig_doManipulate
    undoev = _rEltFactoryConfig_undoManipulate
  return
    REltFactor {
        _rEltFactory_rEltTree = pushAlways (deserialize doev undoev) _rEltFactoryConfig_sEltTree
      }
