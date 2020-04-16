{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}
module Potato.Flow.Reflex.REltFactory (
  REltFactory(..)
  , REltFactoryConfig(..)
  , holdREltFactory
) where

import           Relude

import           Reflex

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
  _rEltFactoryConfig_sEltTree :: Event t SEltTree
}

holdREltFactory ::
  forall t m. (Reflex t, MonadHold t m, MonadFix m)
  => REltFactoryConfig t
  -> m (REltFactory t)
holdREltFactory REltFactoryConfig {..} = do
  let
    foldfn :: SEltTree -> REltTree t -> PushM t (Maybe (REltTree t))
    foldfn selttree _ = case selttree of
      [] -> return Nothing
      x  -> Just <$> deserialize x
  -- so there's no fmapMaybeM for events?
  rtree :: Dynamic t (REltTree t) <-
    foldDynMaybeM foldfn [] _rEltFactoryConfig_sEltTree
  return
    REltFactor {
        _rEltFactory_rEltTree = updated rtree
      }
