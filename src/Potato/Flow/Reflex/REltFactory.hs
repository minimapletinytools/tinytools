{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}
module Potato.Flow.Reflex.REltFactory (
  REltFactory(..)
  , REltFactoryConfig(..)
  , holdREltFactory
) where

import           Relude

import           Potato.Flow.Math
import           Potato.Flow.Reflex.Layers
import           Potato.Flow.Reflex.RElts
import           Potato.Flow.SElts
import           Potato.Flow.Types

import           Reflex


data REltFactory t = REltFactor {
  _rEltFactory_newRElt :: Event t (REltId, RElt t)
}

data REltFactoryConfig t = REltFactoryConfig {
  -- connects to _pfc_addElt
  -- does not do any checking if the SEltTree is valid
  _rEltFactoryConfig_newEltTree :: Event t SEltTree
}

holdREltFactory ::
  forall t m. (Reflex t, MonadHold t m)
  => REltFactoryConfig t
  -> m (REltFactory t)
holdREltFactory REltFactoryConfig {..} = do
  undefined
