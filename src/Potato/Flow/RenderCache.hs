module Potato.Flow.RenderCache where

import           Relude

import           Potato.Flow.Math
import           Potato.Flow.SElts
import Potato.Flow.Types
import           Potato.Flow.OwlItem
import Potato.Flow.OwlState
import           Potato.Flow.OwlItem
import Potato.Flow.Owl


data SEltCache = SEltCache

data RenderCache = RenderCache {
  renderCache_sEltCache :: REltIdMap SEltCache
}

-- TODO
--class MonadBroadPhase m where
--  getBroadPhase :: m BroadPhase
--  setBroadPhase :: BroadPhase -> m ()

class MonadRenderCache m where
    getRenderCache :: m RenderCache
    putRenderCache :: RenderCache -> m ()

instance MonadRenderCache (State RenderCache) where
  getRenderCache = get
  putRenderCache = put

newtype RenderCacheM a = RenderCacheM { unRenderCacheM :: State RenderCache a}

instance MonadRenderCache RenderCacheM
