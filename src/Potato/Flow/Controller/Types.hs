{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Types (
  LayerMeta(..)
  , LayerMetaMap
  , ControllerMeta(..)
  , emptyControllerMeta
  , EverythingLoadState
) where

import           Relude

import           Reflex
import           Reflex.Potato.Helpers

import           Potato.Flow.BroadPhase
import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.State
import           Potato.Flow.Types
import           Potato.Flow.Workspace

import           Control.Exception      (assert)
import           Control.Monad.Fix
import           Data.Aeson
import           Data.Default
import qualified Data.IntMap            as IM
import           Data.Maybe
import qualified Data.Sequence          as Seq
import           Data.Tuple.Extra



data LayerMeta = LayerMeta {
  -- if False, these will inherit from parent
  _layerMeta_isLocked      :: Bool
  , _layerMeta_isHidden    :: Bool
  , _layerMeta_isCollapsed :: Bool

} deriving (Eq, Generic, Show)

instance FromJSON LayerMeta
instance ToJSON LayerMeta
instance NFData LayerMeta

instance Default LayerMeta where
  def = LayerMeta {
      _layerMeta_isLocked = False
      , _layerMeta_isHidden = False
      , _layerMeta_isCollapsed = True
    }

type LayerMetaMap = REltIdMap LayerMeta


-- TODO this is a problem because LayerMetaMap
-- I guess you can just reindex LayerMetaMap to index via LayerPos (which should be the same as REltId after loading I think)
-- Alternatively, you could just have SPotatoFlow include REltId, that might be slightly better solution...
data ControllerMeta = ControllerMeta {
  _controllerMeta_pan      :: XY
  , _controllerMeta_layers :: LayerMetaMap
} deriving (Show, Eq, Generic)

instance FromJSON ControllerMeta
instance ToJSON ControllerMeta

emptyControllerMeta :: ControllerMeta
emptyControllerMeta = ControllerMeta 0 IM.empty

type EverythingLoadState = (SPotatoFlow, ControllerMeta)
