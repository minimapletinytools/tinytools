{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Types (
  UnicodeWidthFn(..)
  , Tool(..)
  , tool_isCreate
  , Selection
  , LayerMeta(..)
  , LayerMetaMap
  , layerMetaMap_isCollapsed
  , layerMetaMap_isHidden
  , layerMetaMap_isHiddenOrLocked
  , ControllerMeta(..)
  , emptyControllerMeta
  , EverythingLoadState
) where

import           Relude

import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.Types
import           Potato.Flow.Owl

import           Data.Aeson
import           Data.Default
import qualified Data.IntMap            as IM


-- someday it would be nice to support graphene clusters and RTL 😭
data UnicodeWidthFn = UnicodeWidthFn {
    unicodeWidth_wcwidth :: PChar -> Int
  }


-- TOOL
data Tool = Tool_Select | Tool_Pan | Tool_Box | Tool_Line | Tool_Text | Tool_TextArea | Tool_CartLine deriving (Eq, Show, Enum)

tool_isCreate :: Tool -> Bool
tool_isCreate = \case
  Tool_Select -> False
  Tool_Pan -> False
  _ -> True

type Selection = SuperOwlParliament


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

layerMetaMap_isCollapsed :: REltId -> LayerMetaMap -> Bool
layerMetaMap_isCollapsed rid lmm = case IM.lookup rid lmm of
  Nothing -> True
  Just lm -> _layerMeta_isCollapsed lm


layerMetaMap_isHidden :: REltId -> LayerMetaMap -> Bool
layerMetaMap_isHidden rid lmm = case IM.lookup rid lmm of
  Nothing -> False
  Just lm -> _layerMeta_isHidden lm

layerMetaMap_isHiddenOrLocked :: REltId -> LayerMetaMap -> Bool
layerMetaMap_isHiddenOrLocked rid lmm = case IM.lookup rid lmm of
  Nothing -> False
  Just lm -> _layerMeta_isLocked lm || _layerMeta_isHidden lm


data ControllerMeta = ControllerMeta {
  _controllerMeta_pan      :: XY -- do we really want this?
  , _controllerMeta_layers :: LayerMetaMap
} deriving (Show, Eq, Generic)

instance FromJSON ControllerMeta
instance ToJSON ControllerMeta

emptyControllerMeta :: ControllerMeta
emptyControllerMeta = ControllerMeta 0 IM.empty

type EverythingLoadState = (SPotatoFlow, ControllerMeta)
