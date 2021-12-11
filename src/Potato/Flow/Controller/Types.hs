{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Types (
  UnicodeWidthFn(..)
  , Tool(..)
  , tool_isCreate
  , PotatoDefaultParameters(..)
  , SetPotatoDefaultParameters(..)
  , potatoDefaultParameters_set
  , Selection
  , LayerMeta(..)
  , LayerMetaMap
  , layerMetaMap_isCollapsed
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

data PotatoDefaultParameters = PotatoDefaultParameters {
  _potatoDefaultParameters_sBoxType :: SBoxType
  , _potatoDefaultParameters_lineStyle :: LineStyle
  , _potatoDefaultParameters_box_label_textAlign :: TextAlign
  , _potatoDefaultParameters_box_text_textAlign :: TextAlign
} deriving (Show)


instance Default PotatoDefaultParameters where
  def = PotatoDefaultParameters {
      _potatoDefaultParameters_sBoxType = def
      , _potatoDefaultParameters_lineStyle = def
      , _potatoDefaultParameters_box_label_textAlign = def
      , _potatoDefaultParameters_box_text_textAlign = def
    }

data SetPotatoDefaultParameters = SetPotatoDefaultParameters {
  _setPotatoDefaultParameters_sBoxType :: Maybe SBoxType
  , _setPotatoDefaultParameters_lineStyle :: Maybe LineStyle
  , _setPotatoDefaultParameters_box_label_textAlign :: Maybe TextAlign
  , _setPotatoDefaultParameters_box_text_textAlign :: Maybe TextAlign
} deriving (Show)

instance Default SetPotatoDefaultParameters where
  def = SetPotatoDefaultParameters {
      _setPotatoDefaultParameters_sBoxType = Nothing
      , _setPotatoDefaultParameters_lineStyle = Nothing
      , _setPotatoDefaultParameters_box_label_textAlign = Nothing
      , _setPotatoDefaultParameters_box_text_textAlign = Nothing
    }

potatoDefaultParameters_set :: PotatoDefaultParameters -> SetPotatoDefaultParameters -> PotatoDefaultParameters
potatoDefaultParameters_set PotatoDefaultParameters {..} SetPotatoDefaultParameters {..} = PotatoDefaultParameters {
    _potatoDefaultParameters_sBoxType = fromMaybe _potatoDefaultParameters_sBoxType _setPotatoDefaultParameters_sBoxType
    , _potatoDefaultParameters_lineStyle = fromMaybe _potatoDefaultParameters_lineStyle _setPotatoDefaultParameters_lineStyle
    , _potatoDefaultParameters_box_label_textAlign = fromMaybe _potatoDefaultParameters_box_label_textAlign _setPotatoDefaultParameters_box_label_textAlign
    , _potatoDefaultParameters_box_text_textAlign = fromMaybe _potatoDefaultParameters_box_text_textAlign _setPotatoDefaultParameters_box_text_textAlign

  }

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

{-
-- these aren't very useful because they won't tell you if it has inherited lock/hidden state
layerMetaMap_isHidden :: REltId -> LayerMetaMap -> Bool
layerMetaMap_isHidden rid lmm = case IM.lookup rid lmm of
  Nothing -> False
  Just lm -> _layerMeta_isHidden lm
layerMetaMap_isHiddenOrLocked :: REltId -> LayerMetaMap -> Bool
layerMetaMap_isHiddenOrLocked rid lmm = case IM.lookup rid lmm of
  Nothing -> False
  Just lm -> _layerMeta_isLocked lm || _layerMeta_isHidden lm
-}

data ControllerMeta = ControllerMeta {
  _controllerMeta_pan      :: XY -- do we really want this?
  , _controllerMeta_layers :: LayerMetaMap
} deriving (Show, Eq, Generic)

instance FromJSON ControllerMeta
instance ToJSON ControllerMeta

emptyControllerMeta :: ControllerMeta
emptyControllerMeta = ControllerMeta 0 IM.empty

type EverythingLoadState = (SPotatoFlow, ControllerMeta)
