{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Types (
  UnicodeWidthFn(..)
  , Tool(..)
  , tool_isCreate
  , PotatoDefaultParameters(..)
  , SetPotatoDefaultParameters(..)
  , potatoDefaultParameters_set
  , Selection
  , defaultFolderCollapseState
  , LayerMeta(..)
  , LayerMetaMap
  , layerMetaMap_isCollapsed
  , ControllerMeta(..)
  , emptyControllerMeta
  , EverythingLoadState
) where

import           Relude

import           Potato.Flow.Math
import           Potato.Flow.Owl
import           Potato.Flow.SElts
import           Potato.Flow.Types

import           Data.Aeson
import           Data.Binary
import           Data.Default
import qualified Data.IntMap       as IM
import qualified Text.Show



-- someday it would be nice to support graphene clusters and RTL 😭
data UnicodeWidthFn = UnicodeWidthFn {
    unicodeWidth_wcwidth :: PChar -> Int
  }


-- TODO remove Tool_TextArea
-- TOOL
data Tool = Tool_Select | Tool_Pan | Tool_Box | Tool_Line | Tool_Text | Tool_TextArea deriving (Eq, Show, Enum)

tool_isCreate :: Tool -> Bool
tool_isCreate = \case
  Tool_Select -> False
  Tool_Pan -> False
  _ -> True

data PotatoDefaultParameters = PotatoDefaultParameters {
  _potatoDefaultParameters_sBoxType              :: SBoxType -- currently not used as we have Tool_TextArea, consider using this instead
  , _potatoDefaultParameters_superStyle          :: SuperStyle
  , _potatoDefaultParameters_lineStyle           :: LineStyle
  , _potatoDefaultParameters_lineStyleEnd        :: LineStyle
  , _potatoDefaultParameters_box_label_textAlign :: TextAlign
  , _potatoDefaultParameters_box_text_textAlign  :: TextAlign
} deriving (Eq, Show)


instance Default PotatoDefaultParameters where
  def = PotatoDefaultParameters {
      _potatoDefaultParameters_sBoxType = def
      , _potatoDefaultParameters_lineStyle = def
      , _potatoDefaultParameters_lineStyleEnd = def
      , _potatoDefaultParameters_superStyle = def
      , _potatoDefaultParameters_box_label_textAlign = def
      , _potatoDefaultParameters_box_text_textAlign = def
    }

-- TODO rename to SetPotatoDefaultStyleParameters or something like that
data SetPotatoDefaultParameters = SetPotatoDefaultParameters {
  _setPotatoDefaultParameters_sBoxType              :: Maybe SBoxType
  , _setPotatoDefaultParameters_lineStyle           :: Maybe LineStyle
  , _setPotatoDefaultParameters_lineStyleEnd        :: Maybe LineStyle
  , _setPotatoDefaultParameters_superStyle          :: Maybe SuperStyle
  , _setPotatoDefaultParameters_box_label_textAlign :: Maybe TextAlign
  , _setPotatoDefaultParameters_box_text_textAlign  :: Maybe TextAlign
} deriving (Eq, Show)

instance Default SetPotatoDefaultParameters where
  def = SetPotatoDefaultParameters {
      _setPotatoDefaultParameters_sBoxType = Nothing
      , _setPotatoDefaultParameters_lineStyle = Nothing
      , _setPotatoDefaultParameters_lineStyleEnd = Nothing
      , _setPotatoDefaultParameters_superStyle = Nothing
      , _setPotatoDefaultParameters_box_label_textAlign = Nothing
      , _setPotatoDefaultParameters_box_text_textAlign = Nothing
    }

potatoDefaultParameters_set :: PotatoDefaultParameters -> SetPotatoDefaultParameters -> PotatoDefaultParameters
potatoDefaultParameters_set PotatoDefaultParameters {..} SetPotatoDefaultParameters {..} = PotatoDefaultParameters {
    _potatoDefaultParameters_sBoxType = fromMaybe _potatoDefaultParameters_sBoxType _setPotatoDefaultParameters_sBoxType
    , _potatoDefaultParameters_lineStyle = fromMaybe _potatoDefaultParameters_lineStyle _setPotatoDefaultParameters_lineStyle
    , _potatoDefaultParameters_lineStyleEnd = fromMaybe _potatoDefaultParameters_lineStyleEnd _setPotatoDefaultParameters_lineStyleEnd
    , _potatoDefaultParameters_superStyle = fromMaybe _potatoDefaultParameters_superStyle _setPotatoDefaultParameters_superStyle
    , _potatoDefaultParameters_box_label_textAlign = fromMaybe _potatoDefaultParameters_box_label_textAlign _setPotatoDefaultParameters_box_label_textAlign
    , _potatoDefaultParameters_box_text_textAlign = fromMaybe _potatoDefaultParameters_box_text_textAlign _setPotatoDefaultParameters_box_text_textAlign

  }

type Selection = SuperOwlParliament

data LayerMeta = LayerMeta {
  -- if False, these will inherit from parent
  _layerMeta_isLocked      :: Bool
  , _layerMeta_isHidden    :: Bool
  , _layerMeta_isCollapsed :: Bool

} deriving (Eq, Generic)

instance Show LayerMeta where
  show LayerMeta {..} = "LayerMeta (l,h,c): " <> show _layerMeta_isLocked <> " " <> show _layerMeta_isHidden <> " " <> show _layerMeta_isCollapsed

instance FromJSON LayerMeta
instance ToJSON LayerMeta
instance NFData LayerMeta
instance Binary LayerMeta


-- Not sure which way I want to do it, so make it configurable for now.
defaultFolderCollapseState :: Bool
defaultFolderCollapseState = False

instance Default LayerMeta where
  def = LayerMeta {
      _layerMeta_isLocked = False
      , _layerMeta_isHidden = False
      , _layerMeta_isCollapsed = defaultFolderCollapseState
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
instance NFData ControllerMeta
instance Binary ControllerMeta

emptyControllerMeta :: ControllerMeta
emptyControllerMeta = ControllerMeta 0 IM.empty

instance Default ControllerMeta where
  def = emptyControllerMeta

type EverythingLoadState = (SPotatoFlow, ControllerMeta)
