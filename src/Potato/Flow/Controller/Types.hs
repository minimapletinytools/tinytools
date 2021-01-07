{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Types (
  UnicodeWidthFn(..)
  , Tool(..)
  , tool_isCreate
  , Selection
  , disjointUnionSelection
  , validateSelection
  , LayerMeta(..)
  , LayerMetaMap
  , ControllerMeta(..)
  , emptyControllerMeta
  , EverythingLoadState
) where

import           Relude

import           Reflex
import           Reflex.Potato.Helpers

import           Potato.Flow.BroadPhase
import           Potato.Flow.Layers     (selectionHasScopingProperty)
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
import qualified Data.List              as L
import qualified Data.List.Ordered      as L (isSortedBy)
import           Data.Maybe
import qualified Data.Sequence          as Seq
import           Data.Tuple.Extra


-- someday it would be nice to support graphene clusters and RTL 😭
data UnicodeWidthFn = UnicodeWidthFn {
    unicodeWidth_wcwidth :: PChar -> Int
  }

-- TOOL
data Tool = Tool_Select | Tool_Pan | Tool_Box | Tool_Line | Tool_Text deriving (Eq, Show, Enum)

tool_isCreate :: Tool -> Bool
tool_isCreate = \case
  Tool_Select -> False
  Tool_Pan -> False
  _ -> True

type Selection = Seq SuperSEltLabel

-- selection helpers
disjointUnion :: (Eq a) => [a] -> [a] -> [a]
disjointUnion a b = L.union a b L.\\ L.intersect a b

-- TODO real implementation...
disjointUnionSelection :: Selection -> Selection -> Selection
disjointUnionSelection s1 s2 = Seq.fromList $ disjointUnion (toList s1) (toList s2)

validateSelection :: Selection -> Bool
validateSelection selection = r1 && r2 where
  -- validate lps in order
  sortingfn (_,lp1,_) (_,lp2,_) = lp1 < lp2
  r1 = L.isSortedBy sortingfn (toList selection)
  -- validate scoping property
  r2 = selectionHasScopingProperty scopeFn selection [0..Seq.length selection - 1]
  scopeFn (_,_,seltl) = case seltl of
    (SEltLabel _ SEltFolderStart) -> Just True
    (SEltLabel _ SEltFolderEnd)   -> Just False
    _                             -> Nothing



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

data ControllerMeta = ControllerMeta {
  _controllerMeta_pan      :: XY
  , _controllerMeta_layers :: LayerMetaMap
} deriving (Show, Eq, Generic)

instance FromJSON ControllerMeta
instance ToJSON ControllerMeta

emptyControllerMeta :: ControllerMeta
emptyControllerMeta = ControllerMeta 0 IM.empty

type EverythingLoadState = (SPotatoFlow, ControllerMeta)
