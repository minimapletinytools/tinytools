{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.Everything (
  MouseModifier(..)
  , LMouseEvent(..)
  , Tool(..)
  , LayerDisplay(..)
  , MouseManipulator(..)
  , EverythingBackend(..)
  , EverythingCmd(..)
  , EverythingWidgetConfig(..)
  , EverythingWidget(..)
) where

import           Relude

import           Reflex

import           Potato.Flow.BroadPhase
import           Potato.Flow.Math
import           Potato.Flow.Reflex.BroadPhase
import           Potato.Flow.Types

import           Control.Monad.Fix

-- only ones we care about
data MouseModifier = Shift | Alt

-- TODO is this the all encompassing mouse event we want?
-- only one modifier allowed at a time for our app
-- TODO is there a way to optionally support more fidelity here?
data LMouseEvent = LMouseDown XY MouseModifier | LMouseUp XY | LMouseCancel


data Tool = TSelect | TPan | TBox | TLine | TText deriving (Eq, Show, Enum)

data LayerDisplay = LayerDisplay {
  _layerDisplay_isFolder :: Bool
  , _layerDisplay_name   :: Text
  , _layerDisplay_ident  :: Int
  -- TODO hidden/locked states
  -- TODO reverse mapping to selt
}

data MouseManipulator = MouseManipulator {

}


data EverythingBackend = EverythingBackend {
  _everythingBackend_selectedTool   :: Tool
  , _everythingBackend_selection    :: [SuperSEltLabel] -- always in order and valid
  , _everythingBackend_layers       :: Seq LayerDisplay
  , _everythingBackend_manipulators :: [MouseManipulator]
  , _everythingBackend_pan          :: XY -- panPos is position of upper left corner of canvas relative to screen
  , _everythingBackend_broadPhase   :: BPTree
}

data EverythingCmd =
  -- panning
  ECmd_Pan XY
  | ECmd_PanMouseEv LMouseEvent

  -- manipulators
  | ECmd_ManipulatorMouseEv LMouseEvent

  -- selection (second param is add or overwrite)
  | ECmd_Select [LayerPos] Bool

data EverythingWidgetConfig t = EverythingWidgetConfig {
}

data EverythingWidget t = EverythingWidget {
  --_everythingWidget
}
