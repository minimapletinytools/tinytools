{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.Everything (
  MouseModifier(..)
  , LMouseData(..)
  , Tool(..)
  , LayerDisplay(..)
  , MouseManipulator(..)
  , EverythingBackend(..)
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
data LMouseData = LMouseDown XY MouseModifier | LMouseUp XY | LMouseCancel


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

  -- selection (second param is add or overwrite)
  ECmd_Select [LayerPos] Bool

data EverythingWidgetConfig t = EverythingWidgetConfig {
  _everythingWidgetConfig_selectTool  :: Event t Tool
  , _everythingWidgetConfig_mouse     :: Event t LMouseData
  , _everythingWidgetConfig_selectNew :: Event t [LayerPos]
  , _everythingWidgetConfig_selectAdd :: Event t [LayerPos]
}

data EverythingWidget t = EverythingWidget {
  _everythingWidget_tool           :: Dynamic t Tool
  , _everythingWidget_selection    :: Dynamic t [SuperSEltLabel]
  , _everythingWidget_layers       :: Dynamic t (Seq LayerDisplay)
  , _everythingWidget_manipulators :: Dynamic t [MouseManipulator]
  , _everythingWidget_pan          :: Dynamic t XY
  , _everythingWidget_broadPhase   :: Dynamic t BPTree
}

holdEverythingWidget :: forall t m. (Adjustable t m, MonadHold t m, MonadFix m)
  => EverythingWidgetConfig t
  -> m (EverythingWidget t)
holdEverythingWidget EverythingWidgetConfig {..} = mdo

  return EverythingWidget
    {
      _everythingWidget_tool           = undefined
      , _everythingWidget_selection    = undefined
      , _everythingWidget_layers       = undefined
      , _everythingWidget_manipulators = undefined
      , _everythingWidget_pan          = undefined
      , _everythingWidget_broadPhase   = undefined

    }
