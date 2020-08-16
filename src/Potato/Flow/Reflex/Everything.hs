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
  , emptyEverythingWidgetConfig
  , EverythingWidget(..)
  , holdEverythingWidget
) where

import           Relude

import           Reflex
import           Reflex.Potato.Helpers

import           Potato.Flow.BroadPhase
import           Potato.Flow.Math
import           Potato.Flow.Reflex.BroadPhase
import           Potato.Flow.Types
import           Reflex.Potato.Helpers

import           Control.Monad.Fix
import qualified Data.Sequence                 as Seq

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

emptyEverythingBackend :: EverythingBackend
emptyEverythingBackend = EverythingBackend {
    _everythingBackend_selectedTool   = TSelect
    , _everythingBackend_selection    = []
    , _everythingBackend_layers       = Seq.empty
    , _everythingBackend_manipulators = []
    , _everythingBackend_pan          = V2 0 0
    , _everythingBackend_broadPhase   = emptyBPTree
  }

data EverythingCmd =
  ECmdTool Tool
  -- selection (second param is add or overwrite)
  | ECmdSelect [LayerPos] Bool

data EverythingWidgetConfig t = EverythingWidgetConfig {
  _everythingWidgetConfig_selectTool  :: Event t Tool
  , _everythingWidgetConfig_mouse     :: Event t LMouseData
  , _everythingWidgetConfig_selectNew :: Event t [LayerPos]
  , _everythingWidgetConfig_selectAdd :: Event t [LayerPos]
}

emptyEverythingWidgetConfig :: (Reflex t) => EverythingWidgetConfig t
emptyEverythingWidgetConfig = EverythingWidgetConfig {
    _everythingWidgetConfig_selectTool  = never
    , _everythingWidgetConfig_mouse     = never
    , _everythingWidgetConfig_selectNew = never
    , _everythingWidgetConfig_selectAdd = never
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

  let
    everythingEvent = leftmostWarn "EverythingWidgetConfig"
      [ ECmdTool <$> _everythingWidgetConfig_selectTool
      ]

    foldEverythingFn :: EverythingCmd -> EverythingBackend -> EverythingBackend
    foldEverythingFn cmd everything@EverythingBackend {..} = case cmd of
      ECmdTool x -> everything { _everythingBackend_selectedTool = x }
      _          -> undefined

  everythingDyn <- foldDyn foldEverythingFn emptyEverythingBackend everythingEvent

  r_tool <- holdUniqDyn $ fmap _everythingBackend_selectedTool everythingDyn

  return EverythingWidget
    {
      _everythingWidget_tool           = r_tool
      , _everythingWidget_selection    = undefined
      , _everythingWidget_layers       = undefined
      , _everythingWidget_manipulators = undefined
      , _everythingWidget_pan          = undefined
      , _everythingWidget_broadPhase   = undefined

    }
