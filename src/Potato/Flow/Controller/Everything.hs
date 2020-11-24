{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.Everything (
  FrontendOperation(..)
  , EverythingFrontend(..)
  , EverythingBackend(..)
  , emptyEverythingFrontend
  , emptyEverythingBackend
  , EverythingCombined_DEBUG(..)
  , combineEverything

) where

import           Relude

import           Potato.Flow.BroadPhase
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.Manipulator
import           Potato.Flow.Math
import           Potato.Flow.Render
import           Potato.Flow.State

-- erhm, maybe move PFEventTag to somewhere else? Could just duplicate it in this file
import qualified Data.Sequence                      as Seq
import           Potato.Flow.Entry                  (PFEventTag)

-- TODO all data to pass onto backend/PFOutput should go here
data FrontendOperation =
  FrontendOperation_None
  | FrontendOperation_Pan
  | FrontendOperation_LayerDrag
  | FrontendOperation_Manipulate (Maybe PFEventTag) ManipulatorIndex
  | FrontendOperation_Undo
  | FrontendOperation_Selecting LBox
  | FrontendOperation_Select Bool Selection
  deriving (Show, Eq)

-- first pass processing inputs
data EverythingFrontend = EverythingFrontend {
  _everythingFrontend_selectedTool    :: Tool
  , _everythingFrontend_pan           :: XY -- panPos is position of upper left corner of canvas relative to screen
  , _everythingFrontend_mouseDrag     :: MouseDrag -- last mouse dragging state
  , _everythingFrontend_lastOperation :: FrontendOperation

  , _everythingFrontend_debugLabel    :: Text
} deriving (Show)

-- second pass, taking outputs from PFOutput
data EverythingBackend = EverythingBackend {
  _everythingBackend_selection         :: Selection
  , _everythingBackend_layers          :: Seq LayerDisplay
  , _everythingBackend_broadPhaseState :: BroadPhaseState
  , _everythingBackend_renderedCanvas  :: RenderedCanvas

}

emptyEverythingFrontend :: EverythingFrontend
emptyEverythingFrontend = EverythingFrontend {
    _everythingFrontend_selectedTool   = Tool_Select
    , _everythingFrontend_pan          = V2 0 0
    , _everythingFrontend_mouseDrag = emptyMouseDrag
    , _everythingFrontend_lastOperation = FrontendOperation_None
    , _everythingFrontend_debugLabel = ""
  }

emptyEverythingBackend :: EverythingBackend
emptyEverythingBackend = EverythingBackend {
    _everythingBackend_selection    = Seq.empty
    , _everythingBackend_layers       = Seq.empty
    , _everythingBackend_broadPhaseState   = emptyBroadPhaseState
    , _everythingBackend_renderedCanvas = emptyRenderedCanvas nilLBox
  }

-- combined output for convenient testing thx
data EverythingCombined_DEBUG = EverythingCombined_DEBUG {
  _everythingCombined_selectedTool     :: Tool
  , _everythingCombined_pan            :: XY -- panPos is position of upper left corner of canvas relative to screen
  , _everythingCombined_mouseDrag      :: MouseDrag -- last mouse dragging state
  , _everythingCombined_lastOperation  :: FrontendOperation
  , _everythingCombined_debugLabel     :: Text

  , _everythingCombined_selection      :: Selection
  , _everythingCombined_layers         :: Seq LayerDisplay
  , _everythingCombined_broadPhase     :: BroadPhaseState
  , _everythingCombined_renderedCanvas :: RenderedCanvas

  -- from PFOutput, remember to set
  , _everythingCombined_pFState        :: PFState
}

combineEverything :: EverythingFrontend -> EverythingBackend -> PFState -> EverythingCombined_DEBUG
combineEverything EverythingFrontend {..} EverythingBackend {..} pfs = EverythingCombined_DEBUG {
    _everythingCombined_selectedTool =   _everythingFrontend_selectedTool
    , _everythingCombined_pan        = _everythingFrontend_pan
    , _everythingCombined_mouseDrag = _everythingFrontend_mouseDrag
    , _everythingCombined_lastOperation = _everythingFrontend_lastOperation
    , _everythingCombined_debugLabel = _everythingFrontend_debugLabel

    , _everythingCombined_selection      = _everythingBackend_selection
    , _everythingCombined_layers       = _everythingBackend_layers
    , _everythingCombined_broadPhase   = _everythingBackend_broadPhaseState
    , _everythingCombined_renderedCanvas   = _everythingBackend_renderedCanvas

    , _everythingCombined_pFState = pfs
  }
