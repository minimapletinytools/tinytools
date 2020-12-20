{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.Everything (
  EverythingFrontend(..)
  , everythingFrontend_isHandlerActive
  , EverythingBackend(..)
  , emptyEverythingFrontend
  , emptyEverythingBackend
  , EverythingCombined_DEBUG(..)
  , combineEverything

) where

import           Relude

import           Potato.Flow.BroadPhase
import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.Layers
import           Potato.Flow.Math
import           Potato.Flow.Render
import           Potato.Flow.State

-- erhm, maybe move PFEventTag to somewhere else? Could just duplicate it in this file
import qualified Data.Sequence                  as Seq
import qualified Data.IntMap                  as IM
import           Potato.Flow.Entry              (PFEventTag)
import Data.Aeson

-- first pass processing inputs
data EverythingFrontend = EverythingFrontend {
  _everythingFrontend_selectedTool     :: Tool
  , _everythingFrontend_pan            :: XY -- panPos is position of upper left corner of canvas relative to screen
  , _everythingFrontend_mouseDrag      :: MouseDrag -- last mouse dragging state, this is a little questionable, arguably we should only store stuff needed, not the entire mouseDrag

  , _everythingFrontend_handler        :: SomePotatoHandler
  , _everythingFrontend_pFEvent        :: Maybe PFEventTag -- one shot event passed onto PF
  , _everythingFrontend_select         :: Maybe (Bool, Selection) -- one shot

  , _everythingFrontend_layerScrollPos :: Int

  , _everythingFrontend_debugLabel     :: Text
} deriving (Show)

everythingFrontend_isHandlerActive :: EverythingFrontend -> Bool
everythingFrontend_isHandlerActive EverythingFrontend {..} = case _everythingFrontend_handler of
  SomePotatoHandler h -> pIsHandlerActive h

-- second pass, taking outputs from PFOutput
data EverythingBackend = EverythingBackend {
  _everythingBackend_selection              :: Selection
  , _everythingBackend_layers               :: Seq LayerDisplay
  , _everythingBackend_broadPhaseState      :: BroadPhaseState
  , _everythingBackend_renderedCanvas       :: RenderedCanvas
  , _everythingBackend_handlerFromSelection :: Maybe SomePotatoHandler
} deriving (Show)

emptyEverythingFrontend :: EverythingFrontend
emptyEverythingFrontend = EverythingFrontend {
    _everythingFrontend_selectedTool   = Tool_Select
    , _everythingFrontend_pan          = V2 0 0
    , _everythingFrontend_mouseDrag = emptyMouseDrag
    , _everythingFrontend_handler = SomePotatoHandler EmptyHandler
    , _everythingFrontend_select = Nothing
    , _everythingFrontend_layerScrollPos = 0

    , _everythingFrontend_debugLabel = ""
  }

emptyEverythingBackend :: EverythingBackend
emptyEverythingBackend = EverythingBackend {
    _everythingBackend_selection    = Seq.empty
    , _everythingBackend_layers       = Seq.empty
    , _everythingBackend_broadPhaseState   = emptyBroadPhaseState
    , _everythingBackend_renderedCanvas = emptyRenderedCanvas nilLBox
    , _everythingBackend_handlerFromSelection = Nothing
  }

-- combined output for convenient testing thx
data EverythingCombined_DEBUG = EverythingCombined_DEBUG {
  _everythingCombined_selectedTool           :: Tool
  , _everythingCombined_pan                  :: XY -- panPos is position of upper left corner of canvas relative to screen
  , _everythingCombined_mouseDrag            :: MouseDrag -- last mouse dragging state

  , _everythingCombined_handler              :: SomePotatoHandler
  , _everythingCombined_pFEvent              :: Maybe PFEventTag -- one shot event passed onto PF
  , _everythingCombined_select               :: Maybe (Bool, Selection) -- one shot
  , _everythingCombined_layerScrollPos       :: Int

  , _everythingCombined_debugLabel           :: Text

  , _everythingCombined_selection            :: Selection
  , _everythingCombined_layers               :: Seq LayerDisplay
  , _everythingCombined_broadPhase           :: BroadPhaseState
  , _everythingCombined_renderedCanvas       :: RenderedCanvas
  , _everythingCombined_handlerFromSelection :: Maybe SomePotatoHandler

  -- from PFOutput, remember to set
  , _everythingCombined_pFState              :: PFState
}

combineEverything :: EverythingFrontend -> EverythingBackend -> PFState -> EverythingCombined_DEBUG
combineEverything EverythingFrontend {..} EverythingBackend {..} pfs = EverythingCombined_DEBUG {
    _everythingCombined_selectedTool =   _everythingFrontend_selectedTool
    , _everythingCombined_pan        = _everythingFrontend_pan
    , _everythingCombined_mouseDrag = _everythingFrontend_mouseDrag
    , _everythingCombined_handler = _everythingFrontend_handler
    , _everythingCombined_pFEvent = _everythingFrontend_pFEvent
    , _everythingCombined_select = _everythingFrontend_select
    , _everythingCombined_layerScrollPos = _everythingFrontend_layerScrollPos
    , _everythingCombined_debugLabel = _everythingFrontend_debugLabel

    , _everythingCombined_selection      = _everythingBackend_selection
    , _everythingCombined_layers       = _everythingBackend_layers
    , _everythingCombined_broadPhase   = _everythingBackend_broadPhaseState
    , _everythingCombined_renderedCanvas   = _everythingBackend_renderedCanvas
    , _everythingCombined_handlerFromSelection = _everythingBackend_handlerFromSelection

    , _everythingCombined_pFState = pfs
  }


data ControllerMeta = ControllerMeta {
  _controllerMeta_pan :: XY
  , _controllerMeta_layers :: LayerMetaMap
  , _controllerMeta_layerScrollPos :: Int
} deriving (Show, Eq, Generic)

instance FromJSON ControllerMeta
instance ToJSON ControllerMeta

-- TODO
updateFrontEndFromMeta :: ControllerMeta -> EverythingFrontend -> EverythingFrontend
updateFrontEndFromMeta = undefined
