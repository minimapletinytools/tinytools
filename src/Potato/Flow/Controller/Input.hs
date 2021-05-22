{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Input (
  KeyModifier(..)
  , KeyboardData(..)
  , KeyboardKey(..)
  , MouseButton(..)
  , MouseDragState(..)
  , LMouseData(..)
  , MouseDrag(..)
  , mouseDrag_isActive
  , newDrag
  , continueDrag
  , cancelDrag
  , mouseDragDelta
  , RelMouseDrag(..)
  , toRelMouseDrag
) where

import           Relude

import           Potato.Flow.Math
import           Potato.Flow.OwlState
import           Potato.Flow.Types

import           Control.Exception (assert)
import           Data.Default
import qualified Data.List         as L
import qualified Data.Sequence     as Seq

-- TODO consider adding space as a modifier so you can do space drag panning in web version
data KeyModifier = KeyModifier_Shift | KeyModifier_Alt | KeyModifier_Ctrl deriving (Show, Eq)

-- KEYBOARD
data KeyboardData = KeyboardData KeyboardKey [KeyModifier] deriving (Show)

data KeyboardKey =
  KeyboardKey_Esc
  | KeyboardKey_Return
  | KeyboardKey_Space
  | KeyboardKey_Delete
  | KeyboardKey_Backspace
  | KeyboardKey_Left
  | KeyboardKey_Right
  | KeyboardKey_Up
  | KeyboardKey_Down
  | KeyboardKey_Home
  | KeyboardKey_End
  | KeyboardKey_PageUp
  | KeyboardKey_PageDown
  | KeyboardKey_Char Char
  -- not really a keypress but it's fine to put it here
  | KeyboardKey_Paste Text
  deriving (Show, Eq)

-- MOUSE
data MouseButton = MouseButton_Left | MouseButton_Middle | MouseButton_Right deriving (Show, Eq)

data MouseDragState = MouseDragState_Down | MouseDragState_Dragging | MouseDragState_Up | MouseDragState_Cancelled deriving (Show, Eq)

-- TODO add modifier
-- TODO is this the all encompassing mouse event we want?
-- TODO is there a way to optionally support more fidelity here?
-- mouse drags are sent as click streams
data LMouseData = LMouseData {
  _lMouseData_position       :: XY
  , _lMouseData_isRelease    :: Bool
  , _lMouseData_button       :: MouseButton
  , _lMouseData_modifiers    :: [KeyModifier]
  , _lMouseData_isLayerMouse :: Bool
} deriving (Show, Eq)

data MouseDrag = MouseDrag {
  _mouseDrag_from           :: XY
  , _mouseDrag_button       :: MouseButton -- tracks button on start of drag
  , _mouseDrag_modifiers    :: [KeyModifier] -- tracks modifiers held at current state of drag
  , _mouseDrag_to           :: XY -- likely not needed as they will be in the input event, but whatever
  , _mouseDrag_state        :: MouseDragState
  , _mouseDrag_isLayerMouse :: Bool
} deriving (Show, Eq)

mouseDrag_isActive :: MouseDrag -> Bool
mouseDrag_isActive MouseDrag {..} = case _mouseDrag_state of
  MouseDragState_Down     -> True
  MouseDragState_Dragging -> True
  _                       -> False

instance Default MouseDrag where
  def = MouseDrag {
      _mouseDrag_from  = 0
      , _mouseDrag_button = MouseButton_Left
      , _mouseDrag_modifiers = []
      , _mouseDrag_to    = 0
      , _mouseDrag_state = MouseDragState_Up -- if the last state was MouseDragState_Up we are ready to process more inputs fresh
      , _mouseDrag_isLayerMouse = False
    }

newDrag :: LMouseData -> MouseDrag
newDrag LMouseData {..} = assert (not _lMouseData_isRelease) $ MouseDrag {
    _mouseDrag_from = _lMouseData_position
    , _mouseDrag_button = _lMouseData_button
    , _mouseDrag_modifiers = _lMouseData_modifiers
    , _mouseDrag_to = _lMouseData_position
    , _mouseDrag_state = MouseDragState_Down
    , _mouseDrag_isLayerMouse = _lMouseData_isLayerMouse
  }

continueDrag :: LMouseData -> MouseDrag -> MouseDrag
continueDrag LMouseData {..} md = md {
    _mouseDrag_to = _lMouseData_position
    , _mouseDrag_state = if _lMouseData_isRelease
      then MouseDragState_Up
      else MouseDragState_Dragging
    , _mouseDrag_modifiers = _lMouseData_modifiers
  }

cancelDrag :: MouseDrag -> MouseDrag
cancelDrag md = md { _mouseDrag_state = case _mouseDrag_state md of
    MouseDragState_Up -> MouseDragState_Up
    _                 -> MouseDragState_Cancelled
  }

-- wats this for D:?
mouseDragDelta :: MouseDrag -> MouseDrag -> XY
mouseDragDelta md prev = (_mouseDrag_to md) - (_mouseDrag_to prev)

newtype RelMouseDrag = RelMouseDrag MouseDrag deriving (Show)

toRelMouseDrag :: OwlPFState -> XY -> MouseDrag -> RelMouseDrag
toRelMouseDrag pfs pan md = RelMouseDrag $ md {
    _mouseDrag_from = owlPFState_toCanvasCoordinates pfs (_mouseDrag_from md) - pan
    , _mouseDrag_to = owlPFState_toCanvasCoordinates pfs (_mouseDrag_to md) - pan
  }
