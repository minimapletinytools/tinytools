{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Input (
  KeyboardData(..)
  , KeyboardKey(..)
  , KeyboardKeyType(..)

  , MouseModifier(..)
  , MouseButton(..)
  , MouseDragState(..)
  , LMouseData(..)
  , MouseDrag(..)
  , emptyMouseDrag
  , newDrag
  , continueDrag
  , cancelDrag
  , mouseDragDelta
  , RelMouseDrag(..)
  , toRelMouseDrag


  , Tool(..)
  , LayerDisplay(..)
  , Selection
  , disjointUnionSelection
  , SelectionManipulatorType(..)

) where

import           Relude

import           Potato.Flow.Math
import           Potato.Flow.State
import           Potato.Flow.Types

import           Control.Exception (assert)
import qualified Data.List         as L
import qualified Data.Sequence     as Seq

-- KEYBOARD
-- TODO decide if text input happens here or in front end
-- (don't wanna implement my own text zipper D:)
data KeyboardData = KeyboardData KeyboardKey KeyboardKeyType

data KeyboardKey =
  KeyboardKey_Esc
  | KeyboardKey_Return
  | KeyboardKey_Space
  | KeyboardKey_Char Char
  deriving (Show, Eq)

data KeyboardKeyType =
  KeyboardKeyType_Down
  | KeyboardKeyType_Up
  | KeyboardKeyType_Click
  deriving (Show, Eq)

-- MOUSE
-- TODO move all this stuff to types folder or something
-- only ones we care about
data MouseModifier = MouseModifier_Shift | MouseModifier_Alt deriving (Show, Eq)

data MouseButton = MouseButton_Left | MouseButton_Middle | MouseButton_Right deriving (Show, Eq)

data MouseDragState = MouseDragState_Down | MouseDragState_Dragging | MouseDragState_Up | MouseDragState_Cancelled deriving (Show, Eq)

-- TODO add modifier
-- TODO is this the all encompassing mouse event we want?
-- TODO is there a way to optionally support more fidelity here?
-- mouse drags are sent as click streams
data LMouseData = LMouseData {
  _lMouseData_position    :: XY
  , _lMouseData_isRelease :: Bool
  , _lMouseData_button    :: MouseButton
  , _lMouseData_modifiers :: [MouseModifier]
} deriving (Show, Eq)

data MouseDrag = MouseDrag {
  _mouseDrag_from        :: XY
  , _mouseDrag_button    :: MouseButton -- tracks button on start of drag
  , _mouseDrag_modifiers :: [MouseModifier] -- tracks modifiers held at current state of drag
  , _mouseDrag_to        :: XY -- likely not needed as they will be in the input event, but whatever
  , _mouseDrag_state     :: MouseDragState
} deriving (Show, Eq)

emptyMouseDrag :: MouseDrag
emptyMouseDrag = MouseDrag {
    _mouseDrag_from  = 0
    , _mouseDrag_button = MouseButton_Left
    , _mouseDrag_modifiers = []
    , _mouseDrag_to    = 0
    , _mouseDrag_state = MouseDragState_Cancelled
  }

newDrag :: LMouseData -> MouseDrag
newDrag LMouseData {..} = assert (not _lMouseData_isRelease) $ MouseDrag {
    _mouseDrag_from = _lMouseData_position
    , _mouseDrag_button = _lMouseData_button
    , _mouseDrag_modifiers = _lMouseData_modifiers
    , _mouseDrag_to = _lMouseData_position
    , _mouseDrag_state = MouseDragState_Down
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
cancelDrag md = md { _mouseDrag_state = MouseDragState_Cancelled }

mouseDragDelta :: MouseDrag -> MouseDrag -> XY
mouseDragDelta md prev = (_mouseDrag_to md) - (_mouseDrag_to prev)

newtype RelMouseDrag = RelMouseDrag MouseDrag

toRelMouseDrag :: PFState -> MouseDrag -> RelMouseDrag
toRelMouseDrag pFState md = RelMouseDrag $ md {
    _mouseDrag_from = pFState_toCanvasCoordinates pFState (_mouseDrag_from md)
    , _mouseDrag_to = pFState_toCanvasCoordinates pFState (_mouseDrag_to md)
  }




-- TODO move out or rename this file to TYpes D:

-- TOOL
data Tool = Tool_Select | Tool_Pan | Tool_Box | Tool_Line | Tool_Text deriving (Eq, Show, Enum)

-- LAYER
data LayerDisplay = LayerDisplay {
  _layerDisplay_isFolder :: Bool
  , _layerDisplay_name   :: Text
  , _layerDisplay_ident  :: Int
  -- TODO hidden/locked states
  -- TODO reverse mapping to selt
}

-- SELECTION
type Selection = Seq SuperSEltLabel

-- TODO move to its own file
-- selection helpers
disjointUnion :: (Eq a) => [a] -> [a] -> [a]
disjointUnion a b = L.union a b L.\\ L.intersect a b

-- TODO real implementation...
disjointUnionSelection :: Selection -> Selection -> Selection
disjointUnionSelection s1 s2 = Seq.fromList $ disjointUnion (toList s1) (toList s2)

data SelectionManipulatorType = SMTNone | SMTBox | SMTLine | SMTText | SMTBoundingBox deriving (Show, Eq)
