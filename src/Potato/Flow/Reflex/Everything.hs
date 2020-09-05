{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

-- TODO move out of Reflex folder
module Potato.Flow.Reflex.Everything (
  KeyboardData(..)
  , MouseModifier(..)
  , LMouseData(..)
  , Tool(..)
  , LayerDisplay(..)
  , MouseManipulator(..)
  , Selection
  , disjointUnionSelection
  , EverythingBackend(..)
  , emptyEverythingBackend
) where

import           Relude

import           Potato.Flow.BroadPhase
import           Potato.Flow.Math
import           Potato.Flow.Reflex.RElts
import           Potato.Flow.SElts
import           Potato.Flow.Types

import           Data.Dependent.Sum       (DSum ((:=>)), (==>))
import qualified Data.List                as L
import qualified Data.Sequence            as Seq

-- KEYBOARD
-- TODO decide if text input happens here or in front end
-- (don't wanna implement my own text zipper D:)
data KeyboardData =
  KeyboardData_Esc
  | KeyboardData_Return
  | KeyboardData_Space
  | KeyboardData_Char Char

-- MOUSE
-- TODO move all this stuff to types folder or something
-- only ones we care about
data MouseModifier = MouseModifier_Shift | MouseModifier_Alt

data MouseButton = MouseButton_Left | MouseButton_Middle | MouseButton_Right

-- TODO Get rid of cancel? It will be sent over via escape key
data MouseDragState = MouseDragState_Down | MouseDragState_Dragging | MouseDragState_Up | MouseDragState_Cancel

-- TODO is this the all encompassing mouse event we want?
-- only one modifier allowed at a time for our app
-- TODO is there a way to optionally support more fidelity here?
data LMouseData = LMouseData {
  _lMouseData_position    :: XY
  , _lMouseData_isRelease :: Bool
  , _lMouseData_button    :: MouseButton
  , _lMouseData_dragState :: MouseDragState
}

data MouseDrag = MouseDrag {
  _mouseDrag_from          :: XY
  , _mouseDrag_startButton :: MouseButton
  -- likely not needed as they will be in the input event
  , _mouseDrag_to          :: XY
  , _mouseDrag_state       :: MouseDragState
}


-- TOOL
data Tool = TSelect | TPan | TBox | TLine | TText deriving (Eq, Show, Enum)

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

computeSelectionType :: Selection -> SelectionManipulatorType
computeSelectionType = foldl' foldfn SMTNone where
  foldfn accType (_,_,SEltLabel _ selt) = case accType of
    SMTNone -> case selt of
      SEltBox _  -> SMTBox
      SEltLine _ -> SMTLine
      SEltText _ -> SMTText
      _          -> SMTNone
    _ -> SMTBoundingBox


-- MANIPULATORS
data MouseManipulatorType = MouseManipulatorType_Corner | MouseManipulatorType_Point
data MouseManipulatorState = MouseManipulatorState_Normal | MouseManipulatorState_Dragging

data MouseManipulator = MouseManipulator {
  _mouseManipulator_pos     :: XY
  , _mouseManipulator_type  :: MouseManipulatorType
  , _mouseManipulator_state :: MouseManipulatorState
}


-- REDUCERS/REDUCER HELPERS
toManipulator :: Selection -> Manipulator
toManipulator selected = r where
  nilState :: Manipulator
  nilState = (MTagNone ==> ())
  r = case toList selected of
    [] -> nilState
    ((rid, _, SEltLabel _ selt):[]) -> case selt of
      SEltBox SBox {..} -> (MTagBox ==> mbox) where
        mbox = MBox {
            _mBox_target = rid
            , _mBox_box  = _sBox_box
          }
      SEltLine SLine {..} -> (MTagLine ==> mline) where
        mline = MLine {
            _mLine_target  = rid
            , _mLine_start = _sLine_start
            , _mLine_end   = _sLine_end
          }
      SEltText SText {..} -> (MTagText ==> mtext) where
        mtext = MText {
            _mText_target = rid
            , _mText_box  = _sText_box
            , _mText_text = _sText_text
          }
      _                 -> nilState
    sss -> bb where
      fmapfn (rid, _, seltl) = do
        box <- getSEltBox . _sEltLabel_sElt $ seltl
        return (rid,box)
      msboxes = catMaybes $ fmap fmapfn sss
      bb = fromMaybe nilState $ flip viaNonEmpty msboxes $ \sboxes ->
        MTagBoundingBox ==>
          MBoundingBox {
            _mBoundingBox_bounded_targets = sboxes
          }

toMouseManipulators :: Selection -> [MouseManipulator]
toMouseManipulators selection = undefined
--TODO do something like toManipulator
-- so convert to Manipulator first and then convert Manipulator to MouseManipulator

changeSelection :: Selection -> EverythingBackend -> EverythingBackend
changeSelection newSelection everything@EverythingBackend {..} = everything {
    _everythingBackend_selection = newSelection
    , _everythingBackend_manipulators = toMouseManipulators newSelection
  }



-- TODO rename to Everything
data EverythingBackend = EverythingBackend {
  _everythingBackend_selectedTool   :: Tool
  , _everythingBackend_selection    :: Selection
  , _everythingBackend_layers       :: Seq LayerDisplay
  , _everythingBackend_manipulators :: [MouseManipulator]
  , _everythingBackend_pan          :: XY -- panPos is position of upper left corner of canvas relative to screen
  , _everythingBackend_broadPhase   :: BPTree
}

emptyEverythingBackend :: EverythingBackend
emptyEverythingBackend = EverythingBackend {
    _everythingBackend_selectedTool   = TSelect
    , _everythingBackend_selection    = Seq.empty
    , _everythingBackend_layers       = Seq.empty
    , _everythingBackend_manipulators = []
    , _everythingBackend_pan          = V2 0 0
    , _everythingBackend_broadPhase   = emptyBPTree
  }
