{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.Everything (
  MouseModifier(..)
  , LMouseData(..)
  , Tool(..)
  , LayerDisplay(..)
  , MouseManipulator(..)
  , Selection
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
import           Potato.Flow.Reflex.Entry
import           Potato.Flow.SElts
import           Potato.Flow.State
import           Potato.Flow.Types
import           Reflex.Potato.Helpers

import           Control.Exception             (assert)
import           Control.Monad.Fix
import qualified Data.List                     as L
import qualified Data.Sequence                 as Seq
import           Data.Tuple.Extra


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
data MouseManipulator = MouseManipulator {

}


-- TODO rename to Everything, move to types folder maybe?
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

data EverythingCmd =
  ECmdTool Tool
  -- selection (first param is add to selection if true)
  | ECmdSelect Bool Selection

  -- canvas direct input
  | ECmdMouse LMouseData
  | ECmdKeyboard KeyboardData


data EverythingWidgetConfig t = EverythingWidgetConfig {
  _everythingWidgetConfig_potatoFlow   :: PFOutput t

  -- canvas direct input
  , _everythingWidgetConfig_mouse      :: Event t LMouseData
  , _everythingWidgetConfig_keyboard   :: Event t KeyboardData

  -- command based
  , _everythingWidgetConfig_selectTool :: Event t Tool
  , _everythingWidgetConfig_selectNew  :: Event t Selection
  , _everythingWidgetConfig_selectAdd  :: Event t Selection
}

emptyEverythingWidgetConfig :: (Reflex t) => EverythingWidgetConfig t
emptyEverythingWidgetConfig = EverythingWidgetConfig {
    _everythingWidgetConfig_potatoFlow = undefined
    , _everythingWidgetConfig_selectTool  = never
    , _everythingWidgetConfig_mouse     = never
    , _everythingWidgetConfig_keyboard = never
    , _everythingWidgetConfig_selectNew = never
    , _everythingWidgetConfig_selectAdd = never
  }

data EverythingWidget t = EverythingWidget {
  _everythingWidget_tool           :: Dynamic t Tool
  , _everythingWidget_selection    :: Dynamic t Selection
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
    PFOutput {..} = _everythingWidgetConfig_potatoFlow

    -- TODO event for newly created element
    -- you could sample directory for "past" version when listening to _pfo_potato_changed for changes
    newEltsEvent :: Event t [LayerPos]
    newEltsEvent = undefined

    everythingEvent = leftmostWarn "EverythingWidgetConfig"
      [ ECmdTool <$> _everythingWidgetConfig_selectTool
      , ECmdSelect False <$> _everythingWidgetConfig_selectNew
      , ECmdSelect True <$> _everythingWidgetConfig_selectAdd
      , ECmdMouse <$> _everythingWidgetConfig_mouse
      , ECmdKeyboard <$> _everythingWidgetConfig_keyboard
      ]

    -- TODO you nee a DMAP because some of the cmd are allowed to happen at once
    -- order them so they run in the right order
    -- wait does this even work? Specific scenario
    -- create new object -> force a new selection...
    -- ok no it doesn't work, instead the state handler needs to do it without triggering a new event
    -- you could have state reducers and have one reducer call the other...
    -- actually manipulator events need to trigger PFO inputs and get RID for PFO output
    -- so what you actualyl need to do is intercept manipulator events and send them to PFO first
    -- then combine promptly PFO outputs with original inputs
    foldEverythingFn :: EverythingCmd -> EverythingBackend -> PushM t EverythingBackend
    foldEverythingFn cmd everything@EverythingBackend {..} = case cmd of
      ECmdTool x -> return $ everything { _everythingBackend_selectedTool = x }
      -- TODO assert that selection is valid

      ECmdSelect add x -> do
        pfState <- sample _pfo_pFState
        return $ assert (pFState_selectionIsValid pfState (fmap snd3 (toList x))) ()
        if add
          then return $ everything { _everythingBackend_selection = disjointUnionSelection _everythingBackend_selection x }
          else return $ everything { _everythingBackend_selection = x }
      ECmdMouse x -> undefined
      ECmdKeyboard x -> undefined
      _          -> undefined

  everythingDyn <- foldDynM foldEverythingFn emptyEverythingBackend everythingEvent

  r_tool <- holdUniqDyn $ fmap _everythingBackend_selectedTool everythingDyn
  r_selection <- holdUniqDyn $ fmap _everythingBackend_selection everythingDyn

  return EverythingWidget
    {
      _everythingWidget_tool           = r_tool
      , _everythingWidget_selection    = r_selection
      , _everythingWidget_layers       = undefined
      , _everythingWidget_manipulators = undefined
      , _everythingWidget_pan          = undefined
      , _everythingWidget_broadPhase   = undefined

    }
