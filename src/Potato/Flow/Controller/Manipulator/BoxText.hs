{-# LANGUAGE RecordWildCards #-}

-- TODO probably move this to Manipulator.Box.Text
module Potato.Flow.Controller.Manipulator.BoxText (
  BoxTextHandler(..)
  , BoxTextInputState(..)
  , makeBoxTextHandler

  -- exposed for testing
  , makeBoxTextInputState
  , mouseText

) where

import           Relude

import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.Manipulator.Common
import           Potato.Flow.Controller.Types
import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.Types
import           Potato.Flow.Workspace

import           Control.Exception
import           Data.Default
import           Data.Dependent.Sum                        (DSum ((:=>)))
import qualified Data.IntMap                               as IM
import qualified Data.Sequence                             as Seq
import qualified Data.Text.Zipper                          as TZ
import           Data.Tuple.Extra

getSBox :: Selection -> SBox
getSBox selection = case selectionToSuperSEltLabel selection of
  (_,_,SEltLabel _ (SEltBox sbox)) -> sbox
  (_,_,SEltLabel _ selt) -> error $ "expected SBox, got " <> show selt

data BoxTextInputState = BoxTextInputState {
  _boxTextInputState_original       :: Text -- needed to properly create DeltaText for undo
  , _boxTextInputState_box          :: LBox -- we can always pull this from selection, but may as well store it
  , _boxTextInputState_zipper       :: TZ.TextZipper
  , _boxTextInputState_displayLines :: TZ.DisplayLines ()
  --, _boxTextInputState_selected :: Int -- WIP
} deriving (Show)

--instance Default BoxTextInputState where
--  def = BoxTextInputState "" (LBox 0 0) TZ.empty undefined 0

-- TODO I think you need to pad empty lines in the zipper to fill out the box D:
-- ok, no you don't, that's only for the non-paragraph text area that we don't actually have yet
makeBoxTextInputState :: SBox -> RelMouseDrag -> BoxTextInputState
makeBoxTextInputState sbox rmd = r where
  ogtext = _sBoxText_text . _sBox_text $ sbox
  ogtz = TZ.fromText ogtext
  box@(LBox _ (V2 width _)) = _sBox_box sbox
  r' = BoxTextInputState {
      _boxTextInputState_original   = ogtext
      , _boxTextInputState_box = box
      , _boxTextInputState_zipper   = ogtz
      , _boxTextInputState_displayLines = TZ.displayLines width () () ogtz
      --, _boxTextInputState_selected = 0
    }
  r = mouseText (Just r') sbox rmd

-- TODO define behavior for when you click outside box or assert
mouseText :: Maybe BoxTextInputState -> SBox -> RelMouseDrag -> BoxTextInputState
mouseText mtais sbox rmd = r where
  RelMouseDrag MouseDrag {..} = rmd
  r = case mtais of
    Nothing -> makeBoxTextInputState sbox rmd
    Just tais -> tais { _boxTextInputState_zipper = newtz } where
      ogtz = _boxTextInputState_zipper tais
      LBox (V2 x y) (V2 w _) = _sBox_box sbox
      V2 mousex mousey = _mouseDrag_to
      newtz = TZ.goToDisplayLinePosition (mousex-x) (mousey-y) (_boxTextInputState_displayLines tais) ogtz

-- TODO support shift selecting text someday meh
inputText :: BoxTextInputState -> Bool -> SuperSEltLabel -> KeyboardKey -> (BoxTextInputState, Maybe WSEvent)
inputText tais undoFirst selected kk = (tais { _boxTextInputState_zipper = newZip }, mop) where

  oldZip = _boxTextInputState_zipper tais
  (changed, newZip) = case kk of
    KeyboardKey_Left    -> (False, TZ.left oldZip)
    KeyboardKey_Right   -> (False, TZ.right oldZip)
    KeyboardKey_Up      -> (False, TZ.up oldZip)
    KeyboardKey_Down    -> (False, TZ.down oldZip)

    KeyboardKey_Return  -> (True, TZ.insertChar '\n' oldZip)
    KeyboardKey_Space   -> (True, TZ.insertChar ' ' oldZip)
    KeyboardKey_Char c  -> (True, TZ.insertChar c oldZip)
    KeyboardKey_Paste t -> (True, TZ.insert t oldZip)

    KeyboardKey_Esc                   -> error "unexpected keyboard char (escape should be handled outside)"

  controller = CTagText :=> (Identity $ CText {
      _cText_deltaText = (_boxTextInputState_original tais, TZ.value newZip)
    })
  mop = if changed
    then Just $ WSEManipulate (undoFirst, IM.fromList [(fst3 selected,controller)])
    else Nothing

-- TODO rename to BoxTextHandler
data BoxTextHandler = BoxTextHandler {
    -- TODO rename to active
    _boxTextHandler_isActive      :: Bool
    , _boxTextHandler_state       :: BoxTextInputState
    -- TODO you can prob delete this now, we don't persist state between sub handlers in this case
    , _boxTextHandler_prevHandler :: SomePotatoHandler
    , _boxTextHandler_undoFirst   :: Bool
  }

makeBoxTextHandler :: SomePotatoHandler -> Selection -> RelMouseDrag -> BoxTextHandler
makeBoxTextHandler prev selection rmd = BoxTextHandler {
      _boxTextHandler_isActive = False
      , _boxTextHandler_state = makeBoxTextInputState (getSBox selection) rmd
      , _boxTextHandler_prevHandler = prev
      , _boxTextHandler_undoFirst = False
    }

updateBoxTextHandlerState :: Selection -> BoxTextHandler -> BoxTextHandler
updateBoxTextHandlerState selection tah@BoxTextHandler {..} = assert tzIsCorrect r where
  sbox = getSBox selection

  newText = _sBoxText_text . _sBox_text $ sbox

  recomputetz = TZ.fromText newText
  oldtz = _boxTextInputState_zipper _boxTextHandler_state
  -- NOTE that recomputetz won't have the same cursor position
  -- TODO delete this check, not very meaningful, but good for development purposes I guess
  tzIsCorrect = TZ.value oldtz == TZ.value recomputetz

  -- TODO refactor out into updateBoxTextStateNoTextZipper from SEltText
  newBox@(LBox _ (V2 width _)) = _sBox_box sbox
  nextstate = _boxTextHandler_state {
      _boxTextInputState_box = newBox
      , _boxTextInputState_displayLines = TZ.displayLines width () () oldtz
    }

  r = tah {
    _boxTextHandler_state = nextstate
  }

instance PotatoHandler BoxTextHandler where
  pHandlerName _ = handlerName_boxText
  pHandleMouse tah' PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) = let
      tah@BoxTextHandler {..} = updateBoxTextHandlerState _potatoHandlerInput_selection tah'
      sbox = getSBox _potatoHandlerInput_selection
    in trace "box mouse input" $ case _mouseDrag_state of
      MouseDragState_Down -> r where
        clickInside = does_lBox_contains_XY (_boxTextInputState_box _boxTextHandler_state) _mouseDrag_to
        newState = mouseText (Just _boxTextHandler_state) sbox rmd
        r = if clickInside
          then Just $ def {
              _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler tah {
                  _boxTextHandler_isActive = True
                  , _boxTextHandler_state = newState
                }
            }
          else Nothing

      -- TODO drag select text someday
      MouseDragState_Dragging -> Just $ captureWithNoChange tah

      MouseDragState_Up -> Just $ def {
          _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler tah {
              _boxTextHandler_isActive = False
              --, _boxTextHandler_undoFirst = False -- this variant adds new undo point each time cursor is moved
            }
        }
      MouseDragState_Cancelled -> Just $ captureWithNoChange tah
      _ -> error "unexpected mouse state passed to handler"

  pHandleKeyboard tah' PotatoHandlerInput {..} (KeyboardData k mods) = case k of
    KeyboardKey_Esc -> Just $ def { _potatoHandlerOutput_nextHandler = Just (_boxTextHandler_prevHandler tah') }
    _ -> Just r where
      tah@BoxTextHandler {..} = updateBoxTextHandlerState _potatoHandlerInput_selection tah'
      sseltl = selectionToSuperSEltLabel _potatoHandlerInput_selection

      -- TODO decide what to do with mods

      (nexttais, mev) = inputText _boxTextHandler_state _boxTextHandler_undoFirst sseltl k
      r = def {
          _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler tah {
              _boxTextHandler_state  = nexttais
              , _boxTextHandler_undoFirst = case mev of
                Nothing -> _boxTextHandler_undoFirst
                --Nothing -> False -- this variant adds new undo point each time cursoer is moved
                Just _  -> True
            }
          , _potatoHandlerOutput_pFEvent = mev
        }

  pRenderHandler tah PotatoHandlerInput {..} = def
  pIsHandlerActive = _boxTextHandler_isActive
