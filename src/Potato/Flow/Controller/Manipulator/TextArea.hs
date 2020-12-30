{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Manipulator.TextArea (
  TextAreaHandler(..)
  , TextAreaInputState(..)
  , makeTextAreaHandler

  -- exposed for testing
  , makeTextAreaInputState
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

getSText :: Selection -> SText
getSText selection = case selectionToSuperSEltLabel selection of
  (_,_,SEltLabel _ (SEltText stext)) -> stext
  (_,_,SEltLabel _ selt) -> error $ "expected SEltText, got " <> show selt

data TextAreaInputState = TextAreaInputState {
  _textAreaInputState_original   :: Text -- needed to properly create DeltaText for undo
  , _textAreaInputState_raw      :: Text -- we can always pull this from selection, but may as well store it (useful for validation)
  , _textAreaInputState_box      :: LBox -- we can always pull this from selection, but may as well store it
  , _textAreaInputState_zipper   :: TZ.TextZipper
  , _textAreaInputState_selected :: Int -- WIP
} deriving (Show)

instance Default TextAreaInputState where
  def = TextAreaInputState "" "" (LBox 0 0) TZ.empty 0

-- TODO I think you need to pad empty lines in the zipper to fill out the box D:
-- ok, no you don't, that's only for the non-paragraph text area that we don't actually have yet
makeTextAreaInputState :: SText -> RelMouseDrag -> TextAreaInputState
makeTextAreaInputState stext rmd = r where
  ogtz = TZ.fromText (_sText_text stext)
  r' = TextAreaInputState {
      _textAreaInputState_original   = _sText_text stext
      , _textAreaInputState_raw = _sText_text stext
      , _textAreaInputState_box = _sText_box stext
      , _textAreaInputState_zipper   = ogtz
      , _textAreaInputState_selected = 0
    }
  r = mouseText (Just r') stext rmd

-- TODO define behavior for when you click outside box or assert
mouseText :: Maybe TextAreaInputState -> SText -> RelMouseDrag -> TextAreaInputState
mouseText mtais stext rmd = r where
  RelMouseDrag MouseDrag {..} = rmd
  r = case mtais of
    Nothing -> makeTextAreaInputState stext rmd
    Just tais -> tais { _textAreaInputState_zipper = newtz } where
      ogtz = _textAreaInputState_zipper tais
      LBox (V2 x y) (V2 w _) = _sText_box stext
      -- TODO clip/overflow/wrap mode
      dl = TZ.displayLines w () () ogtz
      V2 mousex mousey = _mouseDrag_to
      newtz = TZ.goToDisplayLinePosition (mousex-x) (mousey-y) dl ogtz

-- TODO support shift selecting text someday meh
inputText :: TextAreaInputState -> Bool -> SuperSEltLabel -> KeyboardKey -> (TextAreaInputState, Maybe WSEvent)
inputText tais undoFirst selected kk = (tais { _textAreaInputState_zipper = newZip }, mop) where

  oldZip = _textAreaInputState_zipper tais
  (changed, newZip) = case kk of
    KeyboardKey_Left    -> (False, TZ.left oldZip)
    KeyboardKey_Right   -> (False, TZ.right oldZip)
    KeyboardKey_Up      -> (False, TZ.up oldZip)
    KeyboardKey_Down    -> (False, TZ.down oldZip)

    KeyboardKey_Return  -> (False, TZ.insertChar '\n' oldZip)
    KeyboardKey_Space   -> (False, TZ.insertChar ' ' oldZip)
    KeyboardKey_Char c  -> (False, TZ.insertChar c oldZip)
    KeyboardKey_Paste t -> (False, TZ.insert t oldZip)

    KeyboardKey_Esc                   -> error "unexpected keyboard char (escape should be handled outside)"

  controller = CTagText :=> (Identity $ CText {
      _cText_deltaText = (_textAreaInputState_original tais, TZ.value newZip)
    })
  mop = if changed
    then Just $ WSEManipulate (undoFirst, IM.fromList [(fst3 selected,controller)])
    else Nothing

-- text area handler state and the text it represents are updated independently and they should always be consistent
checkTextAreaHandlerStateIsConsistent :: TextAreaInputState -> SText -> Bool
checkTextAreaHandlerStateIsConsistent TextAreaInputState {..} SText {..} = r where
  LBox _ (V2 w _) = _sText_box
  LBox _ (V2 bw _) = _textAreaInputState_box
  r = _textAreaInputState_raw == _sText_text && w == bw

data TextAreaHandler = TextAreaHandler {
    -- TODO rename to active
    _textAreaHandler_isActive      :: Bool
    , _textAreaHandler_state       :: TextAreaInputState
    , _textAreaHandler_prevHandler :: SomePotatoHandler
    , _textAreaHandler_undoFirst   :: Bool
  }

makeTextAreaHandler :: SomePotatoHandler -> Selection -> RelMouseDrag -> TextAreaHandler
makeTextAreaHandler prev selection rmd = TextAreaHandler {
      _textAreaHandler_isActive = False
      , _textAreaHandler_state = makeTextAreaInputState (getSText selection) rmd
      , _textAreaHandler_prevHandler = prev
      , _textAreaHandler_undoFirst = False
    }

updateTextAreaHandlerState :: Selection -> TextAreaHandler -> TextAreaHandler
updateTextAreaHandlerState selection tah@TextAreaHandler {..} = r where
  stext = getSText selection
  nextstate = _textAreaHandler_state
  -- TODO resize zipper
  r = tah {
    _textAreaHandler_state = nextstate
  }

instance PotatoHandler TextAreaHandler where
  pHandlerName _ = handlerName_textArea
  pHandleMouse tah' PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) = let
      tah@TextAreaHandler {..} = updateTextAreaHandlerState _potatoHandlerInput_selection tah'
      stext = getSText _potatoHandlerInput_selection
      validateFirst = assert (checkTextAreaHandlerStateIsConsistent _textAreaHandler_state stext)
    in validateFirst $ case _mouseDrag_state of
      MouseDragState_Down -> r where
        clickOutside = does_lBox_contains_XY (_textAreaInputState_box _textAreaHandler_state) _mouseDrag_from
        newState = mouseText (Just _textAreaHandler_state) stext rmd
        r = if clickOutside
          then Nothing
          else Just $ def {
              _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler tah {
                  _textAreaHandler_isActive = True
                  , _textAreaHandler_state = newState
                }
            }

      -- TODO drag select text someday
      MouseDragState_Dragging -> Just $ captureWithNoChange tah

      MouseDragState_Up -> Just $ def {
          _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler tah {
              _textAreaHandler_isActive = False
              --, _textAreaHandler_undoFirst = False -- this variant adds new undo point each time cursor is moved
            }
        }
      MouseDragState_Cancelled -> Just $ captureWithNoChange tah
      _ -> error "unexpected mouse state passed to handler"

  pHandleKeyboard tah' PotatoHandlerInput {..} (KeyboardData k mods) = case k of
    KeyboardKey_Esc -> Just $ def { _potatoHandlerOutput_nextHandler = Just (_textAreaHandler_prevHandler tah') }
    _ -> Just r where
      tah@TextAreaHandler {..} = updateTextAreaHandlerState _potatoHandlerInput_selection tah'
      sseltl = selectionToSuperSEltLabel _potatoHandlerInput_selection
      -- TODO decide what to do with mods
      (nexttais, mev) = inputText _textAreaHandler_state _textAreaHandler_undoFirst sseltl k
      r = def {
          _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler tah {
              _textAreaHandler_state  = nexttais
              , _textAreaHandler_undoFirst = case mev of
                Nothing -> _textAreaHandler_undoFirst
                --Nothing -> False -- this variant adds new undo point each time cursoer is moved
                Just _  -> True
            }
          , _potatoHandlerOutput_pFEvent = mev
        }

  pRenderHandler tah PotatoHandlerInput {..} = def
  pIsHandlerActive = _textAreaHandler_isActive
