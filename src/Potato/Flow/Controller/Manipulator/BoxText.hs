{-# LANGUAGE RecordWildCards #-}

-- TODO probably move this to Manipulator.Box.Text
module Potato.Flow.Controller.Manipulator.BoxText (
  BoxTextHandler(..)
  , TextInputState(..)
  , makeBoxTextHandler
  , BoxLabelHandler(..)
  , makeBoxLabelHandler
  , lBox_to_boxLabelBox

  -- exposed for testing
  , makeTextInputState
  , mouseText
  , getBoxTextOffset

) where

import           Relude

import Potato.Flow.Controller.Manipulator.TextInputState
import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.Manipulator.Common
import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.Types
import           Potato.Flow.OwlItem
import Potato.Flow.Owl
import           Potato.Flow.OwlItem
import Potato.Flow.OwlWorkspace
import Potato.Flow.Llama

import           Control.Exception
import           Data.Default
import           Data.Dependent.Sum                        (DSum ((:=>)))
import qualified Data.IntMap                               as IM
import qualified Data.Map as Map
import qualified Data.Sequence                             as Seq
import qualified Potato.Data.Text.Zipper                          as TZ
import qualified Text.Pretty.Simple as Pretty
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T

getSBox :: CanvasSelection -> (REltId, SBox)
getSBox selection = case superOwl_toSElt_hack sowl of
  SEltBox sbox -> (rid, sbox)
  selt -> error $ "expected SBox, got " <> show selt
  where
    sowl = selectionToSuperOwl selection
    rid = _superOwl_id sowl

updateTextInputStateWithSBox :: SBox -> TextInputState -> TextInputState
updateTextInputStateWithSBox sbox btis = r where
  alignment = convertTextAlignToTextZipperTextAlignment . _textStyle_alignment . _sBoxText_style . _sBox_text $ sbox
  CanonicalLBox _ _ newBox@(LBox _ (V2 width' _)) = canonicalLBox_from_lBox $ _sBox_box sbox
  width = case _sBox_boxType sbox of
    SBoxType_BoxText   -> max 0 (width'-2)
    SBoxType_NoBoxText -> width'
    _                  -> error "wrong type"
  r = btis {
      _textInputState_box = newBox
      , _textInputState_displayLines = TZ.displayLinesWithAlignment alignment width () () (_textInputState_zipper btis)
    }

-- TODO I think you need to pad empty lines in the zipper to fill out the box D:
-- ok, no you don't, that's only for the non-paragraph text area that we don't actually have yet
makeTextInputState :: REltId -> SBox -> RelMouseDrag -> TextInputState
makeTextInputState rid sbox rmd = r where
  ogtext = _sBoxText_text . _sBox_text $ sbox
  ogtz = TZ.fromText ogtext
  r' = TextInputState {
      _textInputState_rid = rid
      , _textInputState_original   = Just ogtext
      , _textInputState_zipper   = ogtz

      -- these fields get updated in next pass
      , _textInputState_box = error "expected to be filled"
      , _textInputState_displayLines = error "expected to be filled"

      --, _textInputState_selected = 0
    }
  r'' = updateTextInputStateWithSBox sbox r'
  r = mouseText r'' (_sBox_box sbox) rmd (getBoxTextOffset sbox)

getBoxTextOffset :: HasCallStack => SBox -> XY
getBoxTextOffset sbox =  case _sBox_boxType sbox of
  SBoxType_BoxText   -> V2 1 1
  SBoxType_NoBoxText -> 0
  _                  -> error "wrong type"



-- TODO support shift selecting text someday meh
-- | returns zipper in TextInputState after keyboard input has been applied
-- Bool indicates if there was any real input
inputBoxTextZipper :: TextInputState -> KeyboardKey -> (Bool, TextInputState)
inputBoxTextZipper tais kk = (changed, tais { _textInputState_zipper = newZip }) where

  oldZip = _textInputState_zipper tais
  (changed, newZip) = case kk of
    KeyboardKey_Left    -> (False, TZ.left oldZip)
    KeyboardKey_Right   -> (False, TZ.right oldZip)
    KeyboardKey_Up      -> (False, TZ.up oldZip)
    KeyboardKey_Down    -> (False, TZ.down oldZip)
    KeyboardKey_Home    -> (False, TZ.home oldZip)
    KeyboardKey_End -> (False, TZ.end oldZip)
    KeyboardKey_PageUp -> (False, TZ.pageUp 5 oldZip)
    KeyboardKey_PageDown -> (False, TZ.pageDown 5 oldZip)

    KeyboardKey_Return  -> (True, TZ.insertChar '\n' oldZip)
    KeyboardKey_Space   -> (True, TZ.insertChar ' ' oldZip)
    KeyboardKey_Delete  -> (True, TZ.deleteRight oldZip)
    KeyboardKey_Backspace -> (True, TZ.deleteLeft oldZip)
    KeyboardKey_Char c  -> (True, TZ.insertChar c oldZip)
    KeyboardKey_Paste t -> (True, TZ.insert t oldZip)

    k                   -> error $ "unexpected keyboard char (event should have been handled outside of this handler)" <> show k

inputBoxText :: TextInputState -> Bool -> SuperOwl -> KeyboardKey -> (TextInputState, Maybe WSEvent)
inputBoxText tais undoFirst sowl kk = (newtais, mop) where
  (changed, newtais) = inputBoxTextZipper tais kk
  controller = CTagBoxText :=> (Identity $ CBoxText {
      _cBoxText_deltaText = (fromMaybe "" (_textInputState_original tais), TZ.value (_textInputState_zipper newtais))
    })
  mop = if changed
    then Just $ WSEApplyLlama (undoFirst, makePFCLlama . OwlPFCManipulate $ IM.fromList [(_superOwl_id sowl,controller)])
    else Nothing

makeTextHandlerRenderOutput :: TextInputState -> XY -> HandlerRenderOutput
makeTextHandlerRenderOutput btis offset = r where
  dls = _textInputState_displayLines btis
  origBox = _textInputState_box $ btis
  (x, y) = TZ._displayLines_cursorPos dls
  offsetMap = TZ._displayLines_offsetMap dls

  mCursorChar = (fmap fst) . T.uncons . TZ._textZipper_after . _textInputState_zipper $ btis

  mlbox = do
    guard $ lBox_area origBox > 0

    -- TODO would be nice to assert that this exists...
    (alignxoff,_) <- Map.lookup y offsetMap
    let
      LBox p _ = _textInputState_box $ btis
      cursorh = RenderHandle {
          _renderHandle_box = LBox (p + (V2 (x + alignxoff) y) + offset) (V2 1 1)
          , _renderHandle_char = mCursorChar
          , _renderHandle_color = RHC_Default
        }
    return [cursorh]

  r = HandlerRenderOutput $ fromMaybe [] mlbox

data BoxTextHandler = BoxTextHandler {
    -- TODO rename to active
    _boxTextHandler_isActive      :: Bool
    , _boxTextHandler_state       :: TextInputState
    -- TODO you can prob delete this now, we don't persist state between sub handlers in this case
    , _boxTextHandler_prevHandler :: SomePotatoHandler
    , _boxTextHandler_undoFirst   :: Bool
  }

makeBoxTextHandler :: SomePotatoHandler -> CanvasSelection -> RelMouseDrag -> BoxTextHandler
makeBoxTextHandler prev selection rmd = BoxTextHandler {
      _boxTextHandler_isActive = False
      , _boxTextHandler_state = uncurry makeTextInputState (getSBox selection) rmd
      , _boxTextHandler_prevHandler = prev
      , _boxTextHandler_undoFirst = False
    }

updateBoxTextHandlerState :: Bool -> CanvasSelection -> BoxTextHandler -> BoxTextHandler
updateBoxTextHandlerState reset selection tah@BoxTextHandler {..} = assert tzIsCorrect r where
  (_, sbox) = getSBox selection

  newText = _sBoxText_text . _sBox_text $ sbox

  recomputetz = TZ.fromText newText
  oldtz = _textInputState_zipper _boxTextHandler_state
  -- NOTE that recomputetz won't have the same cursor position
  -- TODO delete this check, not very meaningful, but good for development purposes I guess
  tzIsCorrect = TZ.value oldtz == TZ.value recomputetz

  nextstate = updateTextInputStateWithSBox sbox _boxTextHandler_state

  r = tah {
    _boxTextHandler_state = if reset
      then nextstate {
          _textInputState_original = Just newText
        }
      else nextstate
    , _boxTextHandler_undoFirst = if reset
      then False
      else _boxTextHandler_undoFirst
  }

instance PotatoHandler BoxTextHandler where
  pHandlerName _ = handlerName_boxText
  pHandlerDebugShow BoxTextHandler {..} = LT.toStrict $ Pretty.pShowNoColor _boxTextHandler_state
  pHandleMouse tah' phi@PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) = let
      tah@BoxTextHandler {..} = updateBoxTextHandlerState False _potatoHandlerInput_canvasSelection tah'
      (_, sbox) = getSBox _potatoHandlerInput_canvasSelection
    in case _mouseDrag_state of
      MouseDragState_Down -> r where
        clickInside = does_lBox_contains_XY (_textInputState_box _boxTextHandler_state) _mouseDrag_to
        newState = mouseText _boxTextHandler_state (_sBox_box sbox) rmd (getBoxTextOffset sbox)
        r = if clickInside
          then Just $ def {
              _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler tah {
                  _boxTextHandler_isActive = True
                  , _boxTextHandler_state = newState
                }
            }
          -- pass the input on to the base handler (so that you can interact with BoxHandler mouse manipulators too)
          else pHandleMouse _boxTextHandler_prevHandler phi rmd

      -- TODO drag select text someday
      MouseDragState_Dragging -> Just $ captureWithNoChange tah

      MouseDragState_Up -> Just $ def {
          _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler tah {
              _boxTextHandler_isActive = False
              --, _boxTextHandler_undoFirst = False -- this variant adds new undo point each time cursor is moved
            }
        }
      MouseDragState_Cancelled -> Just $ captureWithNoChange tah

  pHandleKeyboard tah' PotatoHandlerInput {..} (KeyboardData k _) = case k of
    KeyboardKey_Esc -> Just $ def { _potatoHandlerOutput_nextHandler = Just (_boxTextHandler_prevHandler tah') }
    _ -> Just r where
      -- this regenerates displayLines unecessarily but who cares
      tah@BoxTextHandler {..} = updateBoxTextHandlerState False _potatoHandlerInput_canvasSelection tah'
      sowl = selectionToSuperOwl _potatoHandlerInput_canvasSelection

      -- TODO decide what to do with mods

      (nexttais, mev) = inputBoxText _boxTextHandler_state _boxTextHandler_undoFirst sowl k
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

  -- TODO do you need to reset _boxTextHandler_prevHandler as well?
  pRefreshHandler tah PotatoHandlerInput {..} = if Seq.null (unCanvasSelection _potatoHandlerInput_canvasSelection)
    then Nothing -- selection was deleted or something
    else if rid /= (_textInputState_rid $ _boxTextHandler_state tah)
      then Nothing -- selection was change to something else
      else case selt of
        SEltBox sbox -> if not $ sBoxType_isText (_sBox_boxType sbox)
          then Nothing -- SEltBox type changed to non-text
          else Just $ SomePotatoHandler $ updateBoxTextHandlerState True _potatoHandlerInput_canvasSelection tah
        _ -> Nothing
      where
        sowl = selectionToSuperOwl _potatoHandlerInput_canvasSelection
        rid = _superOwl_id sowl
        selt = superOwl_toSElt_hack sowl

  pRenderHandler tah' phi@PotatoHandlerInput {..} = r where
    tah = updateBoxTextHandlerState False _potatoHandlerInput_canvasSelection tah'
    btis = _boxTextHandler_state tah
    offset = getBoxTextOffset $ snd $ getSBox _potatoHandlerInput_canvasSelection
    r = pRenderHandler (_boxTextHandler_prevHandler tah) phi <> makeTextHandlerRenderOutput btis offset

  pIsHandlerActive = _boxTextHandler_isActive




-- BOX LABEL STUFF STARTS HERE
data BoxLabelHandler = BoxLabelHandler {
    _boxLabelHandler_active      :: Bool
    -- NOTE some fields in here are ignored or interpreted differently from BoxTextHandler
    , _boxLabelHandler_state       :: TextInputState
    , _boxLabelHandler_prevHandler :: SomePotatoHandler
    , _boxLabelHandler_undoFirst   :: Bool
  }

lBox_to_boxLabelBox :: LBox -> LBox
lBox_to_boxLabelBox lbx = r where
  CanonicalLBox _ _ (LBox (V2 x y) (V2 w _)) = canonicalLBox_from_lBox lbx
  width = max 0 (w - 2)
  r = LBox (V2 (x+1) y) (V2 width 1)


updateBoxLabelInputStateWithSBox :: SBox -> TextInputState -> TextInputState
updateBoxLabelInputStateWithSBox sbox btis = r where
  alignment = convertTextAlignToTextZipperTextAlignment . _sBoxTitle_align . _sBox_title $ sbox
  newBox =  lBox_to_boxLabelBox $ _sBox_box sbox
  width = 99999 -- box label text always overflows
  r = btis {
      _textInputState_box = newBox
      , _textInputState_displayLines = TZ.displayLinesWithAlignment alignment width () () (_textInputState_zipper btis)
    }

makeBoxLabelInputState :: REltId -> SBox -> RelMouseDrag -> TextInputState
makeBoxLabelInputState rid sbox rmd = r where
  mogtext = _sBoxTitle_title . _sBox_title $ sbox
  ogtz = TZ.fromText (fromMaybe "" mogtext)
  r' = TextInputState {
      _textInputState_rid = rid
      , _textInputState_original   = mogtext
      , _textInputState_zipper   = ogtz

      -- these fields get updated in next pass
      , _textInputState_box = error "expected to be filled"
      , _textInputState_displayLines = error "expected to be filled"
    }
  r'' = updateBoxLabelInputStateWithSBox sbox r'
  r = mouseText r'' (_sBox_box sbox) rmd (V2 1 0)

makeBoxLabelHandler :: SomePotatoHandler -> CanvasSelection -> RelMouseDrag -> BoxLabelHandler
makeBoxLabelHandler prev selection rmd = BoxLabelHandler {
      _boxLabelHandler_active = False
      , _boxLabelHandler_state = uncurry makeBoxLabelInputState (getSBox selection) rmd
      , _boxLabelHandler_prevHandler = prev
      , _boxLabelHandler_undoFirst = False
    }


-- UNTESTED
updateBoxLabelHandlerState :: Bool -> CanvasSelection -> BoxLabelHandler -> BoxLabelHandler
updateBoxLabelHandlerState reset selection tah@BoxLabelHandler {..} = assert tzIsCorrect r where
  (_, sbox) = getSBox selection

  mNewText = _sBoxTitle_title . _sBox_title $ sbox

  recomputetz = TZ.fromText (fromMaybe "" mNewText)
  oldtz = _textInputState_zipper _boxLabelHandler_state
  -- NOTE that recomputetz won't have the same cursor position
  -- TODO delete this check, not very meaningful, but good for development purposes I guess
  tzIsCorrect = TZ.value oldtz == TZ.value recomputetz
  nextstate = updateBoxLabelInputStateWithSBox sbox _boxLabelHandler_state

  r = tah {
    _boxLabelHandler_state = if reset
      then nextstate {
          _textInputState_original = mNewText
        }
      else nextstate
    , _boxLabelHandler_undoFirst = if reset
      then False
      else _boxLabelHandler_undoFirst
  }


inputBoxLabel :: TextInputState -> Bool -> SuperOwl -> KeyboardKey -> (TextInputState, Maybe WSEvent)
inputBoxLabel tais undoFirst sowl kk = (newtais, mop) where
  (changed, newtais) = inputSingleLineZipper tais kk
  newtext = TZ.value (_textInputState_zipper newtais)
  controller = CTagBoxLabelText :=> (Identity $ CMaybeText (DeltaMaybeText (_textInputState_original tais, if newtext == "" then Nothing else Just newtext)))
  mop = if changed
    then Just $ WSEApplyLlama (undoFirst, makePFCLlama . OwlPFCManipulate $ IM.fromList [(_superOwl_id sowl,controller)])
    else Nothing


-- | just a helper for pHandleMouse
handleMouseDownOrFirstUpForBoxLabelHandler :: BoxLabelHandler -> PotatoHandlerInput -> RelMouseDrag -> SBox -> Bool -> Maybe PotatoHandlerOutput
handleMouseDownOrFirstUpForBoxLabelHandler tah@BoxLabelHandler {..} phi@PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) sbox isdown = r where
  clickInside = does_lBox_contains_XY (_textInputState_box _boxLabelHandler_state) _mouseDrag_to
  newState = mouseText _boxLabelHandler_state (_sBox_box sbox) rmd (V2 1 0)
  r = if clickInside
    then Just $ def {
        _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler tah {
            _boxLabelHandler_active = isdown
            , _boxLabelHandler_state = newState
          }
      }
    -- pass the input on to the base handler (so that you can interact with BoxHandler mouse manipulators too)
    else pHandleMouse _boxLabelHandler_prevHandler phi rmd


instance PotatoHandler BoxLabelHandler where
  pHandlerName _ = handlerName_boxLabel
  pHandlerDebugShow BoxLabelHandler {..} = LT.toStrict $ Pretty.pShowNoColor _boxLabelHandler_state

  -- UNTESTED
  pHandleMouse tah' phi@PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) = let
      -- TODO we need a different updated function here that does just the label
      tah@BoxLabelHandler {..} = updateBoxLabelHandlerState False _potatoHandlerInput_canvasSelection tah'
      (_, sbox) = getSBox _potatoHandlerInput_canvasSelection
    in case _mouseDrag_state of


      MouseDragState_Down -> handleMouseDownOrFirstUpForBoxLabelHandler tah phi rmd sbox True

      -- TODO drag select text someday
      MouseDragState_Dragging -> Just $ captureWithNoChange tah

      MouseDragState_Up -> if not _boxLabelHandler_active
        then handleMouseDownOrFirstUpForBoxLabelHandler tah phi rmd sbox False
        else Just $ def {
            _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler tah {
                _boxLabelHandler_active = False
              }
          }

      MouseDragState_Cancelled -> Just $ captureWithNoChange tah

  pHandleKeyboard tah' PotatoHandlerInput {..} (KeyboardData k _) = case k of
    KeyboardKey_Esc -> Just $ def { _potatoHandlerOutput_nextHandler = Just (_boxLabelHandler_prevHandler tah') }

    _ -> Just r where
      -- this regenerates displayLines unecessarily but who cares
      tah@BoxLabelHandler {..} = updateBoxLabelHandlerState False _potatoHandlerInput_canvasSelection tah'
      sowl = selectionToSuperOwl _potatoHandlerInput_canvasSelection

      -- TODO decide what to do with mods

      -- TODO inputBoxText is wrong, you need a label specific version
      (nexttais, mev) = inputBoxLabel _boxLabelHandler_state _boxLabelHandler_undoFirst sowl k
      r = def {
          _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler tah {
              _boxLabelHandler_state  = nexttais
              , _boxLabelHandler_undoFirst = case mev of
                Nothing -> _boxLabelHandler_undoFirst
                --Nothing -> False -- this variant adds new undo point each time cursoer is moved
                Just _  -> True
            }
          , _potatoHandlerOutput_pFEvent = mev
        }

  -- UNTESTED
  -- TODO do you need to reset _boxLabelHandler_prevHandler as well?
  pRefreshHandler tah PotatoHandlerInput {..} = if Seq.null (unCanvasSelection _potatoHandlerInput_canvasSelection)
    then Nothing -- selection was deleted or something
    else if rid /= (_textInputState_rid $ _boxLabelHandler_state tah)
      then Nothing -- selection was change to something else
      else case selt of
        SEltBox sbox -> if sBoxType_hasBorder (_sBox_boxType sbox)
          then Just $ SomePotatoHandler $ updateBoxLabelHandlerState True _potatoHandlerInput_canvasSelection tah
          -- SEltBox type changed to non-text
          else Nothing
        _ -> Nothing
      where
        sowl = selectionToSuperOwl _potatoHandlerInput_canvasSelection
        rid = _superOwl_id sowl
        selt = superOwl_toSElt_hack sowl

  pRenderHandler tah' phi@PotatoHandlerInput {..} = r where
    tah = updateBoxLabelHandlerState False _potatoHandlerInput_canvasSelection tah'
    btis = _boxLabelHandler_state tah
    offset = V2 0 0  -- TODO figure out why this isn't V2 1 0 😱
    r = pRenderHandler (_boxLabelHandler_prevHandler tah) phi <> makeTextHandlerRenderOutput btis offset

  pIsHandlerActive = _boxLabelHandler_active
