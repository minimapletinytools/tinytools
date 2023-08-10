{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-record-wildcards #-}


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

) where

import           Relude

import Potato.Flow.Controller.Manipulator.TextInputState
import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.Manipulator.Common
import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.Types
import Potato.Flow.Owl
import Potato.Flow.OwlWorkspace
import Potato.Flow.Llama
import           Potato.Flow.Preview

import           Control.Exception
import           Data.Default
import           Data.Dependent.Sum                        (DSum ((:=>)))
import qualified Data.IntMap                               as IM
import qualified Data.Sequence                             as Seq
import qualified Potato.Data.Text.Zipper                          as TZ
import qualified Text.Pretty.Simple as Pretty
import qualified Data.Text.Lazy as LT

getSBox :: CanvasSelection -> (REltId, SBox)
getSBox selection = case superOwl_toSElt_hack sowl of
  SEltBox sbox -> (rid, sbox)
  selt -> error $ "expected SBox, got " <> show selt
  where
    sowl = selectionToSuperOwl selection
    rid = _superOwl_id sowl

-- | shrink an LBox uniformly in each direction, but don't allow it to become negative
shrink_lBox_no_negative :: LBox -> Int -> Int -> LBox
shrink_lBox_no_negative (LBox (V2 x y) (V2 w h)) dw dh = LBox (V2 nx ny) (V2 nw nh) where
  (nx, nw) = if w <= 2*dw
    then if w <= dw
      -- prioritize shrinking from the right
      then (x, 0)
      else (x + (w - dw), 0)
    else (x+dw, w-2*dw)
  (ny, nh) = if h <= 2*dh
    then if h <= dh
      -- prioritize shrinking from the bottom
      then (y, 0)
      else (y + (h - dh), 0)
    else (y+dh, h-2*dh)


getSBoxTextBox :: SBox -> CanonicalLBox
getSBoxTextBox sbox = r where
  CanonicalLBox fx fy box' = canonicalLBox_from_lBox $ _sBox_box sbox
  r = CanonicalLBox fx fy $  if sBoxType_hasBorder (_sBox_boxType sbox)
    then shrink_lBox_no_negative box' 1 1
    else box'


updateTextInputStateWithSBox :: SBox -> TextInputState -> TextInputState
updateTextInputStateWithSBox sbox btis = r where
  alignment = convertTextAlignToTextZipperTextAlignment . _textStyle_alignment . _sBoxText_style . _sBox_text $ sbox
  CanonicalLBox _ _ newBox@(LBox _ (V2 width _)) = getSBoxTextBox sbox
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
  r = mouseText r'' rmd

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

inputBoxText :: TextInputState -> SuperOwl -> KeyboardKey -> (TextInputState, Maybe Llama)
inputBoxText tais sowl kk = (newtais, mop) where
  (changed, newtais) = inputBoxTextZipper tais kk
  controller = CTagBoxText :=> (Identity $ CBoxText {
      _cBoxText_deltaText = (fromMaybe "" (_textInputState_original tais), TZ.value (_textInputState_zipper newtais))
    })
  mop = if changed
    then Just $ makePFCLlama . OwlPFCManipulate $ IM.fromList [(_superOwl_id sowl,controller)]
    else Nothing

data BoxTextHandler = BoxTextHandler {
    -- TODO Delete this
    _boxTextHandler_isActive      :: Bool
    
    , _boxTextHandler_state       :: TextInputState
    -- TODO you can prob delete this now, we don't persist state between sub handlers in this case
    , _boxTextHandler_prevHandler :: SomePotatoHandler
    , _boxTextHandler_undoFirst   :: Bool

    , _boxTextHandler_commitOnMouseUp :: Bool
  }

makeBoxTextHandler :: Bool -> SomePotatoHandler -> CanvasSelection -> RelMouseDrag -> BoxTextHandler
makeBoxTextHandler commit prev selection rmd = BoxTextHandler {
      _boxTextHandler_isActive = False
      , _boxTextHandler_state = uncurry makeTextInputState (getSBox selection) rmd
      , _boxTextHandler_prevHandler = prev
      , _boxTextHandler_undoFirst = False
      , _boxTextHandler_commitOnMouseUp = commit
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
      (rid, sbox) = getSBox _potatoHandlerInput_canvasSelection
      tah@BoxTextHandler {..} = updateBoxTextHandlerState False _potatoHandlerInput_canvasSelection tah'
    in case _mouseDrag_state of
      MouseDragState_Down -> r where
        clickInside = does_lBox_contains_XY (_textInputState_box _boxTextHandler_state) _mouseDrag_to
        newState = mouseText _boxTextHandler_state rmd
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

      MouseDragState_Up -> r where

        -- if box is not text box, convert to text box and set undoFirst to True
        oldbt = _sBox_boxType $ sbox
        istext = sBoxType_isText oldbt
        newbt = make_sBoxType (sBoxType_hasBorder oldbt) True

        -- if it's not a text box, convert it to one (remember that this gets called from pHandleMouse with MouseDragState_Up in BoxHandler)
        r = if not istext
          then Just $ def {
              _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler tah {
                  _boxTextHandler_isActive = False    
                }
              -- TODO change this to just PO_Start
              -- the issue here is when you undo, it becomes not a text box, so you need to make sure to convert it to a text box in the preview operation (actually to do that, there's no point in converting it here really)
              -- NOTE if we PO_Start/_boxTextHandler_undoFirst = True we will undo the conversion to text box :(. It's fine, just permanently convert it to a text box, NBD
              -- also NOTE that this will not undo the text box conversion if you cancel this handler, it will just permanently be a text box now.
              -- NOTE this creates a weird undo operation that just converts from text to not text which is weird
              , _potatoHandlerOutput_action = HOA_Preview $ Preview PO_StartAndCommit $ makePFCLlama . OwlPFCManipulate $ IM.fromList [(rid, CTagBoxType :=> Identity (CBoxType (oldbt, newbt)))]

            }
          else Just $ def {
              _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler tah {
                  _boxTextHandler_isActive = False
                  , _boxTextHandler_commitOnMouseUp = False
                }
              , _potatoHandlerOutput_action = if _boxTextHandler_commitOnMouseUp then HOA_Preview Preview_Commit else HOA_Nothing
            }
      MouseDragState_Cancelled -> Just $ captureWithNoChange tah

  pHandleKeyboard tah' PotatoHandlerInput {..} (KeyboardData k _) = case k of
    KeyboardKey_Esc -> Just $ def { _potatoHandlerOutput_nextHandler = Just (_boxTextHandler_prevHandler tah') }
    -- TODO should only capture stuff caught by inputBoxTextZipper

    _ -> Just r where
      -- this regenerates displayLines unecessarily but who cares
      tah@BoxTextHandler {..} = updateBoxTextHandlerState False _potatoHandlerInput_canvasSelection tah'
      sowl = selectionToSuperOwl _potatoHandlerInput_canvasSelection

      -- TODO decide what to do with mods

      (nexttais, mllama) = inputBoxText _boxTextHandler_state sowl k
      r = def {
          _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler tah {
              _boxTextHandler_state  = nexttais
              , _boxTextHandler_undoFirst = case mllama of
                Nothing -> _boxTextHandler_undoFirst
                --Nothing -> False -- this variant adds new undo point each time cursoer is moved
                Just _  -> True
            }
          -- TODO do a Preview_Cancel if we reverted back to original text
          -- TODO we want to PO_Continue here, but we don't have a good place to commit right now as there's no explicit cancel for us to Preview_Commit
          , _potatoHandlerOutput_action = maybe HOA_Nothing (HOA_Preview . Preview (previewOperation_fromUndoFirst _boxTextHandler_undoFirst)) mllama

        }

  -- TODO do you need to reset _boxTextHandler_prevHandler as well?
  pRefreshHandler tah PotatoHandlerInput {..} = if Seq.null (unCanvasSelection _potatoHandlerInput_canvasSelection)
    then Nothing -- selection was deleted or something
    else if rid /= (_textInputState_rid $ _boxTextHandler_state tah)
      then Nothing -- selection was change to something else
      else case selt of
        SEltBox sbox -> if not $ sBoxType_isText (_sBox_boxType sbox)
          then Nothing -- SEltBox type changed to non-text
          -- TODO this needs to merge the TextZipper if change came due to remote event
          else Just $ SomePotatoHandler $ updateBoxTextHandlerState True _potatoHandlerInput_canvasSelection tah
        _ -> Nothing
      where
        sowl = selectionToSuperOwl _potatoHandlerInput_canvasSelection
        rid = _superOwl_id sowl
        selt = superOwl_toSElt_hack sowl

  pRenderHandler tah' phi@PotatoHandlerInput {..} = r where
    tah = updateBoxTextHandlerState False _potatoHandlerInput_canvasSelection tah'
    btis = _boxTextHandler_state tah
    r = pRenderHandler (_boxTextHandler_prevHandler tah) phi <> makeTextHandlerRenderOutput btis

  -- TODO set properly (_boxTextHandler_isActive checks mouse activity, but we have more subtle notions of active now)
  pIsHandlerActive tah = if _boxTextHandler_isActive tah then HAS_Active_Mouse else HAS_Active_Keyboard




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
  width = maxBound :: Int -- box label text always overflows
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
  r = mouseText r'' rmd

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


inputBoxLabel :: TextInputState -> Bool -> SuperOwl -> KeyboardKey -> (TextInputState, Maybe Llama)
inputBoxLabel tais undoFirst sowl kk = (newtais, mop) where
  (changed, newtais) = inputSingleLineZipper tais kk
  newtext = TZ.value (_textInputState_zipper newtais)
  controller = CTagBoxLabelText :=> (Identity $ CMaybeText (DeltaMaybeText (_textInputState_original tais, if newtext == "" then Nothing else Just newtext)))
  mop = if changed
    then Just $ makePFCLlama . OwlPFCManipulate $ IM.fromList [(_superOwl_id sowl,controller)]
    else Nothing


-- | just a helper for pHandleMouse
handleMouseDownOrFirstUpForBoxLabelHandler :: BoxLabelHandler -> PotatoHandlerInput -> RelMouseDrag -> Bool -> Maybe PotatoHandlerOutput
handleMouseDownOrFirstUpForBoxLabelHandler tah@BoxLabelHandler {..} phi@PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) isdown = r where
  clickInside = does_lBox_contains_XY (_textInputState_box _boxLabelHandler_state) _mouseDrag_to
  newState = mouseText _boxLabelHandler_state rmd
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
      tah@BoxLabelHandler {..} = updateBoxLabelHandlerState False _potatoHandlerInput_canvasSelection tah'
    in case _mouseDrag_state of


      MouseDragState_Down -> handleMouseDownOrFirstUpForBoxLabelHandler tah phi rmd True

      -- TODO drag select text someday
      MouseDragState_Dragging -> Just $ captureWithNoChange tah

      MouseDragState_Up -> if not _boxLabelHandler_active
        then handleMouseDownOrFirstUpForBoxLabelHandler tah phi rmd False
        else Just $ def {
            _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler tah {
                _boxLabelHandler_active = False
              }
          }

      MouseDragState_Cancelled -> Just $ captureWithNoChange tah

  pHandleKeyboard tah' PotatoHandlerInput {..} (KeyboardData k _) = case k of
    KeyboardKey_Esc -> Just $ def { _potatoHandlerOutput_nextHandler = Just (_boxLabelHandler_prevHandler tah') }
    -- TODO should only capture stuff caught by inputSingleLineZipper
    _ -> Just r where
      -- this regenerates displayLines unecessarily but who cares
      tah@BoxLabelHandler {..} = updateBoxLabelHandlerState False _potatoHandlerInput_canvasSelection tah'
      sowl = selectionToSuperOwl _potatoHandlerInput_canvasSelection

      -- TODO decide what to do with mods

      (nexttais, mllama) = inputBoxLabel _boxLabelHandler_state _boxLabelHandler_undoFirst sowl k
      r = def {
          _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler tah {
              _boxLabelHandler_state  = nexttais
              , _boxLabelHandler_undoFirst = case mllama of
                Nothing -> _boxLabelHandler_undoFirst
                --Nothing -> False -- this variant adds new undo point each time cursoer is moved
                Just _  -> True
            }
          , _potatoHandlerOutput_action = maybe HOA_Nothing (HOA_Preview . Preview (previewOperation_fromUndoFirst _boxLabelHandler_undoFirst)) mllama
        }

  -- UNTESTED
  -- TODO do you need to reset _boxLabelHandler_prevHandler as well?
  pRefreshHandler tah PotatoHandlerInput {..} = if Seq.null (unCanvasSelection _potatoHandlerInput_canvasSelection)
    then Nothing -- selection was deleted or something
    else if rid /= (_textInputState_rid $ _boxLabelHandler_state tah)
      then Nothing -- selection was change to something else
      else case selt of
        SEltBox sbox -> if sBoxType_hasBorder (_sBox_boxType sbox)
          -- TODO this needs to merge the TextZipper if change came due to remote event
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
    r = pRenderHandler (_boxLabelHandler_prevHandler tah) phi <> makeTextHandlerRenderOutput btis

  -- TODO set properly
  pIsHandlerActive tah = if _boxLabelHandler_active tah then HAS_Active_Mouse else HAS_Active_Keyboard
