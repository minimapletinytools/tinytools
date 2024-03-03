{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Manipulator.TextInputState where

import Relude

import           Potato.Flow.Math
import           Potato.Flow.Serialization.Snake
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.Handler

import qualified Data.Text as T
import qualified Potato.Data.Text.Zipper                          as TZ
import qualified Data.Map as Map


data TextInputState = TextInputState {
  _textInputState_rid            :: REltId
  , _textInputState_original     :: Maybe Text -- needed to properly create DeltaText for undo
  , _textInputState_box          :: LBox -- we can always pull this from selection, but may as well store it
  , _textInputState_zipper       :: TZ.TextZipper
  , _textInputState_displayLines :: TZ.DisplayLines ()
  --, _textInputState_selected :: Int -- WIP
} deriving (Show)


moveToEol :: TextInputState -> TextInputState
moveToEol tais = tais { _textInputState_zipper = TZ.end (_textInputState_zipper tais) }

-- TODO support shift selecting someday
-- TODO define behavior for when you click outside box or assert
mouseText :: TextInputState -> RelMouseDrag -> TextInputState
mouseText tais rmd = r where
  lbox = _textInputState_box tais
  RelMouseDrag MouseDrag {..} = rmd
  ogtz = _textInputState_zipper tais
  CanonicalLBox _ _ (LBox (V2 x y) (V2 _ _)) = canonicalLBox_from_lBox lbox
  V2 mousex mousey = _mouseDrag_to
  newtz = TZ.goToDisplayLinePosition (mousex-x) (mousey-y) (_textInputState_displayLines tais) ogtz
  r = tais { _textInputState_zipper = newtz }



-- TODO support shift selecting text someday meh
-- | returns zipper in TextInputState after keyboard input has been applied for single line entry (does not allow line breaks)
-- Bool indicates if there was any real input
inputSingleLineZipper :: TextInputState -> KeyboardKey -> (Bool, TextInputState)
inputSingleLineZipper tais kk = (changed, tais { _textInputState_zipper = newZip }) where

  oldZip = _textInputState_zipper tais
  (changed, newZip) = case kk of
    KeyboardKey_Left    -> (False, TZ.left oldZip)
    KeyboardKey_Right   -> (False, TZ.right oldZip)
    KeyboardKey_Home    -> (False, TZ.home oldZip)
    KeyboardKey_End -> (False, TZ.end oldZip)

    KeyboardKey_Space   -> (True, TZ.insertChar ' ' oldZip)
    KeyboardKey_Delete  -> (newtz /= oldZip, TZ.deleteRight oldZip) where newtz = TZ.deleteRight oldZip
    KeyboardKey_Backspace -> (newtz /= oldZip, newtz) where newtz = TZ.deleteLeft oldZip
    KeyboardKey_Char c  -> (True, TZ.insertChar c oldZip)

    -- TODO remove new line characters
    KeyboardKey_Paste t -> (True, TZ.insert t oldZip)

    _ -> (False, oldZip)


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



makeTextHandlerRenderOutput :: TextInputState -> HandlerRenderOutput
makeTextHandlerRenderOutput btis = r where
  dls = _textInputState_displayLines btis
  (x, y) = TZ._displayLines_cursorPos dls
  offsetMap = TZ._displayLines_offsetMap dls

  mCursorChar = (fmap fst) . T.uncons . TZ._textZipper_after . _textInputState_zipper $ btis

  mlbox = do
    -- empty boxes are used with line labels
    --guard $ lBox_area origBox > 0

    -- TODO would be nice to assert that this exists...
    (alignxoff,_) <- Map.lookup y offsetMap
    let
      LBox p _ = _textInputState_box $ btis
      cursorh = RenderHandle {
          _renderHandle_box = LBox (p + (V2 x y)) (V2 1 1)
          , _renderHandle_char =  case mCursorChar of
            Nothing -> Just ' '
            jc -> jc
          , _renderHandle_color = RHC_Default
        }
    return [cursorh]

  r = HandlerRenderOutput $ fromMaybe [] mlbox






---- GENERIC TEXT HANDLER STARTS HERE



data TextImpl o = TextImpl {
  _textImpl_mustGetOwlItem :: CanvasSelection -> o
  --, _textImpl_updateTextInputStateWithOwlItem :: o -> TextInputState -> TextInputState
  , _textImpl_owlItemText :: o -> Text
  , _textImpl_owlItemBox :: o -> CanonicalLBox
  , _textImpl_owlItemAlignment :: o -> _textStyle_alignment

  , _textImpl_makeTextInputState :: REltId -> o -> RelMouseDrag -> TextInputState
  , _textImpl_inputOwlItemZipper :: TextInputState -> KeyboardKey -> (Bool, TextInputState)


  -- TODO replace this with something higher level so that you can maybe use this for AutoLineLabel as well
  , _textImpl_makeController :: Text -> Text -> Controller
  -- TODO need something to make mouse up action generic
}


data OwlItemHandler o = OwlItemHandler {
    -- TODO Delete this
    _owlItemHandler_isActive      :: Bool
    
    , _owlItemHandler_state       :: TextInputState
    -- TODO you can prob delete this now, we don't persist state between sub handlers in this case
    , _owlItemHandler_prevHandler :: SomePotatoHandler
    , _owlItemHandler_undoFirst   :: Bool

    , _owlItemHandler_commitOnMouseUp :: Bool

    , _owlItemHandler_impl :: TextImpl o
  }

makeOwlItemHandler :: TextImpl o -> Bool -> SomePotatoHandler -> CanvasSelection -> RelMouseDrag -> OwlItemHandler o
makeOwlItemHandler impl commit prev selection rmd = OwlItemHandler {
    _owlItemHandler_isActive = False
    , _owlItemHandler_state = uncurry makeTextInputState (_textImpl_mustGetOwlItem impl $ selection) rmd
    , _owlItemHandler_prevHandler = prev
    , _owlItemHandler_undoFirst = False
    , _owlItemHandler_commitOnMouseUp = commit
    , _owlItemHandler_impl = impl
  }


updateTextInputStateWithOwlItem :: TextImpl o -> o -> TextInputState -> TextInputState
updateTextInputStateWithOwlItem impl oitem btis = r where
  alignment = convertTextAlignToTextZipperTextAlignment . (_textImpl_owlItemAlignment impl) $ oitem
  CanonicalLBox _ _ newBox@(LBox _ (V2 width _)) = _textImpl_owlItemBox impl $ oitem
  r = btis {
      _textInputState_box = newBox
      , _textInputState_displayLines = TZ.displayLinesWithAlignment alignment width () () (_textInputState_zipper btis)
    }

-- TODO I think you need to pad empty lines in the zipper to fill out the box D:
-- ok, no you don't, that's only for the non-paragraph text area that we don't actually have yet
makeTextInputState :: TextImpl o -> REltId -> o -> RelMouseDrag -> TextInputState
makeTextInputState impl rid oitem rmd = r where
  ogtext = _textImpl_owlItemText impl $ oitem
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
  r'' = updateTextInputStateWithOwlItem impl oitem r'
  r = mouseText r'' rmd


inputOwlItem :: TextImpl o -> TextInputState -> SuperOwl -> KeyboardKey -> (TextInputState, Maybe Llama)
inputOwlItem impl tais sowl kk = (newtais, mop) where
  (changed, newtais) = (_textImpl_inputOwlItemZipper impl) tais kk
  origt = _textInputState_original tais
  newt = TZ.value (_textInputState_zipper newtais)
  controller = _textImpl_makeController origt newt
  mop = if changed
    then Just $ makePFCLlama . OwlPFCManipulate $ IM.fromList [(_superOwl_id sowl,controller)]
    else Nothing


updateOwlItemHandlerState :: TextImpl o -> Bool -> CanvasSelection -> OwlItemHandler -> OwlItemHandler
updateOwlItemHandlerState impl reset selection tah@OwlItemHandler {..} = assert tzIsCorrect r where
  (_, oitem) = _textImpl_mustGetOwlItem impl $ selection

  newText = _textImpl_owlItemText oitem

  recomputetz = TZ.fromText newText
  oldtz = _textInputState_zipper _owlItemHandler_state
  -- NOTE that recomputetz won't have the same cursor position
  -- TODO delete this check, not very meaningful, but good for development purposes I guess
  tzIsCorrect = TZ.value oldtz == TZ.value recomputetz

  nextstate = updateTextInputStateWithOwlItem impl oitem _owlItemHandler_state

  r = tah {
    _owlItemHandler_state = if reset
      then nextstate {
          _textInputState_original = Just newText
        }
      else nextstate
    , _owlItemHandler_undoFirst = if reset
      then False
      else _owlItemHandler_undoFirst
  }

instance PotatoHandler OwlItemHandler where
  pHandlerName _ = handlerName_boxText
  pHandlerDebugShow OwlItemHandler {..} = LT.toStrict $ Pretty.pShowNoColor _owlItemHandler_state
  pHandleMouse tah' phi@PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) = let
      (rid, sbox) = (_textImpl_mustGetOwlItem _owlItemHandler_impl) _potatoHandlerInput_canvasSelection
      tah@OwlItemHandler {..} = updateOwlItemHandlerState False _potatoHandlerInput_canvasSelection tah'
    in case _mouseDrag_state of
      MouseDragState_Down -> r where
        clickInside = does_lBox_contains_XY (_textInputState_box _owlItemHandler_state) _mouseDrag_to
        newState = mouseText _owlItemHandler_state rmd
        r = if clickInside
          then Just $ def {
              _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler tah {
                  _owlItemHandler_isActive = True
                  , _owlItemHandler_state = newState
                }
            }
          -- pass the input on to the base handler (so that you can interact with BoxHandler mouse manipulators too)
          else pHandleMouse _owlItemHandler_prevHandler phi rmd

      -- TODO drag select text someday
      MouseDragState_Dragging -> Just $ captureWithNoChange tah

      MouseDragState_Up -> r where

        -- TODO YOU STOPPED HERE NEE DTO FIGURE OUT HOW TO MAKE THIS GENERIC
        -- I think one way to do it is to have handlers pass the call to the generic handler and then for mouse input the generic hnadler will return nothing and then the parent handler can do its specific stuff
        -- but maybe better to not make things so generic. You should still do the impl trick, but maybe a the generic Handler instance is not necessary
        -- you just need to update the undoFirst parameter outside of updateOwlItemHandlerState or make some awkward generic version of it

        -- if box is not text box, convert to text box and set undoFirst to True
        oldbt = _sBox_boxType $ sbox
        istext = sBoxType_isText oldbt
        newbt = make_sBoxType (sBoxType_hasBorder oldbt) True

        -- if it's not a text box, convert it to one (remember that this gets called from pHandleMouse with MouseDragState_Up in BoxHandler)
        r = if not istext
          then Just $ def {
              _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler tah {
                  _owlItemHandler_isActive = False    
                }
              -- TODO change this to just PO_Start
              -- the issue here is when you undo, it becomes not a text box, so you need to make sure to convert it to a text box in the preview operation (actually to do that, there's no point in converting it here really)
              -- NOTE if we PO_Start/_owlItemHandler_undoFirst = True we will undo the conversion to text box :(. It's fine, just permanently convert it to a text box, NBD
              -- also NOTE that this will not undo the text box conversion if you cancel this handler, it will just permanently be a text box now.
              -- NOTE this creates a weird undo operation that just converts from text to not text which is weird
              , _potatoHandlerOutput_action = HOA_Preview $ Preview PO_StartAndCommit $ makePFCLlama . OwlPFCManipulate $ IM.fromList [(rid, CTagBoxType :=> Identity (CBoxType (oldbt, newbt)))]

            }
          else Just $ def {
              _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler tah {
                  _owlItemHandler_isActive = False
                  , _owlItemHandler_commitOnMouseUp = False
                }
              , _potatoHandlerOutput_action = if _owlItemHandler_commitOnMouseUp then HOA_Preview Preview_Commit else HOA_Nothing
            }
      MouseDragState_Cancelled -> Just $ captureWithNoChange tah

  pHandleKeyboard tah' PotatoHandlerInput {..} (KeyboardData k _) = case k of
    KeyboardKey_Esc -> Just $ def { _potatoHandlerOutput_nextHandler = Just (_owlItemHandler_prevHandler tah') }
    -- TODO should only capture stuff caught by inputBoxTextZipper

    _ -> Just r where
      -- this regenerates displayLines unecessarily but who cares
      tah@OwlItemHandler {..} = updateOwlItemHandlerState False _potatoHandlerInput_canvasSelection tah'
      sowl = selectionToSuperOwl _potatoHandlerInput_canvasSelection

      -- TODO decide what to do with mods

      (nexttais, mllama) = inputBoxText _owlItemHandler_state sowl k
      r = def {
          _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler tah {
              _owlItemHandler_state  = nexttais
              , _owlItemHandler_undoFirst = case mllama of
                Nothing -> _owlItemHandler_undoFirst
                --Nothing -> False -- this variant adds new undo point each time cursoer is moved
                Just _  -> True
            }
          -- TODO do a Preview_Cancel if we reverted back to original text
          -- TODO we want to PO_Continue here, but we don't have a good place to commit right now as there's no explicit cancel for us to Preview_Commit
          , _potatoHandlerOutput_action = maybe HOA_Nothing (HOA_Preview . Preview (previewOperation_fromUndoFirst _owlItemHandler_undoFirst)) mllama

        }

  -- TODO do you need to reset _owlItemHandler_prevHandler as well?
  pRefreshHandler tah PotatoHandlerInput {..} = if Seq.null (unCanvasSelection _potatoHandlerInput_canvasSelection)
    then Nothing -- selection was deleted or something
    else if rid /= (_textInputState_rid $ _owlItemHandler_state tah)
      then Nothing -- selection was change to something else
      else case selt of
        SEltBox sbox -> if not $ sBoxType_isText (_sBox_boxType sbox)
          then Nothing -- SEltBox type changed to non-text
          -- TODO this needs to merge the TextZipper if change came due to remote event
          else Just $ SomePotatoHandler $ updateOwlItemHandlerState True _potatoHandlerInput_canvasSelection tah
        _ -> Nothing
      where
        sowl = selectionToSuperOwl _potatoHandlerInput_canvasSelection
        rid = _superOwl_id sowl
        selt = superOwl_toSElt_hack sowl

  pRenderHandler tah' phi@PotatoHandlerInput {..} = r where
    tah = updateOwlItemHandlerState False _potatoHandlerInput_canvasSelection tah'
    btis = _owlItemHandler_state tah
    r = pRenderHandler (_owlItemHandler_prevHandler tah) phi <> makeTextHandlerRenderOutput btis

  -- TODO set properly (_owlItemHandler_isActive checks mouse activity, but we have more subtle notions of active now)
  pIsHandlerActive tah = if _owlItemHandler_isActive tah then HAS_Active_Mouse else HAS_Active_Keyboard







