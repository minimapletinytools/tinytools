{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Manipulator.TextInputState where

import Relude

import           Potato.Flow.Math
import           Potato.Flow.Serialization.Snake
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.Handler
import Potato.Flow.Owl
import Potato.Flow.Llama
import           Potato.Flow.Serialization.Snake
import          Potato.Flow.Types


import qualified Data.IntMap as IM
import qualified Data.Text as T
import qualified Potato.Data.Text.Zipper                          as TZ
import qualified Data.Map as Map

import Control.Exception (assert)

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




data TextImpl o = TextImpl {
  _textImpl_mustGetOwlItem :: CanvasSelection -> (REltId, o)
  --, _textImpl_updateTextInputStateWithOwlItem :: o -> TextInputState -> TextInputState
  , _textImpl_owlItemText :: o -> Text
  , _textImpl_owlItemBox :: o -> CanonicalLBox
  , _textImpl_owlItemAlignment :: o -> TextAlign

  , _textImpl_inputOwlItemZipper :: TextInputState -> KeyboardKey -> (Bool, TextInputState)


  -- TODO replace this with something higher level so that you can maybe use this for AutoLineLabel as well
  , _textImpl_makeController :: Text -> Text -> Controller
  -- TODO need something to make mouse up action generic
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
makeOwlItemTextInputState :: TextImpl o -> REltId -> o -> RelMouseDrag -> TextInputState
makeOwlItemTextInputState impl rid oitem rmd = r where
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
  (changed, newtais) = _textImpl_inputOwlItemZipper impl tais kk
  origt = _textInputState_original tais
  newt = TZ.value (_textInputState_zipper newtais)
  controller = _textImpl_makeController impl (fromMaybe "" origt) newt
  mop = if changed
    then Just $ makePFCLlama . OwlPFCManipulate $ IM.fromList [(_superOwl_id sowl,controller)]
    else Nothing


-- TODO rename to updateOwlItemTextInputStateWithSelection
updateOwlItemTextInputState :: TextImpl o -> Bool -> CanvasSelection -> TextInputState -> TextInputState
updateOwlItemTextInputState impl reset selection tais = assert tzIsCorrect r where
  (_, oitem) = _textImpl_mustGetOwlItem impl $ selection

  newText = _textImpl_owlItemText impl oitem

  recomputetz = TZ.fromText newText
  oldtz = _textInputState_zipper tais
  -- NOTE that recomputetz won't have the same cursor position
  -- TODO delete this check, not very meaningful, but good for development purposes I guess
  tzIsCorrect = TZ.value oldtz == TZ.value recomputetz

  nextstate = updateTextInputStateWithOwlItem impl oitem tais

  r = if reset
    then nextstate {
        _textInputState_original = Just newText
      }
    else nextstate

