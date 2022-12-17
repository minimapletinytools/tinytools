{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Manipulator.TextInputState where

import Relude

import           Potato.Flow.Math
import           Potato.Flow.SElts
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

-- TODO support shift selecting someday
-- TODO define behavior for when you click outside box or assert
mouseText :: TextInputState -> LBox -> RelMouseDrag -> XY -> TextInputState
mouseText tais lbox rmd (V2 xoffset yoffset)= r where
  RelMouseDrag MouseDrag {..} = rmd
  ogtz = _textInputState_zipper tais
  CanonicalLBox _ _ (LBox (V2 x y) (V2 _ _)) = canonicalLBox_from_lBox lbox
  V2 mousex mousey = _mouseDrag_to
  newtz = TZ.goToDisplayLinePosition (mousex-x-xoffset) (mousey-y-yoffset) (_textInputState_displayLines tais) ogtz
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
    KeyboardKey_Delete  -> (True, TZ.deleteRight oldZip)
    KeyboardKey_Backspace -> (True, TZ.deleteLeft oldZip)
    KeyboardKey_Char c  -> (True, TZ.insertChar c oldZip)

    -- TODO remove new line characters
    KeyboardKey_Paste t -> (True, TZ.insert t oldZip)

    _ -> (False, oldZip)


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
