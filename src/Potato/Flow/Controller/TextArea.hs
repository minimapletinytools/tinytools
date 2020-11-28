{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.TextArea (
  TextAreaInputState(..)

  -- exposed for testing
  , makeTextAreaInputState
  , mouseText
) where

import           Relude

import           Potato.Flow.Controller.Input
import           Potato.Flow.Entry
import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.Types

import           Data.Dependent.Sum           (DSum ((:=>)))
import qualified Data.IntMap                  as IM
-- TODO prob switch to text-zipper one
import qualified Data.Text.Zipper             as TZ
import           Data.Tuple.Extra


data TextAreaInputState = TextAreaInputState {
  _textAreaInputState_original   :: Text
  , _textAreaInputState_zipper   :: TZ.TextZipper
  , _textAreaInputState_selected :: Int -- WIP
} deriving (Show)

makeTextAreaInputState :: SText -> RelMouseDrag -> TextAreaInputState
makeTextAreaInputState stext rmd = r where
  ogtz = TZ.fromText (_sText_text stext)
  r' = TextAreaInputState {
      _textAreaInputState_original   = _sText_text stext
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

-- TODO super shift selecting text someday meh
inputText :: TextAreaInputState -> Bool -> SuperSEltLabel -> KeyboardKey -> (TextAreaInputState, Maybe PFEventTag)
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
      _cText_deltaBox = DeltaLBox 0 0
      , _cText_deltaText = (_textAreaInputState_original tais, TZ.value newZip)
    })
  mop = if changed
    then Just $ PFEManipulate (undoFirst, IM.fromList [(fst3 selected,controller)])
    else Nothing
