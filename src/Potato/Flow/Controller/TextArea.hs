{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.TextArea (

) where

import           Relude

import           Potato.Flow.Entry
import           Potato.Flow.Math
import           Potato.Flow.Types

import           Data.Dependent.Sum (DSum ((:=>)))
import qualified Data.IntMap        as IM
import qualified Data.Text.Zipper   as TZ
import           Data.Tuple.Extra


data TextAreaInputState = TextAreaInputState {
  _textAreaInputState_original   :: Text
  , _textAreaInputState_zipper   :: TZ.TextZipper
  , _textAreaInputState_selected :: Int -- WIP
}

inputText :: TextAreaInputState -> Bool -> SuperSEltLabel -> Char -> PFEventTag
inputText TextAreaInputState {..} undoFirst selected c = r where
  controller = CTagText :=> (Identity $ CText {
      _cText_deltaBox = DeltaLBox 0 0
      , _cText_deltaText = (_textAreaInputState_original, TZ.value _textAreaInputState_zipper)
    })
  r = PFEManipulate (undoFirst, IM.fromList [(fst3 selected,controller)])
