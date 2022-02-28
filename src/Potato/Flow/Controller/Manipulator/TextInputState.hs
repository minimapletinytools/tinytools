{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Manipulator.TextInputState where

import Relude

import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.Controller.Input

import qualified Potato.Data.Text.Zipper                          as TZ


data TextInputState = TextInputState {
  _textInputState_rid            :: REltId
  , _textInputState_original     :: Maybe Text -- needed to properly create DeltaText for undo
  , _textInputState_box          :: LBox -- we can always pull this from selection, but may as well store it
  , _textInputState_zipper       :: TZ.TextZipper
  , _textInputState_displayLines :: TZ.DisplayLines ()
  --, _textInputState_selected :: Int -- WIP
} deriving (Show)

-- TODO define behavior for when you click outside box or assert
mouseText :: TextInputState -> LBox -> RelMouseDrag -> XY -> TextInputState
mouseText tais lbox rmd (V2 xoffset yoffset)= r where
  RelMouseDrag MouseDrag {..} = rmd
  ogtz = _textInputState_zipper tais
  CanonicalLBox _ _ (LBox (V2 x y) (V2 _ _)) = canonicalLBox_from_lBox lbox
  V2 mousex mousey = _mouseDrag_to
  newtz = TZ.goToDisplayLinePosition (mousex-x-xoffset) (mousey-y-yoffset) (_textInputState_displayLines tais) ogtz
  r = tais { _textInputState_zipper = newtz }
