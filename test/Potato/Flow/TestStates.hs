{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Potato.Flow.TestStates (
  folderStart
  , folderEnd
  , someSEltLabel
  , someSCanvas
) where

import           Relude

import qualified Data.IntMap   as IM
import qualified Data.Sequence as Seq
import           Potato.Flow

-- TODO probably create TestStates.hs and put this stuff in there
folderStart :: SEltLabel
folderStart = SEltLabel "folder" SEltFolderStart

folderEnd :: SEltLabel
folderEnd = SEltLabel "folder (end)" SEltFolderEnd

someSEltLabel :: SEltLabel
someSEltLabel = SEltLabel "some elt" SEltNone

defaultCanvasLBox :: LBox
defaultCanvasLBox = LBox (V2 0 0) (V2 100 50)

someSCanvas :: SCanvas
someSCanvas = SCanvas  defaultCanvasLBox

someValidState1 :: PFState
someValidState1 = PFState {
      _pFState_layers = Seq.fromList [0..5]
      , _pFState_directory = IM.fromList [(0, folderStart), (1, folderEnd), (2, someSEltLabel), (3, someSEltLabel), (4, someSEltLabel), (5, someSEltLabel)]
      , _pFState_canvas = SCanvas defaultCanvasLBox
  }

someInvalidState1 :: PFState
someInvalidState1 = PFState {
      _pFState_layers = Seq.fromList [0..5]
      -- missing REltId 5
      , _pFState_directory = IM.fromList [(0, folderStart), (1, folderEnd), (2, someSEltLabel), (3, someSEltLabel), (4, someSEltLabel)]
      , _pFState_canvas = SCanvas defaultCanvasLBox
  }

someInvalidState2 :: PFState
someInvalidState2 = PFState {
      -- folder mismatch
      _pFState_layers = Seq.fromList [0..5]
      , _pFState_directory = IM.fromList [(0, folderStart), (1, folderStart), (2, someSEltLabel), (3, someSEltLabel), (4, someSEltLabel)]
      , _pFState_canvas = SCanvas defaultCanvasLBox
  }
