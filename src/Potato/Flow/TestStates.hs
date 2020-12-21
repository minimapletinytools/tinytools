{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Potato.Flow.TestStates (
  folderStart
  , folderEnd
  , someSEltLabel
  , someSCanvas
  , defaultCanvasLBox
  , pfstate_someValidState1
  , pfstate_someInvalidState1
  , pfstate_someInvalidState2
  , pfstate_basic1
) where

import           Relude

import           Data.Default
import qualified Data.IntMap   as IM
import qualified Data.Sequence as Seq
import           Potato.Flow

folderStart :: SEltLabel
folderStart = SEltLabel "folder" SEltFolderStart

folderEnd :: SEltLabel
folderEnd = SEltLabel "folder (end)" SEltFolderEnd

someSEltLabel :: SEltLabel
someSEltLabel = SEltLabel "some elt" SEltNone

defaultCanvasLBox :: LBox
defaultCanvasLBox = LBox (V2 0 0) (V2 50 25)

someSCanvas :: SCanvas
someSCanvas = SCanvas  defaultCanvasLBox

pfstate_someValidState1 :: PFState
pfstate_someValidState1 = PFState {
      _pFState_layers = Seq.fromList [0..5]
      , _pFState_directory = IM.fromList [(0, folderStart), (1, folderEnd), (2, someSEltLabel), (3, someSEltLabel), (4, someSEltLabel), (5, someSEltLabel)]
      , _pFState_canvas = SCanvas defaultCanvasLBox
  }

pfstate_someInvalidState1 :: PFState
pfstate_someInvalidState1 = PFState {
      _pFState_layers = Seq.fromList [0..5]
      -- missing REltId 5
      , _pFState_directory = IM.fromList [(0, folderStart), (1, folderEnd), (2, someSEltLabel), (3, someSEltLabel), (4, someSEltLabel)]
      , _pFState_canvas = SCanvas defaultCanvasLBox
  }

pfstate_someInvalidState2 :: PFState
pfstate_someInvalidState2 = PFState {
      -- folder mismatch
      _pFState_layers = Seq.fromList [0..5]
      , _pFState_directory = IM.fromList [(0, folderStart), (1, folderStart), (2, someSEltLabel), (3, someSEltLabel), (4, someSEltLabel)]
      , _pFState_canvas = SCanvas defaultCanvasLBox
  }

pfstate_basic1 :: PFState
pfstate_basic1 = PFState {
    _pFState_layers = Seq.fromList [0..5]
    , _pFState_directory = IM.fromList [
        -- 4 boxes in a grid
        (0, SEltLabel "b1" (SEltBox SBox {
            _sBox_box = LBox (V2 0 0) (V2 5 5)
            , _sBox_style = def
          }))
        , (1, SEltLabel "b2" (SEltBox SBox {
            _sBox_box = LBox (V2 10 10) (V2 5 5)
            , _sBox_style = def
          }))
        , (2, SEltLabel "b3" (SEltBox SBox {
            _sBox_box = LBox (V2 0 10) (V2 5 5)
            , _sBox_style = def
          }))
        , (3, SEltLabel "b4" (SEltBox SBox {
            _sBox_box = LBox (V2 10 0) (V2 5 5)
            , _sBox_style = def
          }))

        -- 2 lines sharing a start point at (0,100)
        , (4, SEltLabel "sl1" (SEltLine SLine {
            _sLine_start = V2 0 100
            , _sLine_end = V2 0 110
            , _sLine_style = def
          }))
        , (5, SEltLabel "sl2" (SEltLine SLine {
            _sLine_start = V2 0 100
            , _sLine_end = V2 10 100
            , _sLine_style = def
          }))

      ]
    , _pFState_canvas = SCanvas defaultCanvasLBox
  }
