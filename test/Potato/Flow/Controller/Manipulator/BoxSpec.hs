{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.Manipulator.BoxSpec (
  spec
) where

import           Relude                                     hiding (empty,
                                                             fromList)

import           Test.Hspec

import Potato.Flow.GoatTester

import           Potato.Flow
import           Potato.Flow.Common
import Potato.Flow.Controller.Manipulator.TestHelpers

import qualified Data.List as L

initSimpleBox :: GoatTester ()
initSimpleBox = drawCanvasBox (0, 0, 100, 100)

basic_test :: Spec
basic_test = hSpecGoatTesterWithOwlPFState emptyOwlPFState $ do

  initSimpleBox

  -- TODO


basic_cancel_test :: Spec
basic_cancel_test = hSpecGoatTesterWithOwlPFState emptyOwlPFState $ do

  setMarker "mouse down and cancel and ensure no box is created"
  setTool Tool_Box
  canvasMouseDown (0, 0)
  pressEscape
  canvasMouseDown (100, 100)
  canvasMouseUp (100, 100)
  verifyOwlCount 0

  setMarker "draw a line and cancel after moving it"
  setTool Tool_Box
  canvasMouseDown (0, 0)
  canvasMouseDown (100, 100)
  verifyOwlCount 1
  pressEscape
  verifyOwlCount 0
  canvasMouseDown (110, 90)
  canvasMouseUp (110, 90)
  verifyOwlCount 0

  initSimpleBox


spec :: Spec
spec = do
  describe "Box" $ do
    describe "basic" $ basic_test
    describe "basic_cancel" $ basic_cancel_test
