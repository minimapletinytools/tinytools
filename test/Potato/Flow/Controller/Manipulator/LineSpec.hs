{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.Manipulator.LineSpec (
  spec
) where

import           Relude                                     hiding (empty,
                                                             fromList)

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit                   (fromHUnitTest)
import           Test.HUnit

import Potato.Flow.GoatTester

import           Potato.Flow
import           Potato.Flow.Common

import qualified Data.List as L


expectMidpointCount :: Int -> GoatTester ()
expectMidpointCount n = verifyMostRecentlyCreatedLine $ \sline -> toMaybe (L.length (_sAutoLine_midpoints sline) /= n) ("expected " <> show n <> " midpoint, got: " <> show (_sAutoLine_midpoints sline))

initSimpleLine :: GoatTester ()
initSimpleLine = do
  verifyOwlCount 0

  setMarker "draw a line"
  setTool Tool_Line
  canvasMouseDown (0, 0)
  canvasMouseDown (100, 0)
  verifyOwlCount 1
  canvasMouseUp (100, 0)
  verifyOwlCount 1
  -- TODO verify line is selected

basic_test :: Test
basic_test = assertGoatTesterWithOwlPFState emptyOwlPFState $ do

  initSimpleLine

  setMarker "add a text label"
  canvasMouseDown (40, 0)
  canvasMouseUp (40, 0)
  pressKeys "meow meow meow meow"
  verifyMostRecentlyCreatedLine $ \sline -> toMaybe (L.length (_sAutoLine_labels sline) /= 1) ("expected 1 label, got: " <> show (_sAutoLine_labels sline))

  setMarker "add a midpoint"
  canvasMouseDown (50, 0)
  canvasMouseDown (50, 50)
  canvasMouseUp (50, 50)
  expectMidpointCount 1

basic_cancel_test :: Test
basic_cancel_test = assertGoatTesterWithOwlPFState emptyOwlPFState $ do
  
  setMarker "mouse down and cancel and ensure no line is created"
  setTool Tool_Line
  canvasMouseDown (0, 0)
  pressEscape
  canvasMouseDown (100, 0)
  canvasMouseUp (110, 0)
  verifyOwlCount 0

  setMarker "draw a line and cancel after moving it"
  setTool Tool_Line
  canvasMouseDown (0, 0)
  canvasMouseDown (100, 0)
  verifyOwlCount 1
  pressEscape
  verifyOwlCount 0
  canvasMouseDown (110, 0)
  canvasMouseUp (110, 0)
  verifyOwlCount 0

  initSimpleLine

  setMarker "create a midpoint and cancel"
  canvasMouseDown (50, 0)
  canvasMouseDown (50, 1)
  expectMidpointCount 1
  pressEscape
  expectMidpointCount 0
  canvasMouseDown (50, 2)
  canvasMouseUp (50, 2)
  expectMidpointCount 0

  setMarker "add a midpoint"
  canvasMouseDown (50, 0)
  canvasMouseDown (50, 50)
  canvasMouseUp (50, 50)
  expectMidpointCount 1



midpoint_modify_basic_test :: Test
midpoint_modify_basic_test = assertGoatTesterWithOwlPFState emptyOwlPFState $ do

  initSimpleLine

  setMarker "add a midpoint"
  canvasMouseDown (50, 0)
  canvasMouseDown (50, 1)
  canvasMouseUp (50, 1)
  expectMidpointCount 1

  setMarker "move it"
  canvasMouseDown (50, 1)
  canvasMouseDown (50, 10)
  canvasMouseUp (50, 10)
  -- TODO verify position

  setMarker "destroy it by dragging it over the start anchor"
  canvasMouseDown (50, 10)
  canvasMouseDown (0, 0)
  canvasMouseUp (0, 0)
  expectMidpointCount 0

  setMarker "undo its destruction"
  pressUndo
  expectMidpointCount 1

  setMarker "destroy it by dragging it over the end anchor"
  canvasMouseDown (50, 10)
  canvasMouseDown (100, 0)
  canvasMouseUp (100, 0)
  expectMidpointCount 0




midpoint_double_adjacent_delete_test :: Test
midpoint_double_adjacent_delete_test = assertGoatTesterWithOwlPFState emptyOwlPFState $ do

  initSimpleLine

  setMarker "add a midpoint"
  canvasMouseDown (50, 0)
  canvasMouseDown (50, 1)
  canvasMouseUp (50, 1)
  expectMidpointCount 1

  setMarker "add another midpoint right next to the first"
  canvasMouseDown (20, 0)
  canvasMouseDown (50, 2)
  canvasMouseUp (50, 2)
  expectMidpointCount 2

  setMarker "add a third midpoint"
  canvasMouseDown (90, 0)
  canvasMouseDown (50, 0)
  expectMidpointCount 3

  setMarker "delete the third midpoint by dragging it over the first"
  canvasMouseDown (50, 1)
  expectMidpointCount 2

  setMarker "you should be allowed to drag it over the second midpoint since it's not adjacent (this is the main edge case we are trying to test)"
  canvasMouseDown (50, 2)
  expectMidpointCount 3


spec :: Spec
spec = do
  describe "Line" $ do
    fromHUnitTest $ basic_test
    fromHUnitTest $ basic_cancel_test
    fromHUnitTest $ midpoint_modify_basic_test
    fromHUnitTest $ midpoint_double_adjacent_delete_test
