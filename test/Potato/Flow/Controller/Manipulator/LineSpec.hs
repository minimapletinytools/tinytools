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
  verifyMostRecentlyCreatedLine $ \sline -> toMaybe (L.length (_sAutoLine_midpoints sline) /= 1) ("expected 1 midpoint, got: " <> show (_sAutoLine_midpoints sline))



midpoint_double_adjacent_delete_test :: Test
midpoint_double_adjacent_delete_test = assertGoatTesterWithOwlPFState emptyOwlPFState $ do

  initSimpleLine

  setMarker "add a midpoint"
  canvasMouseDown (50, 0)
  canvasMouseDown (50, 1)
  canvasMouseUp (50, 1)
  verifyMostRecentlyCreatedLine $ \sline -> toMaybe (L.length (_sAutoLine_midpoints sline) /= 1) ("expected 1 midpoint, got: " <> show (_sAutoLine_midpoints sline))

  setMarker "add another midpoint right next to the first"
  canvasMouseDown (20, 0)
  canvasMouseDown (50, 2)
  canvasMouseUp (50, 2)
  verifyMostRecentlyCreatedLine $ \sline -> toMaybe (L.length (_sAutoLine_midpoints sline) /= 2) ("expected 1 midpoint, got: " <> show (_sAutoLine_midpoints sline))

  setMarker "add a third midpoint"
  canvasMouseDown (90, 0)
  canvasMouseDown (50, 0)
  verifyMostRecentlyCreatedLine $ \sline -> toMaybe (L.length (_sAutoLine_midpoints sline) /= 3) ("expected 1 midpoint, got: " <> show (_sAutoLine_midpoints sline))

  setMarker "delete the third midpoint by dragging it over the first"
  canvasMouseDown (50, 1)
  verifyMostRecentlyCreatedLine $ \sline -> toMaybe (L.length (_sAutoLine_midpoints sline) /= 2) ("expected 1 midpoint, got: " <> show (_sAutoLine_midpoints sline))

  setMarker "you should be allowed to drag it over the second midpoint since it's not adjacent"
  canvasMouseDown (50, 2)
  verifyMostRecentlyCreatedLine $ \sline -> toMaybe (L.length (_sAutoLine_midpoints sline) /= 3) ("expected 1 midpoint, got: " <> show (_sAutoLine_midpoints sline))


spec :: Spec
spec = do
  describe "Line" $ do
    fromHUnitTest $ basic_test
    fromHUnitTest $ midpoint_double_adjacent_delete_test
