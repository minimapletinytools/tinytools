{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.Manipulator.LineSpec (
  spec
) where

import           Relude                                         hiding (empty,
                                                                 fromList)

import           Test.Hspec

import           Potato.Flow.GoatTester

import           Potato.Flow
import           Potato.Flow.Common
import           Potato.Flow.Controller.Manipulator.TestHelpers
import           Potato.Flow.DebugHelpers

import qualified Data.List                                      as L


verifyMostRecentlyCreatedLinesLatestLineLabelHasText :: Text -> GoatTester ()
verifyMostRecentlyCreatedLinesLatestLineLabelHasText text = verifyMostRecentlyCreatedLine  checkfn where
  checkfn sline = case _sAutoLine_labels sline of
    [] -> Just "most recently created line has no line labels"
    (x:_) -> if _sAutoLineLabel_text x == text
      then Nothing
      else Just $ "found line label with text: " <> _sAutoLineLabel_text x <> " expected: " <> text


blankOwlPFState :: OwlPFState
blankOwlPFState = OwlPFState emptyOwlTree (SCanvas (LBox 0 200))

expectMidpointCount :: Int -> GoatTester ()
expectMidpointCount n = verifyMostRecentlyCreatedLine $ \sline -> toMaybe (L.length (_sAutoLine_midpoints sline) /= n) ("expected " <> show n <> " midpoint, got: " <> show (_sAutoLine_midpoints sline))

initSimpleLine :: GoatTester ()
initSimpleLine = drawCanvasLine (0, 0) (100, 0)

basic_test :: Spec
basic_test = hSpecGoatTesterWithOwlPFState blankOwlPFState $ do

  initSimpleLine

  setMarker "add a text label"
  canvasMouseDown (40, 0)
  canvasMouseUp (40, 0)
  verifyMostRecentlyCreatedLine $ \sline -> toMaybe (L.length (_sAutoLine_labels sline) /= 1) ("expected 1 label, got: " <> show (_sAutoLine_labels sline))
  verifyMostRecentlyCreatedLinesLatestLineLabelHasText ""
  pressKeys "meow meow meow meow"
  verifyMostRecentlyCreatedLinesLatestLineLabelHasText "meow meow meow meow"


  setMarker "add a midpoint"
  canvasMouseDown (50, 0)
  canvasMouseDown (50, 50)
  canvasMouseUp (50, 50)
  expectMidpointCount 1

basic_cancel_test :: Spec
basic_cancel_test = hSpecGoatTesterWithOwlPFState blankOwlPFState $ do

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



midpoint_modify_basic_test :: Spec
midpoint_modify_basic_test = hSpecGoatTesterWithOwlPFState blankOwlPFState $ do

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




midpoint_double_adjacent_delete_test :: Spec
midpoint_double_adjacent_delete_test = hSpecGoatTesterWithOwlPFState blankOwlPFState $ do

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


initUnitBox :: (Int, Int) -> GoatTester ()
initUnitBox (x, y) = do
  setMarker "draw a 1x1 box"
  setTool Tool_Box
  canvasMouseDown (x, y)
  canvasMouseDown (x+1, y+1)
  canvasMouseUp (x+1, y+1)
  -- TODO verify box is selected

attaching_delete_test :: Spec
attaching_delete_test = hSpecGoatTesterWithOwlPFState blankOwlPFState $ do
  initUnitBox (0,0)
  verifyOwlCount 1

  setMarker "draw line attached to box"
  setTool Tool_Line
  canvasMouseDown (-1, 0)
  canvasMouseDown (-10, 1)
  canvasMouseUp (-10, 1)
  verifyOwlCount 2

  -- TODO verify line is attached to box

  setMarker "delete the box"
  canvasMouseDown (0, 0)
  canvasMouseUp (0, 0)
  pressDelete
  verifyOwlCount 1

  -- TODO verify the line that's left is in a sensible place

  pressUndo
  verifyOwlCount 2
  -- TODO verify again that the line is in the expected place

attaching_fully_attached_wont_move_test :: Spec
attaching_fully_attached_wont_move_test = hSpecGoatTesterWithOwlPFState blankOwlPFState $ do
  initUnitBox (0,0)
  initUnitBox (10,0)
  verifyOwlCount 2

  setMarker "draw line attached to box"
  setTool Tool_Line
  canvasMouseDown (1, 0)
  canvasMouseDown (9, 0)
  canvasMouseUp (9, 0)
  verifyOwlCount 3

  -- TODO verify line is attached to box
  -- TODO verify line is selected

  --
  setMarker "draw another line so that we can select both lines and move them together (otherwise you create a midpoint)"
  setTool Tool_Line
  canvasMouseDown (0, -1)
  canvasMouseDown (10, -1)
  canvasMouseUp (10, -1)
  verifyOwlCount 4

  s1 <- getOwlPFState

  setMarker "select both lines"
  canvasMouseDown (5, -5)
  canvasMouseDown (5, 5)
  canvasMouseUp (5, 5)
  verifySelectionCount 2

  setMarker "try and move the lines"
  canvasMouseDown (5, 0)
  canvasMouseDown (5, 10)
  canvasMouseUp (5, 10)

  s2 <- getOwlPFState
  verify "state did not change after attempting to move line" $ if s1 == s2 then Nothing else Just "it changed!!"

  setMarker "select everything"
  canvasMouseDown (-10, -10)
  canvasMouseDown (20, 20)
  canvasMouseUp (20, 20)
  verifySelectionCount 4

  setMarker "move everything"
  canvasMouseDown (5, 0)
  canvasMouseDown (5, 10)
  canvasMouseUp (5, 10)

  s3 <- getOwlPFState
  verify "state did not change after attempting to move line" $ if s2 == s3 then Just "it didn't change!" else Nothing


label_cursor_test :: Spec
label_cursor_test = hSpecGoatTesterWithOwlPFState blankOwlPFState $ do

  initSimpleLine
  verifySelectionCount 1

  setMarker "add a label"
  canvasMouseDown (50, 0)
  canvasMouseUp (50, 0)
  pressKey '1'
  pressKey '2'
  pressKey '3'
  pressKey '4'
  pressKey '5'
  verifyMostRecentlyCreatedLinesLatestLineLabelHasText "12345"

  setMarker "move the cursor to 3"
  canvasMouseDown (50, 0)
  canvasMouseUp (50, 0)
  pressKey 'A'
  verifyMostRecentlyCreatedLinesLatestLineLabelHasText "12A345"

  setMarker "attempt to move the cursor to beyond the line"
  canvasMouseDown (150, 0)
  canvasMouseUp (150, 0)
  verifySelectionCount 0



cache_basic_test :: Spec
cache_basic_test = hSpecGoatTesterWithOwlPFState blankOwlPFState $ do

  initSimpleLine

  let
    f sowl = case _owlItem_subItem (_superOwl_elt sowl) of
      OwlSubItemLine _ (Just c) -> Nothing
      _                         -> Just $ "expected cache, got " <> show sowl
  verifyMostRecentlyCreatedOwl' "verify cache got created for the line we just created" f



spec :: Spec
spec = do
  describe "Line" $ do
    describe "basic" $ basic_test
    describe "basic_cancel" $ basic_cancel_test
    describe "midpoint_modify_basic" $  midpoint_modify_basic_test
    describe "midpoint_double_adjacent_delete" $  midpoint_double_adjacent_delete_test
    describe "attaching_delete_test" $ attaching_delete_test
    describe "attaching_fully_attached_wont_move_test" $ attaching_fully_attached_wont_move_test
    describe "label_cursor_test" $ label_cursor_test

    -- TODO enable once you fix the "-- TODO DELETE THIS YOU SHOULDN'T HAVE TO DO THIS, this is breaking caching" comment in Goat.hs
    --describe "cache_basic" $ fromHUnitTest $ cache_basic_test
