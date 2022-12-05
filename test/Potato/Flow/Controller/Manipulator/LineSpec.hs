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


basic_line_test :: Test
basic_line_test = assertGoatTesterWithOwlPFState emptyOwlPFState basic_line_test_m where
  basic_line_test_m = do
    verifyOwlCount 0

    setMarker "draw a line"
    setTool Tool_Line
    canvasMouseDown (0, 0)
    canvasMouseDown (100, 0)
    verifyOwlCount 1
    canvasMouseUp (100, 0)
    verifyOwlCount 1
    -- TODO verify line is selected

    setMarker "add a midpoint"
    canvasMouseDown (50, 0)
    canvasMouseDown (50, 50)
    canvasMouseUp (50, 50)

    verifyMostRecentlyCreatedLine $ \sline -> toMaybe (L.length (_sAutoLine_midpoints sline) /= 1) ("expected 1 midpoint, got: " <> show (_sAutoLine_midpoints sline))


spec :: Spec
spec = do
  describe "Line" $ do
    fromHUnitTest $ basic_line_test
