{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.Manipulator.BoxSpec (
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

initSimpleBox :: GoatTester ()
initSimpleBox = do
  verifyOwlCount 0

  setMarker "draw a box"
  setTool Tool_Box
  canvasMouseDown (0, 0)
  canvasMouseDown (100, 100)
  verifyOwlCount 1
  canvasMouseUp (100, 100)
  verifyOwlCount 1
  -- TODO verify box is selected

basic_test :: Test
basic_test = assertGoatTesterWithOwlPFState emptyOwlPFState $ do

  initSimpleBox

  -- TODO 


basic_cancel_test :: Test
basic_cancel_test = assertGoatTesterWithOwlPFState emptyOwlPFState $ do
  
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
  describe "Line" $ do
    fromHUnitTest $ basic_test
    fromHUnitTest $ basic_cancel_test