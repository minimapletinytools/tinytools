{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.Manipulator.LayersSpec (
  spec
) where

import           Relude                                     hiding (empty,
                                                             fromList)

import           Test.Hspec

import Potato.Flow.GoatTester

import           Potato.Flow
import           Potato.Flow.Common

import qualified Data.List as L


basic_test :: Spec
basic_test = hSpecGoatTesterWithOwlPFState emptyOwlPFState $ do
  verifyOwlCount 0
  addFolder "testfolder"
  verifyOwlCount 1

  -- TODO
  return ()


rename_focus_test :: Spec
rename_focus_test = hSpecGoatTesterWithOwlPFState emptyOwlPFState $ do

  setMarker "draw a box"
  setTool Tool_Box
  canvasMouseDown (0, 0)
  canvasMouseDown (100, 100)
  canvasMouseUp (100, 100)
  verifyOwlCount 1

  setMarker "select the box via layers"
  -- TODO

  setMarker "begin renaming the box"
  -- TODO

  setMarker "change focus and ensure rename took effect"
  setFocusArea GoatFocusedArea_Other
  -- TODO verify

create_in_folder_test :: Spec
create_in_folder_test = hSpecGoatTesterWithOwlPFState emptyOwlPFState $ do

  -- TODO
  return ()


spec :: Spec
spec = do
  describe "Layers" $ do
    describe "basic" $ basic_test
    describe "rename_focus_test" $ rename_focus_test
    describe "create_in_folder_test" $ create_in_folder_test
