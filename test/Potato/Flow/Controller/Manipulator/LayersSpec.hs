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
import Control.Monad


someFolderName :: Text
someFolderName = "testfolder"

verifyFolderSelected :: (Monad m) => Text -> GoatTesterT m ()
verifyFolderSelected name = verifySelectionIsAndOnlyIs ("selected " <> name) $
  \sowl -> if hasOwlItem_name sowl == name && hasOwlItem_isFolder sowl
    then Nothing
    else Just $ "expected folder named \"" <> name <> "\", got: " <> show sowl

basic_test :: Spec
basic_test = hSpecGoatTesterWithOwlPFState emptyOwlPFState $ do

  setMarker "create a folder"
  verifyOwlCount 0
  addFolder someFolderName
  verifyOwlCount 1
  verifyFolderSelected someFolderName

  setMarker "press escape to unselect the folder"
  pressEscape
  verifySelectionCount 0

  setMarker "select the folder"
  layerMouseDown (5,0)
  layerMouseUp (5,0)
  verifyFolderSelected someFolderName



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

  setMarker "create a folder"
  addFolder someFolderName
  verifyFolderSelected someFolderName

  setMarker "create a new element"
  -- TODO create element
  -- TODO verify it's in the right folder



  -- TODO
  return ()


spec :: Spec
spec = do
  describe "Layers" $ do
    describe "basic" $ basic_test
    describe "rename_focus_test" $ rename_focus_test
    describe "create_in_folder_test" $ create_in_folder_test
