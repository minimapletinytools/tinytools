{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.Manipulator.LayersSpec (
  spec
) where

import           Relude                                         hiding (empty,
                                                                 fromList)

import           Test.Hspec

import           Potato.Flow.GoatTester

import           Potato.Flow
import           Potato.Flow.Controller.Manipulator.TestHelpers

import qualified Data.List                                      as L
import qualified Data.Sequence as Seq


someFolderName :: Text
someFolderName = "testfolder"

verifyFolderSelected :: Text -> GoatTester ()
verifyFolderSelected name = verifySelectionIsAndOnlyIs ("selected " <> name) $
  \sowl -> if hasOwlItem_name sowl == name && hasOwlItem_isFolder sowl
    then Nothing
    else Just $ "expected folder named \"" <> name <> "\", got: " <> show sowl

verifyLayersCount :: Int -> GoatTester ()
verifyLayersCount n =  verifyState ("layers count is " <> show n) $ \s -> if (countentriesfn s) == n then Nothing else Just $ "expected " <> show n <> " elts, got " <> show (countentriesfn s)
  where countentriesfn = Seq.length . _layersState_entries . _goatState_layersState

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
  layerMouseDownUp (5,0)
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

create_in_folder_and_collapse_test :: Spec
create_in_folder_and_collapse_test = hSpecGoatTesterWithOwlPFState emptyOwlPFState $ do

  setMarker "create a folder"
  addFolder someFolderName
  verifyFolderSelected someFolderName
  verifyLayersCount 1

  folder <- mustGetMostRecentlyCreatedOwl

  setMarker "create a new element"
  drawCanvasBox (0, 0, 100, 100)
  verifyLayersCount 2

  setMarker "ensure it has the correct parent"
  verifyMostRecentlyCreatedOwl $ \sowl -> if _owlItemMeta_parent (_superOwl_meta sowl) == _superOwl_id folder then Nothing else Just $ "expected parent " <> show (_superOwl_id folder) <> " got " <> show sowl

  setMarker "collapse the folder"
  layerMouseDownUpRel LMO_Collapse 0 0
  verifyLayersCount 1

  setMarker "select the box and ensure the folders expand"
  canvasMouseDownUp (50, 50)
  verifyLayersCount 2


folder_collapse_test :: Spec
folder_collapse_test = hSpecGoatTesterWithOwlPFState emptyOwlPFState $ do
  setMarker "create folders"
  addFolder someFolderName
  verifyFolderSelected someFolderName
  verifyLayersCount 1
  addFolder "2"
  verifyFolderSelected "2"
  verifyLayersCount 2
  addFolder "3"
  verifyFolderSelected "3"
  verifyLayersCount 3
  addFolder "4"
  verifyFolderSelected "4"
  verifyLayersCount 4

  setMarker "collapse the second folder"
  layerMouseDownUpRel LMO_Collapse 1 1
  verifyLayersCount 2

  setMarker "collapse the first folder"
  layerMouseDownUpRel LMO_Collapse 0 0
  verifyLayersCount 1

  setMarker "expand the first folder"
  layerMouseDownUpRel LMO_Collapse 0 0
  verifyLayersCount 2

  setMarker "expand the second folder"
  layerMouseDownUpRel LMO_Collapse 1 1
  verifyLayersCount 4


lock_or_hide_select_test :: LayerMouseOp -> Spec
lock_or_hide_select_test lmo = hSpecGoatTesterWithOwlPFState emptyOwlPFState $ do
  setMarker "draw a box"
  drawCanvasBox (0,0,10,10)
  verifySelectionCount 1

  setMarker "lock or hide the box"
  layerMouseDownUpRel lmo 0 0
  verifySelectionCount 1

  setMarker "deselect"
  pressEscape
  verifySelectionCount 0

  setMarker "try and select the box via canvas"
  canvasMouseDown (5,5)
  canvasMouseUp (5,5)
  verifySelectionCount 0

  setMarker "select the box via layers"
  layerMouseDownUpRel LMO_Normal 0 0
  verifySelectionCount 1

  setMarker "deselect"
  pressEscape
  verifySelectionCount 0

  setMarker "unlock or unhide the box"
  layerMouseDownUpRel lmo 0 0

  setMarker "select the box via canvas"
  canvasMouseDownUp (5,5)
  verifySelectionCount 1


  

spec :: Spec
spec = do
  describe "Layers" $ do
    describe "basic" $ basic_test
    describe "rename_focus_test" $ rename_focus_test
    describe "create_in_folder_and_collapse_test" $ create_in_folder_and_collapse_test
    describe "folder_collapse_test" $ folder_collapse_test
    describe "hide_select_test" $ lock_or_hide_select_test LMO_Hide
    describe "lock_select_test" $ lock_or_hide_select_test LMO_Lock
