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
import Control.Monad (forM_)


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

verifyNthEntryInLayersPropertyIs :: (Show a, Eq a) => Int -> (LayerEntry -> a) -> a -> GoatTester()
verifyNthEntryInLayersPropertyIs n f v = verifyState (show n <> " entry in layers has property " <> show v) $ vf where
  vf s = r where
    lentries = _layersState_entries . _goatState_layersState $ s
    mfirstentry = Seq.lookup n lentries
    r = case mfirstentry of
      Nothing -> Just $ "no " <> show n <> " entry in layers"
      Just lentry -> if firstentry == v then Nothing else Just $ "expected " <> show v <> ", got " <> show firstentry where
        firstentry = f lentry

verifyNthEntryInLayersIs :: Int -> REltId -> GoatTester()
verifyNthEntryInLayersIs n rid = verifyNthEntryInLayersPropertyIs n (_superOwl_id . _layerEntry_superOwl) rid

{--
verifyNthEntryInLayersIs :: Int -> REltId -> GoatTester()
verifyNthEntryInLayersIs n rid = verifyState (show n <> " entry in layers is " <> show rid) $ vf where
  vf s = r where
    lentries = _layersState_entries . _goatState_layersState $ s
    mfirstentry = Seq.lookup n lentries
    r = case mfirstentry of
      Nothing -> Just $ "no " <> show n <> " entry in layers"
      Just lentry -> if firstentry == rid then Nothing else Just $ "expected REltId " <> show rid <> ", got " <> show firstentry where
        firstentry = _superOwl_id . _layerEntry_superOwl $ lentry
--}

verifyFirstEntryInLayersIs :: REltId -> GoatTester()
verifyFirstEntryInLayersIs = verifyNthEntryInLayersIs 0
    

verifyNthEntryInLayersHasDepth :: Int -> Int -> GoatTester()
verifyNthEntryInLayersHasDepth n d = verifyNthEntryInLayersPropertyIs n layerEntry_depth d


create_select_test :: Spec
create_select_test = hSpecGoatTesterWithOwlPFState emptyOwlPFState $ do

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

shift_select_test :: Spec
shift_select_test = hSpecGoatTesterWithOwlPFState emptyOwlPFState $ do

  let xoff = 10

  setMarker "draw several boxes"
  forM_ [0..9] $ \_ -> drawCanvasBox (0, 0, 100, 100)
  verifyOwlCount 10
  pressEscape
  verifySelectionCount 0

  setMarker "shift select some stuff"
  forM_ [0..9] $ \n -> layerMouseDownUpWithModifier (xoff, n) KeyModifier_Shift
  verifySelectionCount 10
  
  setMarker "shift unselect stuff"
  layerMouseDownUpWithModifier (xoff, 5) KeyModifier_Shift
  verifySelectionCount 9
  layerMouseDownUpWithModifier (xoff, 8) KeyModifier_Shift
  verifySelectionCount 8
  
  setMarker "regular select one already selected element"
  layerMouseDownUp (xoff, 2)
  verifySelectionCount 1
  
  setMarker "deselect everything"
  pressEscape
  verifySelectionCount 0


rename_focus_clickCanvas_test :: Spec
rename_focus_clickCanvas_test = hSpecGoatTesterWithOwlPFState emptyOwlPFState $ do

  setMarker "draw a box"
  drawCanvasBox (0, 0, 100, 100)
  verifyOwlCount 1

  setMarker "select the box via layers"
  layerMouseDownUpRel LMO_Normal 0 0
  verifySelectionCount 1

  setMarker "begin renaming the box"
  layerMouseDownUpRel LMO_Normal 0 0
  pressKeys "aoeu"

  let
    origname = "<box>"
    -- NOTE, when when have multi-select, we should auto-select the previous name such that it gets deleted
    expectedname = "aoeu<box>"

  setMarker "verify the name has not been changed yet (no preview)"
  verifyMostRecentlyCreatedOwl $ \sowl -> if hasOwlItem_name sowl == origname then Nothing else Just $ "expected name " <> show origname <> " got " <> show (hasOwlItem_name sowl)

  setMarker "change focus by clicking on canvas and ensure rename took effect"
  canvasMouseDownUp (50, 50)
  verifyMostRecentlyCreatedOwl $ \sowl -> if hasOwlItem_name sowl == expectedname then Nothing else Just $ "expected name " <> show expectedname <> " got " <> show (hasOwlItem_name sowl)

-- same as rename_focus_clickCanvas_test but calls setFocus instead
rename_focus_setFocus_test :: GoatFocusedArea -> Spec
rename_focus_setFocus_test gfa = hSpecGoatTesterWithOwlPFState emptyOwlPFState $ do

  setMarker "draw a box"
  drawCanvasBox (0, 0, 100, 100)
  verifyOwlCount 1

  setMarker "select the box via layers"
  layerMouseDownUpRel LMO_Normal 0 0
  verifySelectionCount 1

  setMarker "begin renaming the box"
  layerMouseDownUpRel LMO_Normal 0 0  
  pressKeys "aoeu"
  
  let
    origname = "<box>"
    -- NOTE, when when have multi-select, we should auto-select the previous name such that it gets deleted
    expectedname = "aoeu<box>"

  setMarker "verify the name has not been changed yet (no preview)"
  verifyMostRecentlyCreatedOwl $ \sowl -> if hasOwlItem_name sowl == origname then Nothing else Just $ "expected name " <> show origname <> " got " <> show (hasOwlItem_name sowl)
  
  setMarker "change focus and ensure rename took effect"
  setFocusArea gfa
  verifyMostRecentlyCreatedOwl $ \sowl -> if hasOwlItem_name sowl == expectedname then Nothing else Just $ "expected name " <> show expectedname <> " got " <> show (hasOwlItem_name sowl)

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

drag_folder_test :: Spec
drag_folder_test = hSpecGoatTesterWithOwlPFState emptyOwlPFState $ do
  setMarker "create a folder"
  verifyOwlCount 0
  addFolder someFolderName
  verifyOwlCount 1
  verifyFolderSelected someFolderName
  folderrid <- _superOwl_id <$> mustGetMostRecentlyCreatedOwl 
  verifyFirstEntryInLayersIs folderrid

  setMarker "create box 1"
  drawCanvasBox (0,0,10,10)
  verifySelectionCount 1
  -- TODO verify the box is in the folder
  verifyFirstEntryInLayersIs folderrid

  setMarker "unselect the box so that the next box is not in the folder"
  pressEscape
  verifySelectionCount 0

  setMarker "create box 2"
  drawCanvasBox (10,10,10,10)
  verifySelectionCount 1
  boxrid <- _superOwl_id <$> mustGetMostRecentlyCreatedOwl 
  verifyFirstEntryInLayersIs boxrid

  beforeDragTheFolder <- getLayersState
  

  setMarker "select and drag the folder"
  layerMouseDownUpRel LMO_Normal 1 0
  layerMouseDownRel LMO_Normal 1 0
  layerMouseDownRel LMO_Normal 0 0
  layerMouseUpRel LMO_Normal 0 0
  -- TODO verify the folder is in the first position in layers
  verifyFirstEntryInLayersIs folderrid

  beforeUndo <- getLayersState

  setMarker "undo"
  pressUndo
  afterPressUndo <- getLayersState
  verifyEqual "undo state is same as before drag the folder" beforeDragTheFolder afterPressUndo

  setMarker "redo"
  pressRedo
  afterPressRedo <- getLayersState
  verifyEqual "undo state is same as before undo redo" beforeUndo afterPressRedo

drag_folder2_test :: Spec
drag_folder2_test = hSpecGoatTesterWithOwlPFState emptyOwlPFState $ do

  setMarker "setup scene"
  drawCanvasBox (0,0,10,10) -- 1
  addFolder someFolderName -- 2
  drawCanvasBox (10,10,10,10) -- 3
  addFolder "innerfolder" -- 4
  -- 2testfolder 
  --   4innerfolder
  --   3<box>
  -- 1<box> 

  setMarker "drag second box into inner folder"
  layerMouseDownUpRel LMO_Normal 2 1
  layerMouseDownRel LMO_Normal 2 1
  -- need to drag away first in order to move it into the folder
  layerMouseDownRel LMO_Normal 1 1
  layerMouseDownRel LMO_Normal 2 1
  layerMouseDownRel LMO_Normal 2 1
  layerMouseUpRel LMO_Normal 2 1
  verifyNthEntryInLayersIs 2 3
  -- 2testfolder
  --   4innerfolder
  --     3<box>
  -- 1<box>

  beforeDrag <- getLayersState

  -- DELETE
  --failWithMessage ("\n" <> show beforeDrag)

  setMarker "drag inner folder outside of outer folder"
  layerMouseDownUpRel LMO_Normal 1 1
  layerMouseDownRel LMO_Normal 1 1
  layerMouseDownRel LMO_Normal 5 0
  layerMouseUpRel LMO_Normal 5 0
  verifyNthEntryInLayersIs 2 4
  -- 2testfolder
  -- 1<box>
  -- 4innerfolder
  --   3<box>

  -- DELETE
  --afterDrag <- getLayersState
  --failWithMessage ("\n" <> show afterDrag)

  setMarker "undo"
  pressUndo
  afterUndo <- getLayersState
  verifyEqual "undo state is same as before drag the folder" beforeDrag afterUndo

  -- DELETE
  --failWithMessage ("\n" <> show afterUndo)

  setMarker "collapse the inner folder"
  layerMouseDownUpRel LMO_Collapse 1 1

  -- DELETE
  --ls <- getLayersState
  --failWithMessage ("\n" <> show ls)

  verifyNthEntryInLayersIs 1 4
  verifyNthEntryInLayersIs 2 1
  -- 2testfolder
  --   4innerfolder
  -- 1<box>
  


drag_folder_depth_test :: Spec
drag_folder_depth_test = hSpecGoatTesterWithOwlPFState emptyOwlPFState $ do

  setMarker "setup scene"
  drawCanvasBox (0,0,10,10) -- 1
  addFolder someFolderName -- 2
  addFolder someFolderName -- 3
  addFolder someFolderName -- 4
  addFolder someFolderName -- 5
  addFolder someFolderName -- 6
  addFolder someFolderName -- 7
  addFolder someFolderName -- 8

  -- 2testfolder
  --   3testfolder
  --     4testfolder
  --       5testfolder
  --         6testfolder
  --           7testfolder
  --             8testfolder
  -- 1<box>

  setMarker "drag the box into the 8th folder"
  layerMouseDownUpRel LMO_Normal 7 0
  layerMouseDownRel LMO_Normal 7 0
  -- we are outside of the last folder so we need to drag above ourselves
  layerMouseDownUpRel (LMO_DropInFolder 0) 7 7
  verifyNthEntryInLayersIs 7 1
  verifyNthEntryInLayersHasDepth 7 7

  -- 2testfolder
  --   3testfolder
  --     4testfolder
  --       5testfolder
  --         6testfolder
  --           7testfolder
  --             8testfolder
  --               1<box>

  setMarker "deselect"
  pressEscape

  setMarker "drag the box into the 5th folder"
  layerMouseDownUpRel LMO_Normal 7 7
  layerMouseDownRel LMO_Normal 7 7

  -- we are inside the last folder, we are dragging BELOW ourselves
  layerMouseDownUpRel (LMO_DropInFolder (-3)) 8 7
  verifyNthEntryInLayersIs 7 1
  verifyNthEntryInLayersHasDepth 7 4



  -- 2testfolder
  --   3testfolder
  --     4testfolder
  --       5testfolder
  --         6testfolder
  --           7testfolder
  --             8testfolder
  --         1<box>



spec :: Spec
spec = do
  describe "Layers" $ do
    describe "create_select_test" $ create_select_test
    describe "shift_select_test" $ shift_select_test
    describe "rename_focus_clickCanvas_test" $ rename_focus_clickCanvas_test 
    describe "rename_focus_setFocus_test canvas" $ rename_focus_setFocus_test GoatFocusedArea_Canvas
    describe "rename_focus_setFocus_test other" $ rename_focus_setFocus_test GoatFocusedArea_Other
    describe "create_in_folder_and_collapse_test" $ create_in_folder_and_collapse_test
    describe "folder_collapse_test" $ folder_collapse_test
    describe "lock_or_hide_select_test hide" $ lock_or_hide_select_test LMO_Hide
    describe "lock_or_hide_select_test lock" $ lock_or_hide_select_test LMO_Lock
    describe "drag_folder_test" $ drag_folder_test
    describe "drag_folder2_test" $ drag_folder2_test
    describe "drag_folder_depth_test" $ drag_folder_depth_test
