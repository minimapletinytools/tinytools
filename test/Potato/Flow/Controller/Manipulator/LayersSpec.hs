{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.Manipulator.LayersSpec
  ( spec
  )
where

import           Relude                            hiding (empty, fromList)

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit          (fromHUnitTest)
import           Test.HUnit

import           Potato.Flow
import           Potato.Flow.Controller.GoatWidget
import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input

import           Potato.Flow.Common
import           Potato.Flow.TestStates

import           Data.Default
import qualified Data.IntMap                       as IM
import qualified Data.Sequence                     as Seq

moveOffset :: Int
moveOffset = 100

collapseOffset :: Int
collapseOffset = 0

hideOffset :: Int
hideOffset = 1

lockOffset :: Int
lockOffset = 2


numLayerEntriesEqualPredicate :: Int -> EverythingPredicate
numLayerEntriesEqualPredicate n = FunctionPredicate $
  (\lentries ->
    let nlentries = Seq.length lentries
    in ("LayerEntries count: " <> show nlentries <> " expected: " <> show n <> " lentries:\n" <> layerEntriesToPrettyText lentries, nlentries == n))
  . _layersState_entries . _goatState_layersState


numVisibleHiddenLayerEntriesEqualPredicate :: Int -> EverythingPredicate
numVisibleHiddenLayerEntriesEqualPredicate n = FunctionPredicate $
  (\lentries ->
    let nhidden = Seq.length $ Seq.filter (lockHiddenStateToBool . _layerEntry_hideState) lentries
    in ("Hidden: " <> show nhidden <> " expected: " <> show n <> " lentries: " <> layerEntriesToPrettyText lentries, nhidden == n))
  . _layersState_entries . _goatState_layersState

numVisibleLockedEltsLayerEntriesPredicate :: Int -> EverythingPredicate
numVisibleLockedEltsLayerEntriesPredicate n = FunctionPredicate $
  (\lentries ->
    let nlocked = Seq.length $ Seq.filter (lockHiddenStateToBool . _layerEntry_lockState) lentries
    in ("Locked: " <> show nlocked <> " expected: " <> show n <> " lentries:\n" <> layerEntriesToPrettyText lentries, nlocked == n))
  . _layersState_entries . _goatState_layersState

-- this should work with any initial state so long as default names aren't used
test_LayersHandler_basic :: Test
test_LayersHandler_basic = constructTest "basic" owlpfstate_basic1 bs expected where
  bs = [

      EWCLabel "select"
      , EWCMouse (LMouseData (V2 moveOffset 0) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 moveOffset 0) True MouseButton_Left [] True)

      , EWCLabel "deselect"
      , EWCMouse (LMouseData (V2 moveOffset 0) False MouseButton_Left [KeyModifier_Shift] True)
      , EWCMouse (LMouseData (V2 moveOffset 0) True MouseButton_Left [KeyModifier_Shift] True)

      , EWCLabel "select and cancel"
      , EWCMouse (LMouseData (V2 moveOffset 0) False MouseButton_Left [] True)
      , EWCKeyboard (KeyboardData KeyboardKey_Esc [])
      , EWCMouse (LMouseData (V2 moveOffset 0) True MouseButton_Left [] True)

      , EWCLabel "shift select 2 elts"
      , EWCMouse (LMouseData (V2 moveOffset 0) False MouseButton_Left [KeyModifier_Shift] True)
      , EWCMouse (LMouseData (V2 moveOffset 0) True MouseButton_Left [KeyModifier_Shift] True)
      , EWCMouse (LMouseData (V2 moveOffset 1) False MouseButton_Left [KeyModifier_Shift] True)
      , EWCMouse (LMouseData (V2 moveOffset 1) True MouseButton_Left [KeyModifier_Shift] True)

      , EWCLabel "out of bounds"
      , EWCMouse (LMouseData (V2 124 10232) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 124 10234) True MouseButton_Left [] True)

    ]
  expected = [
      LabelCheck "select"
      , numSelectedEltsEqualPredicate 0
      , numSelectedEltsEqualPredicate 1

      , LabelCheck "deselect"
      , numSelectedEltsEqualPredicate 1
      , numSelectedEltsEqualPredicate 0

      , LabelCheck "select and cancel"
      , numSelectedEltsEqualPredicate 0
      , numSelectedEltsEqualPredicate 0
      , numSelectedEltsEqualPredicate 0

      , LabelCheck "shift select 2 elts"
      , numSelectedEltsEqualPredicate 0
      , numSelectedEltsEqualPredicate 1
      , numSelectedEltsEqualPredicate 1
      , numSelectedEltsEqualPredicate 2

      , LabelCheck "out of bounds"
      , numSelectedEltsEqualPredicate 2
      , numSelectedEltsEqualPredicate 2 -- TODO change to 0 once deselect via LayersHandler is supported
    ]

test_LayersHandler_toggle :: Test
test_LayersHandler_toggle = constructTest "toggle" owlpfstate_basic1 bs expected where
  bs = [
      EWCLabel "lock"
      , EWCNothing
      , EWCMouse (LMouseData (V2 lockOffset 0) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 lockOffset 0) True MouseButton_Left [] True)

      , EWCLabel "hide"
      , EWCNothing
      , EWCMouse (LMouseData (V2 hideOffset 0) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 hideOffset 0) True MouseButton_Left [] True)

      -- TODO inherit/folder test stuff
    ]
  expected = [
      LabelCheck "lock"
      , numVisibleLockedEltsLayerEntriesPredicate 0
      , numVisibleLockedEltsLayerEntriesPredicate 1
      , numVisibleLockedEltsLayerEntriesPredicate 1

      , LabelCheck "hide"
      , numVisibleHiddenLayerEntriesEqualPredicate 0
      , numVisibleHiddenLayerEntriesEqualPredicate 1
      , numVisibleHiddenLayerEntriesEqualPredicate 1
    ]

test_LayersHandler_collapse :: Test
test_LayersHandler_collapse = constructTest "collapse" owlpfstate_basic2 bs expected where
  bs = [

      EWCLabel "expand fstart1"
      , EWCNothing
      , EWCMouse (LMouseData (V2 collapseOffset 0) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 collapseOffset 0) True MouseButton_Left [] True)
      , EWCLabel "expand fstart2"
      , EWCMouse (LMouseData (V2 (1+collapseOffset) 1) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 (1+collapseOffset) 1) True MouseButton_Left [] True)
      , EWCLabel "collapse fstart2"
      , EWCMouse (LMouseData (V2 (1+collapseOffset) 1) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 (1+collapseOffset) 1) True MouseButton_Left [] True)
      , EWCLabel "expand fstart3"
      , EWCMouse (LMouseData (V2 (1+collapseOffset) 2) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 (1+collapseOffset) 2) True MouseButton_Left [] True)
      , EWCLabel "collapse fstart1"
      , EWCMouse (LMouseData (V2 collapseOffset 0) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 collapseOffset 0) True MouseButton_Left [] True)

      -- TODO select folder test (not support yet)

    ]
  expected = [
      LabelCheck "expand fstart1"
      , numLayerEntriesEqualPredicate 1
      , numLayerEntriesEqualPredicate 3
      , numLayerEntriesEqualPredicate 3
      , LabelCheck "expand fstart2"
      , numLayerEntriesEqualPredicate 7
      , numLayerEntriesEqualPredicate 7
      , LabelCheck "collapse fstart2"
      , numLayerEntriesEqualPredicate 3
      , numLayerEntriesEqualPredicate 3
      , LabelCheck "expand fstart3"
      , numLayerEntriesEqualPredicate 5
      , numLayerEntriesEqualPredicate 5
      , LabelCheck "collapse fstart1"
      , numLayerEntriesEqualPredicate 1
      , numLayerEntriesEqualPredicate 1
    ]

test_LayersHandler_move :: Test
test_LayersHandler_move = constructTest "move" owlpfstate_basic1 bs expected where
  bs = [

      EWCLabel "select b1"
      , EWCMouse (LMouseData (V2 moveOffset 0) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 moveOffset 0) True MouseButton_Left [] True)

      , EWCLabel "drag b1"
      , EWCMouse (LMouseData (V2 moveOffset 0) False MouseButton_Left [] True)
      -- must enter "Dragging" state for handler to work correctly
      , EWCMouse (LMouseData (V2 moveOffset 4) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 moveOffset 4) True MouseButton_Left [] True)

      -- TODO folder drag/move

    ]
  expected = [
      LabelCheck "select b1"
      , numSelectedEltsEqualPredicate 0
      , numSelectedEltsEqualPredicate 1
      , LabelCheck "drag b1"
      , firstSelectedSuperOwlWithOwlTreePredicate (Just "b1") $ \od sowl -> owlTree_rEltId_toFlattenedIndex_debug od (_superOwl_id sowl) == 0
      , AlwaysPass
      , firstSelectedSuperOwlWithOwlTreePredicate (Just "b1") $ \od sowl -> owlTree_rEltId_toFlattenedIndex_debug od (_superOwl_id sowl) == 3
    ]

expandfoldersforbasic2 :: [GoatWidgetCmd]
expandfoldersforbasic2 = [
    EWCLabel "expand fstart1"
    , EWCNothing
    , EWCMouse (LMouseData (V2 0 0) False MouseButton_Left [] True) -- expands folder on the downclick
    , EWCMouse (LMouseData (V2 0 0) True MouseButton_Left [] True) -- but we still need to release the mouse to continue

    , EWCLabel "expand fstart2"
    , EWCMouse (LMouseData (V2 1 1) False MouseButton_Left [] True)
    , EWCMouse (LMouseData (V2 1 1) True MouseButton_Left [] True)]

expandfoldersforbasic2_expected :: [EverythingPredicate]
expandfoldersforbasic2_expected = [
    LabelCheck "expand fstart1"
    , numLayerEntriesEqualPredicate 1
    , numLayerEntriesEqualPredicate 3
    , numLayerEntriesEqualPredicate 3

    , LabelCheck "expand fstart2"
    , numLayerEntriesEqualPredicate 7
    , numLayerEntriesEqualPredicate 7
  ]

test_LayersHandler_folders :: Test
test_LayersHandler_folders = constructTest "folders" owlpfstate_basic2 bs expected where
  bs = expandfoldersforbasic2 <> [
      EWCLabel "select b1"
      , EWCMouse (LMouseData (V2 moveOffset 2) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 moveOffset 2) True MouseButton_Left [] True)

      -- drag b1 to first elt in fstart1
      , EWCLabel "drag b1"
      , EWCMouse (LMouseData (V2 moveOffset 2) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 moveOffset 1) False MouseButton_Left [] True) -- must enter "Dragging" state for handler to work correctly
      , EWCMouse (LMouseData (V2 moveOffset 1) True MouseButton_Left [] True)

      -- drag b1 into fstart3 (collapsed)
      , EWCMouse (LMouseData (V2 moveOffset 1) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 moveOffset 7) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 moveOffset 7) True MouseButton_Left [] True) -- go far to the right so it goes inside the folder

      , EWCLabel "expand fstart3"
      , EWCMouse (LMouseData (V2 1 5) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 1 5) True MouseButton_Left [] True)

      , EWCLabel "collapse fstart2"
      , EWCMouse (LMouseData (V2 1 1) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 1 1) True MouseButton_Left [] True)

      , EWCLabel "collapse fstart1"
      , EWCMouse (LMouseData (V2 0 0) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 0 0) True MouseButton_Left [] True)

    ]
  expected = expandfoldersforbasic2_expected <> [
      LabelCheck "select b1"
      , numSelectedEltsEqualPredicate 0
      , firstSelectedSuperOwlWithOwlTreePredicate (Just "b1") $ \_ _ -> True

      , LabelCheck "drag b1"
      , AlwaysPass
      , AlwaysPass
      , firstSelectedSuperOwlWithOwlTreePredicate (Just "b1") $ \od sowl -> _owlEltMeta_parent (_superOwl_meta sowl) == 0

      , AlwaysPass
      , AlwaysPass
      , numLayerEntriesEqualPredicate 6

      , LabelCheck "expand fstart3"
      , numLayerEntriesEqualPredicate 9
      , AlwaysPass

      , LabelCheck "collapse fstart2"
      , numLayerEntriesEqualPredicate 6
      , numLayerEntriesEqualPredicate 6

      , LabelCheck "collapse fstart1"
      , numLayerEntriesEqualPredicate 1
      , numLayerEntriesEqualPredicate 1
    ]

test_LayersHandler_folders2 :: Test
test_LayersHandler_folders2 = constructTest "folders2" owlpfstate_basic2 bs expected where
  bs = expandfoldersforbasic2 <> [
      EWCLabel "expand fstart3"
      , EWCMouse (LMouseData (V2 1 6) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 1 6) True MouseButton_Left [] True)

      , EWCLabel "select b1"
      , EWCMouse (LMouseData (V2 moveOffset 2) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 moveOffset 2) True MouseButton_Left [] True)

      -- drag b1 to the end
      , EWCLabel "drag b1"
      , EWCMouse (LMouseData (V2 moveOffset 2) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 moveOffset 9) False MouseButton_Left [] True) -- must enter "Dragging" state for handler to work correctly
      , EWCMouse (LMouseData (V2 moveOffset 9) True MouseButton_Left [] True)

      -- drag b1 to parent level
      , EWCLabel "drag b1 2"
      , EWCMouse (LMouseData (V2 moveOffset 8) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 0 10) False MouseButton_Left [] True) -- must enter "Dragging" state for handler to work correctly
      , EWCMouse (LMouseData (V2 0 10) True MouseButton_Left [] True)

      -- drag fstart2 to parent level
      , EWCLabel "drag fstart2"
      --first select it
      , EWCMouse (LMouseData (V2 moveOffset 1) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 moveOffset 1) True MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 moveOffset 1) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 0 10) False MouseButton_Left [] True) -- must enter "Dragging" state for handler to work correctly
      , EWCMouse (LMouseData (V2 0 10) True MouseButton_Left [] True)
    ]
  expected = expandfoldersforbasic2_expected <> [
      LabelCheck "expand fstart3"
      , numLayerEntriesEqualPredicate 9
      , numSelectedEltsEqualPredicate 0

      , LabelCheck "select b1"
      , numSelectedEltsEqualPredicate 0
      , firstSelectedSuperOwlWithOwlTreePredicate (Just "b1") $ \_ _ -> True

      , LabelCheck "drag b1"
      , AlwaysPass
      , AlwaysPass
      , firstSelectedSuperOwlWithOwlTreePredicate (Just "b1") $ \od sowl -> _owlEltMeta_parent (_superOwl_meta sowl) == 7

      , LabelCheck "drag b1 2"
      , AlwaysPass
      , AlwaysPass
      , firstSelectedSuperOwlWithOwlTreePredicate (Just "b1") $ \od sowl -> _owlEltMeta_parent (_superOwl_meta sowl) == noOwl

      , LabelCheck "drag fstart2"
      , AlwaysPass
      , firstSelectedSuperOwlWithOwlTreePredicate (Just "fstart2") $ \_ _ -> True
      , AlwaysPass
      , AlwaysPass
      , firstSelectedSuperOwlWithOwlTreePredicate (Just "fstart2") $ \od sowl -> _owlEltMeta_parent (_superOwl_meta sowl) == noOwl
    ]

test_LayersHandler_scroll :: Test
test_LayersHandler_scroll = constructTest "scroll" owlpfstate_basic1 bs expected where
  bs = [
      -- click first to focus on layers (so it can register keyboard input)
      EWCLabel "select b1"
      , EWCMouse (LMouseData (V2 moveOffset 0) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 moveOffset 0) True MouseButton_Left [] True)

      , EWCLabel "scroll"
      , EWCKeyboard (KeyboardData (KeyboardKey_Scroll 1) [])

      , EWCLabel "select b2"
      , EWCMouse (LMouseData (V2 moveOffset 0) False MouseButton_Left [] True)
      , EWCMouse (LMouseData (V2 moveOffset 0) True MouseButton_Left [] True)

      -- TODO folder drag/move

    ]
  expected = [
      LabelCheck "select b1"
      , numSelectedEltsEqualPredicate 0
      , firstSelectedSuperOwlWithOwlTreePredicate (Just "b1") $ \_ _ -> True

      , LabelCheck "scroll"
      , AlwaysPass

      , LabelCheck "select b2"
      , numSelectedEltsEqualPredicate 1 -- b1 still selected
      , firstSelectedSuperOwlWithOwlTreePredicate (Just "b2") $ \_ _ -> True
    ]


spec :: Spec
spec = do
  describe "LayersHandler" $ do
    fromHUnitTest $ test_LayersHandler_basic
    fromHUnitTest $ test_LayersHandler_toggle
    fromHUnitTest $ test_LayersHandler_collapse
    fromHUnitTest $ test_LayersHandler_move
    fromHUnitTest $ test_LayersHandler_folders
    fromHUnitTest $ test_LayersHandler_folders2
    fromHUnitTest $ test_LayersHandler_scroll
