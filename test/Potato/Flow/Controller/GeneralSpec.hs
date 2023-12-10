{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.GeneralSpec (
  spec
) where

import           Relude                                         hiding (empty,
                                                                 fromList)

import           Test.Hspec

import           Potato.Flow.GoatTester

import           Potato.Flow
import           Potato.Flow.Controller.Manipulator.TestHelpers

verifyHasUnsavedChanges :: Bool -> GoatTester ()
verifyHasUnsavedChanges b = do
  gs <- getGoatState
  let b' = goatState_hasUnsavedChanges gs
  verify "has unsaved changes" $ if b' == b then Nothing else Just $ "expected " <> show b <> " got " <> show b'

has_unsaved_changes_basic :: Spec
has_unsaved_changes_basic = hSpecGoatTesterWithOwlPFState emptyOwlPFState $ do
  setMarker "draw a box"
  drawCanvasBox (0,0,10,10)
  verifyHasUnsavedChanges True

  setMarker "save"
  markSaved
  verifyHasUnsavedChanges False

  setMarker "undo"
  pressUndo 
  verifyHasUnsavedChanges True

  setMarker "redo"
  pressRedo
  verifyHasUnsavedChanges False


spec :: Spec
spec = do
  describe "General" $ do
    describe "has_unsaved_changes_basic" $ has_unsaved_changes_basic

