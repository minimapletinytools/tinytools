module Potato.Flow.OwlWorkspaceSpec (
  spec
) where


import           Relude

import           Test.Hspec

import qualified Data.Sequence            as Seq
import Data.Maybe (fromJust)
import           Potato.Flow.OwlItem
import Potato.Flow.Owl
import           Potato.Flow.OwlItem
import Potato.Flow.OwlState
import           Potato.Flow.OwlItem
import Potato.Flow.OwlWorkspace
import           Potato.Flow.Deprecated.TestStates
import           Potato.Flow.Deprecated.State
import Potato.Flow.SElts

verifyOwlAt :: OwlPFWorkspace -> OwlSpot -> (SuperOwl -> Bool) -> Bool
verifyOwlAt ws ospot f = fromMaybe False $ do
  let
    ot = _owlPFState_owlTree . _owlPFWorkspace_pFState $ ws
  sowl <- owlTree_findSuperOwlAtOwlSpot ot ospot
  Just $ f sowl

pred_nameIs :: Text -> SuperOwl -> Bool
pred_nameIs n sowl = hasOwlItem_name sowl == n

undoAndVerify :: OwlPFWorkspace -> OwlPFState -> Bool
undoAndVerify ws prev = r where
  undows = updateOwlPFWorkspace WSEUndo ws
  newstate = _owlPFWorkspace_pFState undows
  r =
    owlTree_equivalent (_owlPFState_owlTree prev) (_owlPFState_owlTree newstate)
    && (_owlPFState_canvas prev) == (_owlPFState_canvas newstate)


spec :: Spec
spec = do
  describe "OwlWorkspace" $ do
    let
      owlTree0 = owlTree_fromOldState (_pFState_directory pfstate_basic2) (_pFState_layers pfstate_basic2)
      someState0 = OwlPFState owlTree0 someSCanvas
      someWorkspace0 = loadOwlPFStateIntoWorkspace someState0 emptyWorkspace

      spot1 = OwlSpot (-1) Nothing
      spot2 = OwlSpot 7 (Just 9)

      owlItem1 = OwlItem (OwlInfo "💩") OwlSubItemNone
      owlItem2 = OwlItem (OwlInfo "🍅") OwlSubItemNone
      owlItem3 = OwlItem (OwlInfo "🧀") (OwlSubItemFolder Seq.empty)

      owlItems = Seq.fromList [owlItem1, owlItem2, owlItem3]

    describe "updateOwlPFWorkspace" $ do
      it "WSEAddElt" $ do
        let
          wse1 = WSEAddElt (False, spot1, owlItem1)
          newws1 = updateOwlPFWorkspace wse1 someWorkspace0
        verifyOwlAt newws1 spot1 (pred_nameIs (hasOwlItem_name owlItem1)) `shouldBe` True
        undoAndVerify newws1 (_owlPFWorkspace_pFState someWorkspace0) `shouldBe` True
        let
          wse2 = WSEAddElt (False, spot2, owlItem1)
          newws2 = updateOwlPFWorkspace wse2 someWorkspace0
        --putTextLn $ debugPrintOwlPFState (_owlPFWorkspace_pFState newws2)
        verifyOwlAt newws2 spot2 (pred_nameIs (hasOwlItem_name owlItem1)) `shouldBe` True
        undoAndVerify newws2 (_owlPFWorkspace_pFState someWorkspace0) `shouldBe` True
      it "WSEAddRelative" $ do
        let
          wse1 = WSEAddRelative (spot1, owlItems)
          newws1 = updateOwlPFWorkspace wse1 someWorkspace0
          ot1 = _owlPFState_owlTree $ _owlPFWorkspace_pFState newws1
        --putTextLn $ debugPrintOwlPFState (_owlPFWorkspace_pFState newws1)
        verifyOwlAt newws1 spot1 (pred_nameIs (hasOwlItem_name owlItem1)) `shouldBe` True
        verifyOwlAt newws1 (fromJust $ owlTree_goRightFromOwlSpot ot1 spot1) (pred_nameIs (hasOwlItem_name owlItem2)) `shouldBe` True
        undoAndVerify newws1 (_owlPFWorkspace_pFState someWorkspace0) `shouldBe` True
      it "WSEAddFolder" $ do
        let
          folderName = "🥕🥕🥕"
          wse1 = WSEAddFolder (spot2, folderName)
          newws1 = updateOwlPFWorkspace wse1 someWorkspace0
          ot1 = _owlPFState_owlTree $ _owlPFWorkspace_pFState newws1
        --putTextLn $ debugPrintOwlPFState (_owlPFWorkspace_pFState newws1)
        verifyOwlAt newws1 spot2 (pred_nameIs folderName) `shouldBe` True
        undoAndVerify newws1 (_owlPFWorkspace_pFState someWorkspace0) `shouldBe` True
        -- create a child in the folder we just created and verify it got added correctly
        let
          sowl = fromJust $ owlTree_findSuperOwlAtOwlSpot ot1 spot2
          childSpot = OwlSpot (_superOwl_id sowl) Nothing
          wse2 = WSEAddElt (False, childSpot, owlItem1)
          newws2 = updateOwlPFWorkspace wse2 newws1
        verifyOwlAt newws2 childSpot (pred_nameIs (hasOwlItem_name owlItem1)) `shouldBe` True
        undoAndVerify newws2 (_owlPFWorkspace_pFState newws1) `shouldBe` True
      it "WSERemoveElt" $ do
        let
          --remove b1
          wse1 = WSERemoveElt (OwlParliament $ Seq.fromList [2])
          newws1 = updateOwlPFWorkspace wse1 someWorkspace0
        --putTextLn $ debugPrintOwlPFState (_owlPFWorkspace_pFState newws1)
        -- b2 should now be where b1 was
        verifyOwlAt newws1 (OwlSpot 1 Nothing) (pred_nameIs ("b2")) `shouldBe` True
        undoAndVerify newws1 (_owlPFWorkspace_pFState someWorkspace0) `shouldBe` True
      it "WSEMoveElt" $ do
        let
          -- move b2 to b1
          b1spot = owlTree_rEltId_toOwlSpot owlTree0 2
          wse1 = WSEMoveElt (b1spot, (OwlParliament $ Seq.fromList [3]))
          newws1 = updateOwlPFWorkspace wse1 someWorkspace0
        verifyOwlAt newws1 b1spot (pred_nameIs "b2") `shouldBe` True
        undoAndVerify newws1 (_owlPFWorkspace_pFState someWorkspace0) `shouldBe` True
        {-
      it "WSEResizeCanvas" $ do
        1 `shouldBe` 1
      it "WSEUndo" $ do
        1 `shouldBe` 1
      it "WSERedo" $ do
        1 `shouldBe` 1
      it "WSELoad" $ do
        1 `shouldBe` 1
        -}
