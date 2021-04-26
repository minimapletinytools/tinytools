module Potato.Flow.OwlWorkspaceSpec (
  spec
) where


import           Relude

import           Test.Hspec

import qualified Data.Text as T
import qualified Data.Sequence            as Seq
import Data.Maybe (fromJust)
import           Potato.Flow.Owl
import           Potato.Flow.OwlState
import           Potato.Flow.OwlWorkspace
import           Potato.Flow.TestStates
import           Potato.Flow.State
import Potato.Flow.Types
import Potato.Flow.SElts

verifyOwlAt :: OwlPFWorkspace -> OwlSpot -> (SuperOwl -> Bool) -> Bool
verifyOwlAt ws ospot f = traceShow ospot $ fromMaybe False $ do
  let
    ot = _owlPFState_owlTree . _owlPFWorkspace_pFState $ ws
  sowl <- owlTree_findSuperOwlAtOwlSpot ospot ot
  traceShow sowl $  Just $ f sowl
  Just $ f sowl


pred_nameIs :: Text -> SuperOwl -> Bool
pred_nameIs n sowl = owl_name sowl == n


spec :: Spec
spec = do
  describe "OwlWorkspace" $ do
    let
      owlTree0 = owlTree_fromOldState (_pFState_directory pfstate_basic2) (_pFState_layers pfstate_basic2)
      someState0 = OwlPFState owlTree0 someSCanvas
      someWorkspace0 = loadOwlPFStateIntoWorkspace someState0 emptyWorkspace

      spot1 = OwlSpot (-1) Nothing
      spot2 = OwlSpot 7 (Just 9)

      owlElt1 = OwlEltSElt (OwlInfo "💩") SEltNone
      owlElt2 = OwlEltSElt (OwlInfo "🍅") SEltNone
      owlElt3 = OwlEltFolder (OwlInfo "🧀") Seq.empty

      owlElts = Seq.fromList [owlElt1, owlElt2, owlElt3]

    describe "updateOwlPFWorkspace" $ do
      it "WSEAddElt" $ do
        let
          wse1 = WSEAddElt (False, spot1, owlElt1)
          newws1 = updateOwlPFWorkspace wse1 someWorkspace0
          ot1 = _owlPFState_owlTree $ _owlPFWorkspace_pFState newws1
        verifyOwlAt newws1 spot1 (pred_nameIs (owl_name owlElt1)) `shouldBe` True
        let
          wse2 = WSEAddElt (False, spot2, owlElt1)
          newws2 = updateOwlPFWorkspace wse2 someWorkspace0
        --putTextLn $ debugPrintOwlPFState (_owlPFWorkspace_pFState newws2)
        verifyOwlAt newws2 spot2 (pred_nameIs (owl_name owlElt1)) `shouldBe` True
      it "WSEAddRelative" $ do
        let
          wse1 = WSEAddRelative (spot1, owlElts)
          newws1 = updateOwlPFWorkspace wse1 someWorkspace0
          ot1 = _owlPFState_owlTree $ _owlPFWorkspace_pFState newws1
        putTextLn $ debugPrintOwlPFState (_owlPFWorkspace_pFState newws1)
        verifyOwlAt newws1 spot1 (pred_nameIs (owl_name owlElt1)) `shouldBe` True
        verifyOwlAt newws1 (fromJust $ owlTree_goRightFromOwlSpot ot1 spot1) (pred_nameIs (owl_name owlElt2)) `shouldBe` True
      it "WSEAddFolder" $ do
        let
          folderName = "🥕🥕🥕"
          wse1 = WSEAddFolder (spot2, folderName)
          newws1 = updateOwlPFWorkspace wse1 someWorkspace0
          ot1 = _owlPFState_owlTree $ _owlPFWorkspace_pFState newws1
        putTextLn $ debugPrintOwlPFState (_owlPFWorkspace_pFState newws1)
        verifyOwlAt newws1 spot2 (pred_nameIs folderName) `shouldBe` True
        -- create a child in the folder we just created and verify it got added correctly
        let
          sowl = fromJust $ owlTree_findSuperOwlAtOwlSpot spot2 ot1
          childSpot = OwlSpot (_superOwl_id sowl) Nothing
          wse2 = WSEAddElt (False, childSpot, owlElt1)
          newws2 = updateOwlPFWorkspace wse2 newws1
        verifyOwlAt newws2 childSpot (pred_nameIs (owl_name owlElt1)) `shouldBe` True
      it "WSERemoveElt" $ do
        let
          --remove b1
          wse1 = WSERemoveElt (OwlParliament $ Seq.fromList [2])
          newws1 = updateOwlPFWorkspace wse1 someWorkspace0
        --putTextLn $ debugPrintOwlPFState (_owlPFWorkspace_pFState newws1)
        -- b2 should now be where b1 was
        verifyOwlAt newws1 (OwlSpot 1 Nothing) (pred_nameIs ("b2")) `shouldBe` True
      it "WSEMoveElt" $ do
        1 `shouldBe` 1
      it "WSEManipulate" $ do
        1 `shouldBe` 1
      it "WSEResizeCanvas" $ do
        1 `shouldBe` 1
      it "WSEUndo" $ do
        1 `shouldBe` 1
      it "WSERedo" $ do
        1 `shouldBe` 1
      it "WSELoad" $ do
        1 `shouldBe` 1
{-
        data WSEvent =
          WSEAddElt (Bool, OwlSpot, OwlElt)
          | WSEAddRelative (OwlSpot, Seq OwlElt)
          | WSEAddFolder (OwlSpot, Text)
          | WSERemoveElt OwlParliament -- removed kiddos get adopted by grandparents or w/e?
          | WSEMoveElt (OwlSpot, SuperOwlParliament) -- use SuperOwlParliament so we know where to undo back to
          -- | WSEDuplicate OwlParliament -- kiddos get duplicated??
          | WSEManipulate (Bool, ControllersWithId)
          | WSEResizeCanvas DeltaLBox
          | WSEUndo
          | WSERedo
          | WSELoad SPotatoFlow
          deriving (Show)-}
