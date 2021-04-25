module Potato.Flow.OwlWorkspaceSpec (
  spec
) where


import           Relude

import           Test.Hspec

import qualified Data.Text as T
import qualified Data.Sequence            as Seq
import           Potato.Flow.Owl
import           Potato.Flow.OwlState
import           Potato.Flow.OwlWorkspace
import           Potato.Flow.TestStates
import           Potato.Flow.State
import Potato.Flow.Types
import Potato.Flow.SElts

verifyOwlAt :: OwlPFWorkspace -> OwlSpot -> (SuperOwl -> Bool) -> Bool
verifyOwlAt ws ospot f = fromMaybe False $ do
  let
    ot = _owlPFState_owlTree . _owlPFWorkspace_pFState $ ws
  sowl <- owlTree_findSuperOwlAtOwlSpot ospot ot
  Just $ f sowl


pred_nameIs :: Text -> SuperOwl -> Bool
pred_nameIs n sowl = owl_name sowl == n


spec :: Spec
spec = do
  describe "OwlWorkspace" $ do
    let
      owlTree2 = owlTree_fromOldState (_pFState_directory pfstate_basic2) (_pFState_layers pfstate_basic2)
      someState2 = OwlPFState owlTree2 someSCanvas
      someWorkspace2 = loadOwlPFStateIntoWorkspace someState2 emptyWorkspace

      spot1 = OwlSpot (-1) Nothing

      owlElt1 = OwlEltSElt (OwlInfo "💩") SEltNone

    describe "updateOwlPFWorkspace" $ do
      it "WSEAddElt" $ do
        let
          wse = WSEAddElt (False, spot1, owlElt1)
          newws = updateOwlPFWorkspace wse someWorkspace2
        verifyOwlAt newws spot1 (pred_nameIs (owl_name owlElt1)) `shouldBe` True

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
