module Potato.Flow.OwlSpec (
  spec
) where


import           Relude

import           Test.Hspec

import qualified Data.Text as T
import qualified Data.Sequence            as Seq
import           Potato.Flow.Owl
import           Potato.Flow.TestStates
import           Potato.Flow.State
import Potato.Flow.Types
import Potato.Flow.SElts


-- | 'owlTree_toSEltTree' may change SEltFolderEnd name and REltId during reconstruction
-- so we simply filter it out so we can compare the parts that should not have changed
filterFolderEndFn :: (REltId, SEltLabel) -> Bool
filterFolderEndFn (_, SEltLabel _ SEltFolderEnd) = False
filterFolderEndFn _ = True

spec :: Spec
spec = do
  describe "Owl" $ do
    let
      owlTree2 = owlTree_fromOldState (_pFState_directory pfstate_basic2) (_pFState_layers pfstate_basic2)
      owlMapping2 = _owlTree_mapping owlTree2
    describe "OwlTree" $ do
      let
        sEltTree2 = _sPotatoFlow_sEltTree (pFState_to_sPotatoFlow pfstate_basic2)
      it "owlTree_fromOldState" $ do
        --forM (owlTree_prettyPrint owlTree2) print
        (Seq.length (owlTree_topSuperOwls owlTree2) `shouldBe` 1)
        owlTree_owlCount owlTree2 `shouldBe` 9
      it "owliterateall" $ do
        toList (fmap _superOwl_id (owliterateall owlTree2)) `shouldBe` [0,1,2,3,4,5,7,8,9]
      it "owlTree_toSEltTree" $ do
        filter filterFolderEndFn (owlTree_toSEltTree owlTree2) `shouldBe` filter filterFolderEndFn sEltTree2
      it "owlTree_removeSuperOwl" $ do
        let
          sowl1 = owlTree_mustFindSuperOwl 2 owlTree2 -- b1
          sowl2 = owlTree_mustFindSuperOwl 1 owlTree2 -- fstart2
        owlTree_owlCount (owlTree_removeSuperOwl sowl1 owlTree2) `shouldBe` 8
        owlTree_owlCount (owlTree_removeSuperOwl sowl2 owlTree2) `shouldBe` 4
      it "owlTree_addOwlElt" $ do
        let
          owlSpot1 = OwlSpot (-1) Nothing
          owlElt1 = OwlEltSElt (OwlInfo "💩") SEltNone
          rid = owlTree_maxId owlTree2 + 1
          owlSpot2 = OwlSpot 7 (Just 9)
        owlTree_owlCount (owlTree_addOwlElt owlSpot1 rid owlElt1 owlTree2) `shouldBe` 10
        owlTree_owlCount (owlTree_addOwlElt owlSpot2 rid owlElt1 owlTree2) `shouldBe` 10
        -- too lazy to write proper test, just print and verify manually
        --putTextLn (owlTree_prettyPrint $ owlTree_addOwlElt owlSpot2 rid owlElt1 owlTree2)
      it "owlTree_moveOwlParliament" $ do
        let
          owlSpot1 = OwlSpot (-1) Nothing
          parliament = OwlParliament $ Seq.fromList [2,3,7]
        owlTree_owlCount (owlTree_moveOwlParliament parliament owlSpot1 owlTree2) `shouldBe` 9
        -- too lazy to write proper test, just print and verify manually
        --putTextLn (owlTree_prettyPrint $ owlTree_moveOwlParliament parliament owlSpot1 owlTree2)
      it "owlTree_addSEltTree" $ do
        let
          owlSpot1 = OwlSpot (-1) Nothing
          seltree1 = owlTree_toSEltTree owlTree2
        owlTree_owlCount (owlTree_addSEltTree owlSpot1 seltree1 owlTree2) `shouldBe` 18
    describe "OwlParliament" $ do
      it "superOwlParliament_isValid" $ do
        --putTextLn (owlTree_prettyPrint owlTree2)
        let
          validParliament = owlParliament_toSuperOwlParliament owlTree2 (OwlParliament $ Seq.fromList [2,3,7])
          invalidParliament = owlParliament_toSuperOwlParliament owlTree2 (OwlParliament $ Seq.fromList [2,3,7,9])
        superOwlParliament_isValid owlMapping2 validParliament `shouldBe` True
        superOwlParliament_isValid owlMapping2 invalidParliament `shouldBe` False
