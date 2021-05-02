module Potato.Flow.OwlSpec (
  spec
) where


import           Relude

import           Test.Hspec

import qualified Data.Text as T
import qualified Data.Sequence            as Seq
import qualified Data.IntMap as IM
import           Potato.Flow.Owl
import           Potato.Flow.Deprecated.TestStates
import           Potato.Flow.Deprecated.State
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
      owlTree0 = owlTree_fromOldState (_pFState_directory pfstate_basic2) (_pFState_layers pfstate_basic2)
      owlMapping0 = _owlTree_mapping owlTree0
    describe "OwlTree" $ do
      let
        sEltTree2 = _sPotatoFlow_sEltTree (pFState_to_sPotatoFlow pfstate_basic2)
      it "owlTree_validate" $ do
        --putTextLn $ show $ owlTree_validate owlTree0
        fst (owlTree_validate owlTree0) `shouldBe` True
      it "owlTree_fromOldState" $ do
        --forM (owlTree_prettyPrint owlTree0) print
        (Seq.length (owlTree_topSuperOwls owlTree0) `shouldBe` 1)
        owlTree_owlCount owlTree0 `shouldBe` 9
      it "owliterateall" $ do
        toList (fmap _superOwl_id (owliterateall owlTree0)) `shouldBe` [0,1,2,3,4,5,7,8,9]
      it "owlTree_toSEltTree" $ do
        filter filterFolderEndFn (owlTree_toSEltTree owlTree0) `shouldBe` filter filterFolderEndFn sEltTree2
      it "owlTree_removeSuperOwl" $ do
        let
          sowl1 = owlTree_mustFindSuperOwl 2 owlTree0 -- b1
          sowl2 = owlTree_mustFindSuperOwl 1 owlTree0 -- fstart2
          ot1 = owlTree_removeSuperOwl sowl1 owlTree0
          ot2 = owlTree_removeSuperOwl sowl2 owlTree0
        owlTree_owlCount ot1 `shouldBe` 8
        owlTree_owlCount ot2 `shouldBe` 4
        fst (owlTree_validate ot1) `shouldBe` True
        fst (owlTree_validate ot2) `shouldBe` True
      it "owlTree_addOwlElt" $ do
        let
          owlSpot1 = OwlSpot (-1) Nothing
          owlElt1 = OwlEltSElt (OwlInfo "💩") SEltNone
          rid = owlTree_maxId owlTree0 + 1
          owlSpot2 = OwlSpot 7 (Just 9)
          (ot1, _) = owlTree_addOwlElt owlSpot1 rid owlElt1 owlTree0
          (ot2, _) = owlTree_addOwlElt owlSpot2 rid owlElt1 owlTree0
        owlTree_owlCount ot1 `shouldBe` 10
        owlTree_owlCount ot2 `shouldBe` 10
        fst (owlTree_validate ot1) `shouldBe` True
        fst (owlTree_validate ot2) `shouldBe` True
        -- too lazy to write proper test, just print and verify manually
        --putTextLn (owlTree_prettyPrint $ owlTree_addOwlElt owlSpot2 rid owlElt1 owlTree0)
      it "owlTree_moveOwlParliament" $ do
        let
          owlSpot1 = OwlSpot (-1) Nothing
          parliament = OwlParliament $ Seq.fromList [2,3,7]
          (ot1, c1) = owlTree_moveOwlParliament parliament owlSpot1 owlTree0
        owlTree_owlCount ot1 `shouldBe` 9
        -- 5 becaues this includes the 2 children of 7
        length c1 `shouldBe` 5
        fst (owlTree_validate ot1) `shouldBe` True
        -- too lazy to write proper test, just print and verify manually
        --putTextLn (owlTree_prettyPrint . fst $ owlTree_moveOwlParliament parliament owlSpot1 owlTree0)
      it "owlTree_addSEltTree" $ do
        let
          owlSpot1 = OwlSpot (-1) Nothing
          -- current version of owlTree_addSEltTree requires non-colliding REltIds so we reindex here
          seltree1 = fmap (\(rid,seltl) -> (rid+100, seltl)) $ owlTree_toSEltTree owlTree0
          (ot1, c1) = owlTree_addSEltTree owlSpot1 seltree1 owlTree0
        owlTree_owlCount ot1 `shouldBe` 18
        length c1 `shouldBe` 9
        fst (owlTree_validate ot1) `shouldBe` True
    describe "OwlParliament" $ do
      it "superOwlParliament_isValid" $ do
        --putTextLn (owlTree_prettyPrint owlTree0)
        let
          validParliament = owlParliament_toSuperOwlParliament owlTree0 (OwlParliament $ Seq.fromList [2,3,7])
          invalidParliament = owlParliament_toSuperOwlParliament owlTree0 (OwlParliament $ Seq.fromList [2,3,7,9])
        superOwlParliament_isValid owlMapping0 validParliament `shouldBe` True
        superOwlParliament_isValid owlMapping0 invalidParliament `shouldBe` False