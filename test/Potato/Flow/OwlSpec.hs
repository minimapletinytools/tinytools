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


-- | 'owlDirectory_toSEltTree' may change SEltFolderEnd name and REltId during reconstruction
-- so we simply filter it out so we can compare the parts that should not have changed
filterFolderEndFn :: (REltId, SEltLabel) -> Bool
filterFolderEndFn (_, SEltLabel _ SEltFolderEnd) = False
filterFolderEndFn _ = True

spec :: Spec
spec = do
  describe "Owl" $ do
    let
      owlDirectory2 = owlDirectory_fromOldState (_pFState_directory pfstate_basic2) (_pFState_layers pfstate_basic2)
      owlMapping2 = _owlDirectory_mapping owlDirectory2
    describe "OwlDirectory" $ do
      let
        sEltTree2 = _sPotatoFlow_sEltTree (pFState_to_sPotatoFlow pfstate_basic2)
      it "owlDirectory_fromOldState" $ do
        --forM (owlDirectory_prettyPrint owlDirectory2) print
        (Seq.length (owlDirectory_topSuperOwls owlDirectory2) `shouldBe` 1)
        owlDirectory_owlCount owlDirectory2 `shouldBe` 9
      it "owliterateall" $ do
        toList (fmap _superOwl_id (owliterateall owlDirectory2)) `shouldBe` [0,1,2,3,4,5,7,8,9]
      it "owlDirectory_toSEltTree" $ do
        filter filterFolderEndFn (owlDirectory_toSEltTree owlDirectory2) `shouldBe` filter filterFolderEndFn sEltTree2
      it "owlDirectory_removeSuperOwl" $ do
        let
          sowl1 = owlDirectory_mustFindSuperOwl 2 owlDirectory2 -- b1
          sowl2 = owlDirectory_mustFindSuperOwl 1 owlDirectory2 -- fstart2
        owlDirectory_owlCount (owlDirectory_removeSuperOwl sowl1 owlDirectory2) `shouldBe` 8
        owlDirectory_owlCount (owlDirectory_removeSuperOwl sowl2 owlDirectory2) `shouldBe` 4
      it "owlDirectory_addOwlElt" $ do
        let
          owlSpot1 = OwlSpot (-1) Nothing
          owlElt1 = OwlEltSElt (OwlInfo "💩") SEltNone
          rid = owlDirectory_maxId owlDirectory2 + 1
          owlSpot2 = OwlSpot 7 (Just 9)
        owlDirectory_owlCount (owlDirectory_addOwlElt owlSpot1 rid owlElt1 owlDirectory2) `shouldBe` 10
        owlDirectory_owlCount (owlDirectory_addOwlElt owlSpot2 rid owlElt1 owlDirectory2) `shouldBe` 10
        -- too lazy to write proper test, just print and verify manually
        --putTextLn (owlDirectory_prettyPrint $ owlDirectory_addOwlElt owlSpot2 rid owlElt1 owlDirectory2)
      it "owlDirectory_moveOwlParliament" $ do
        let
          owlSpot1 = OwlSpot (-1) Nothing
          parliament = OwlParliament $ Seq.fromList [2,3,7]
        owlDirectory_owlCount (owlDirectory_moveOwlParliament parliament owlSpot1 owlDirectory2) `shouldBe` 9
        -- too lazy to write proper test, just print and verify manually
        --putTextLn (owlDirectory_prettyPrint $ owlDirectory_moveOwlParliament parliament owlSpot1 owlDirectory2)
      it "owlDirectory_addSEltTree" $ do
        let
          owlSpot1 = OwlSpot (-1) Nothing
          seltree1 = owlDirectory_toSEltTree owlDirectory2
        owlDirectory_owlCount (owlDirectory_addSEltTree owlSpot1 seltree1 owlDirectory2) `shouldBe` 18
    describe "OwlParliament" $ do
      it "superOwlParliament_isValid" $ do
        --putTextLn (owlDirectory_prettyPrint owlDirectory2)
        let
          validParliament = owlParliament_toSuperOwlParliament owlDirectory2 (OwlParliament $ Seq.fromList [2,3,7])
          invalidParliament = owlParliament_toSuperOwlParliament owlDirectory2 (OwlParliament $ Seq.fromList [2,3,7,9])
        superOwlParliament_isValid owlMapping2 validParliament `shouldBe` True
        superOwlParliament_isValid owlMapping2 invalidParliament `shouldBe` False
