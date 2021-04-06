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


-- | 'owlDirectory_toOldState' may change SEltFolderEnd name and REltId during reconstruction
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
      it "owlDirectory_toOldState" $ do
        filter filterFolderEndFn (owlDirectory_toOldState owlDirectory2) `shouldBe` filter filterFolderEndFn sEltTree2
    describe "OwlParliament" $ do
      it "superOwlParliament_isValid" $ do
        putTextLn (owlDirectory_prettyPrint owlDirectory2)
        let
          validParliament = owlParliament_toSuperOwlParliament owlDirectory2 (OwlParliament $ Seq.fromList [2,3,7])
          invalidParliament = owlParliament_toSuperOwlParliament owlDirectory2 (OwlParliament $ Seq.fromList [2,3,7,9])
        superOwlParliament_isValid owlMapping2 validParliament `shouldBe` True
        superOwlParliament_isValid owlMapping2 invalidParliament `shouldBe` False
