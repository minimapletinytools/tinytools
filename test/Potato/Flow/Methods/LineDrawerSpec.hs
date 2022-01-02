{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Methods.LineDrawerSpec(
  spec
) where

import           Relude           hiding (empty, fromList)

import           Test.Hspec


import Potato.Flow
import           Potato.Flow.Methods.LineDrawer
import Potato.Flow.TestStates

import Potato.Flow.Common


rotateMe_validate :: (Eq a, RotateMe a) => a -> Bool
rotateMe_validate a = (rotateMe_Left . rotateMe_Right $ a) == a && (rotateMe_Right . rotateMe_Left $ a) == a

spec :: Spec
spec = do
  describe "Lines - internal" $ do
    it "rotateMe" $ do
      let
        somelbx1 = LBox (V2 12 (-2)) (V2 12323 (143))
        somexy1 :: XY = V2 345 21
      rotateMe_validate somelbx1 `shouldBe` True
      rotateMe_validate somexy1 `shouldBe` True
    it "determineSeparation" $ do
      let
        lb1 = LBox (V2 0 0) (V2 10 10)
        lb2 = LBox (V2 11 11) (V2 10 10)
      determineSeparation (lb1, (0,0,0,0)) (lb2, (0,0,0,0)) `shouldBe` (True, True)
      determineSeparation (lb1, (2,2,0,0)) (lb2, (0,0,0,0)) `shouldBe` (False, True)
      determineSeparation (lb1, (1,1,1,1)) (lb2, (1,1,1,1)) `shouldBe` (False, False)
    it "lineAnchorsForRender_simplify" $ do
      let
        lineanchors = LineAnchorsForRender {
            _lineAnchorsForRender_start = 0
            , _lineAnchorsForRender_rest = [(CD_Up, 10),(CD_Up, 15),(CD_Up, 1),(CD_Right, 10)]
          }
      _lineAnchorsForRender_rest (lineAnchorsForRender_simplify lineanchors) `shouldBe` [(CD_Up, 26),(CD_Right, 10)]
  describe "Lines - rendering" $ do
    let
      pfs = owlpfstate_attachments2
      --somessline = hasOwlElt_test_toSSimpleLine $ hasOwlTree_test_mustFindFirstSuperOwlByName pfs "b1-> <-b4"
      somessline = hasOwlElt_test_toSSimpleLine $ hasOwlTree_test_mustFindFirstSuperOwlByName pfs "<-b1 b2->"
      owltree = _owlPFState_owlTree pfs
      sd = sSimpleLineNewRenderFn somessline Nothing
    it "basic" $ do
      print $ _sEltDrawer_box sd owltree
      forM_ (sEltDrawer_renderToLines owltree sd) putTextLn
      -- TODO test stuff
      --sd (V2 0 0) owltree `shouldBe` Just '<'
      True `shouldBe` True
