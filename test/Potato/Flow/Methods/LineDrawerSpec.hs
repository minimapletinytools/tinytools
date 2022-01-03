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

import Data.Default

generateTestCases = r where

  -- MODIFY THESE TO TEST WHAT YOU NEED TO TEST :O
  --al1s = [AL_LEFT, AL_RIGHT, AL_TOP, AL_BOT]
  --al2s = [AL_LEFT, AL_RIGHT, AL_TOP, AL_BOT]
  al1s = [AL_TOP]
  al2s = [AL_LEFT]
  box1s = [LBox 0 5]
  box2s = [LBox (V2 3 10) 5]
  canvasbox = LBox (-5) (V2 25 25)
  startrid = 0
  endrid = 0

  boxpairs = [(b1,b2) | b1 <- box1s, b2 <- box2s]
  attachmentpairs = [(al1,al2) | al1 <- al1s, al2 <- al2s]



  makestree (b1,b2) (al1, al2) =
    [ (0, SEltLabel "b1" (SEltBox (def {_sBox_box = b1})))
    , (1, SEltLabel "b2" (SEltBox (def {_sBox_box = b2})))
    , (2, SEltLabel "l" (SEltLine (def {_sSimpleLine_attachStart = Just (Attachment 0 al1), _sSimpleLine_attachEnd = Just (Attachment 1 al2)})))
    , (3, SEltLabel "lreverse" (SEltLine (def {_sSimpleLine_attachStart = Just (Attachment 1 al2), _sSimpleLine_attachEnd = Just (Attachment 0 al1)})))
    ]

  topfs ot = OwlPFState {
      _owlPFState_owlTree = ot
      , _owlPFState_canvas = SCanvas $ canvasbox
    }

  r = [topfs $ owlTree_fromSEltTree (makestree bp ap) | bp <- boxpairs, ap <- attachmentpairs]


validateTransformMe :: (Eq a, TransformMe a) => a -> Bool
validateTransformMe a =
  (transformMe_rotateLeft . transformMe_rotateRight $ a) == a
  && (transformMe_rotateRight . transformMe_rotateLeft $ a) == a
  && (transformMe_reflectHorizontally . transformMe_reflectHorizontally $ a) == a

spec :: Spec
spec = do
  describe "Lines - internal" $ do
    it "rotateMe" $ do
      let
        somelbx1 = LBox (V2 12 (-2)) (V2 12323 (143))
        somexy1 :: XY = V2 345 21
      validateTransformMe somelbx1 `shouldBe` True
      validateTransformMe somexy1 `shouldBe` True
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
  describe "Lines - rendering" $ it "autorendercase" $ forM_ generateTestCases $ \pfs -> do
    let
      rc = potatoRenderPFState pfs
    putTextLn (renderedCanvasToText rc)
    True `shouldBe` True

    -- TODO write a test such that reversing start/end parts of lines always renders the same thing
    -- (actually, this won't work because rotation messed with whether we go up/down for midpoint stuff)
    -- (you could fix this by keeping a rotation counter flag of course)

{-
  -- TODO DELETE ME
  describe "Lines - rendering" $ do
    let
      pfs = owlpfstate_attachments2
      owltree = _owlPFState_owlTree pfs
      somessline1 = hasOwlElt_test_toSSimpleLine $ hasOwlTree_test_mustFindFirstSuperOwlByName pfs "b1-> <-b4"
      somessline2 = hasOwlElt_test_toSSimpleLine $ hasOwlTree_test_mustFindFirstSuperOwlByName pfs "<-b1 b2->"
      somessline3 = hasOwlElt_test_toSSimpleLine $ hasOwlTree_test_mustFindFirstSuperOwlByName pfs "<-b1 b4->"
      somessline4 = hasOwlElt_test_toSSimpleLine $ hasOwlTree_test_mustFindFirstSuperOwlByName pfs "b1-> b4->"
      somessline5 = hasOwlElt_test_toSSimpleLine $ hasOwlTree_test_mustFindFirstSuperOwlByName pfs "b1-> b2->"
      sd1 = sSimpleLineNewRenderFn somessline1 Nothing
      sd2 = sSimpleLineNewRenderFn somessline2 Nothing
      sd3 = sSimpleLineNewRenderFn somessline3 Nothing
      sd4 = sSimpleLineNewRenderFn somessline4 Nothing
      sd5 = sSimpleLineNewRenderFn somessline5 Nothing


    it "basic" $ do
      forM_ (sEltDrawer_renderToLines owltree sd1) putTextLn
      forM_ (sEltDrawer_renderToLines owltree sd2) putTextLn
      forM_ (sEltDrawer_renderToLines owltree sd3) putTextLn
      forM_ (sEltDrawer_renderToLines owltree sd4) putTextLn
      forM_ (sEltDrawer_renderToLines owltree sd5) putTextLn
      -- TODO test stuff
      --sd (V2 0 0) owltree `shouldBe` Just '<'
      True `shouldBe` True
    -- TODO change order of attachments and render and make sure it's still the same
-}
