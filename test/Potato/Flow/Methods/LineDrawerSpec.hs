{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Methods.LineDrawerSpec(
  spec
) where

import           Relude           hiding (empty, fromList)

import           Test.Hspec


import Potato.Flow
import           Potato.Flow.Methods.LineDrawer
import           Potato.Flow.TestStates



import Data.Default
import Data.Tuple.Extra

generateTestCases :: [OwlPFState]
generateTestCases = r where

  -- MODIFY THESE TO TEST WHAT YOU NEED TO TEST :O
  al1s = [AL_Left, AL_Right, AL_Top, AL_Bot, AL_Any]
  al2s = [AL_Left, AL_Right, AL_Top, AL_Bot, AL_Any]
  --al1s = [AL_Top]
  --al2s = [AL_Bot]
  box1s = [LBox (V2 0 10) 5]
  box2s = [LBox (V2 5 8) 3]
  canvasbox = LBox (-5) (V2 25 25)


  boxpairs = [(b1,b2) | b1 <- box1s, b2 <- box2s]
  attachmentpairs = [(al1,al2) | al1 <- al1s, al2 <- al2s]



  makestree (b1,b2) (al1, al2) =
    [ (0, SEltLabel "b1" (SEltBox (def {_sBox_box = b1})))
    , (1, SEltLabel "b2" (SEltBox (def {_sBox_box = b2})))
    , (2, SEltLabel "l" (SEltLine (def {_sAutoLine_attachStart = Just (Attachment 0 al1), _sAutoLine_attachEnd = Just (Attachment 1 al2)})))
    --, (3, SEltLabel "lreverse" (SEltLine (def {_sAutoLine_attachStart = Just (Attachment 1 al2), _sAutoLine_attachEnd = Just (Attachment 0 al1)})))
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


emptyLineAnchorsForRender :: LineAnchorsForRender
emptyLineAnchorsForRender = LineAnchorsForRender {
    _lineAnchorsForRender_start = V2 (-123) 45
    , _lineAnchorsForRender_rest = []
  }

unsimplifiedLineAnchorsForRender :: LineAnchorsForRender
unsimplifiedLineAnchorsForRender = LineAnchorsForRender {
    _lineAnchorsForRender_start = 0
    , _lineAnchorsForRender_rest = [(CD_Up, 10, True),(CD_Up, 15, False),(CD_Up, 1, False),(CD_Right, 10, False)]
  }

someLineAnchorsForRender :: LineAnchorsForRender
someLineAnchorsForRender = LineAnchorsForRender {
    _lineAnchorsForRender_start = 0
    , _lineAnchorsForRender_rest = [(CD_Up, 10, True),(CD_Right, 15, False),(CD_Down, 1, False),(CD_Left, 10, False)]
  }


someSAutoLine_withLabels_label3 :: SAutoLineLabel
someSAutoLine_withLabels_label3 = SAutoLineLabel 0 (SAutoLineLabelPositionRelative 0) ""

someSAutoLine_withLabels :: SAutoLine
someSAutoLine_withLabels = def {
      _sAutoLine_start = 0
      , _sAutoLine_end = V2 100 0
      , _sAutoLine_midpoints = [SAutoLineConstraintFixed (V2 50 0)]
      , _sAutoLine_labels = [
          SAutoLineLabel 1 (SAutoLineLabelPositionRelative 0) ""
          , SAutoLineLabel 0 (SAutoLineLabelPositionRelative 0.5) ""
          , someSAutoLine_withLabels_label3
        ]
  }


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
      _lineAnchorsForRender_rest (lineAnchorsForRender_simplify unsimplifiedLineAnchorsForRender) `shouldBe` [(CD_Up, 26, True),(CD_Right, 10, False)]
      lineAnchorsForRender_simplify someLineAnchorsForRender `shouldBe` someLineAnchorsForRender
    it "lineAnchorsForRender_length" $ do
      lineAnchorsForRender_length unsimplifiedLineAnchorsForRender `shouldBe` 36 + 1
      lineAnchorsForRender_length someLineAnchorsForRender `shouldBe` 36 + 1
      lineAnchorsForRender_length emptyLineAnchorsForRender `shouldBe` 1
    it "internal_getSAutoLineLabelPosition_walk" $ do
      let totall = lineAnchorsForRender_length someLineAnchorsForRender
      internal_getSAutoLineLabelPosition_walk someLineAnchorsForRender 0 totall `shouldBe` 0
      internal_getSAutoLineLabelPosition_walk someLineAnchorsForRender totall totall `shouldBe` V2 5 (-9)
    it "getSAutoLineLabelPosition" $ do
      -- use owlpfstate_zero OK because there are no attachments so state is never read
      getSAutoLineLabelPosition owlpfstate_zero someSAutoLine_withLabels someSAutoLine_withLabels_label3 `shouldBe` V2 0 0
    it "getSortedSAutoLineLabelPositions" $ do
      -- use owlpfstate_zero OK because there are no attachments so state is never read
      fmap fst3 (getSortedSAutoLineLabelPositions owlpfstate_zero someSAutoLine_withLabels) `shouldBe` [V2 0 0, V2 25 0, V2 50 0]


  describe "Lines - rendering" $ it "autorendercase" $ forM_ generateTestCases $ \pfs -> do
    --putTextLn (renderedCanvasToText (potatoRenderPFState pfs))
    True `shouldBe` True

    -- TODO write a test such that reversing start/end parts of lines always renders the same thing
    -- (actually, this won't work because rotation messed with whether we go up/down for midpoint stuff)
    -- (you could fix this by keeping a rotation counter flag of course)
