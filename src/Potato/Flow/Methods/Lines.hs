{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Methods.Lines (

) where

import           Relude

import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.Types
import Potato.Flow.Methods.Types

import qualified Data.Map as Map
import           Data.Dependent.Sum (DSum ((:=>)), (==>))
import           Data.Maybe         (fromJust)
import qualified Data.Text          as T
import qualified Potato.Data.Text.Zipper   as TZ

-- cases
-- '⇇' '⇇'
-- '⇇' '⇉'
-- '⇉' '⇇'
-- '⇇' '⇊'
-- '⇇' '⇈'
-- '⇉' '⇊'


data CartDir = CD_Up | CD_Down | CD_Left | CD_Right
data AnchorType = AT_End_Up | AT_End_Down | AT_End_Left | AT_End_Right | AT_Elbow_UR | AT_Elbow_RD | AT_Elbow_DL | AT_Elbow_LU

cartDirToUnit :: CartDir -> XY
cartDirToUnit = \case
  CD_Up -> V2 0 (-1)
  CD_Down -> V2 0 1
  CD_Left -> V2 (-1) 0
  CD_Right -> V2 1 0

data LineAnchorsForRender = LineAnchorsForRender {
  _lineAnchorsForRender_start :: XY
  , _lineAnchorsForRender_rest :: [(CartDir, Int)]
}

--foldLineAnchors :: (a -> )

-- TODO
lineAnchorsForRenderToPointList :: LineAnchorsForRender -> [XY]
lineAnchorsForRenderToPointList LineAnchorsForRender {..} = r where
  rest = undefined
  r = _lineAnchorsForRender_start : rest

data SimpleLineSolverParameters = SimpleLineSolverParameters {
  _simpleLineSolverParameters_offsetBorder :: Bool -- whether we attach directly to the box or offset by the border (note this still applies just the same for borderless boxes, w/e)
  , _simpleLineSolverParameters_attachOffset :: Int -- cells to offset attach to box by
}

-- TODO
sSimpleLineSolver :: SimpleLineSolverParameters -> (LBox, AttachmentLocation) -> (LBox, AttachmentLocation) -> LineAnchorsForRender
sSimpleLineSolver SimpleLineSolverParameters {..} (lbx1, al1) (lbx2, al2) = r where
  LBox (V2 x1 y1) (V2 w1 h1) = lbx1
  LBox (V2 x2 y2) (V2 w2 h2) = lbx2
  r = undefined



sSimpleLineNewRenderFn :: LineStyle -> LineAnchorsForRender -> SEltDrawer
sSimpleLineNewRenderFn LineStyle {..} anchors = r where
  box = case nonEmpty (lineAnchorsForRenderToPointList anchors) of
    Nothing -> LBox 0 0
    Just (x :| xs) -> foldr add_XY_to_LBox (make_lBox_from_XY x) xs

  r = SEltDrawer {
      _sEltDrawer_box = box
      , _sEltDrawer_renderFn = undefined
    }
