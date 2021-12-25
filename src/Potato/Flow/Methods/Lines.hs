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

-- TODO
sSimpleLineSolver :: (LBox, AttachmentLocation) -> (LBox, AttachmentLocation) -> [XY]
sSimpleLineSolver = undefined

sSimpleLineNewRenderFn :: LineStyle -> LineAnchorsForRender -> SEltDrawer
sSimpleLineNewRenderFn LineStyle {..} anchors = r where
  box = case nonEmpty (lineAnchorsForRenderToPointList anchors) of
    Nothing -> LBox 0 0
    Just (x :| xs) -> foldr add_XY_to_LBox (make_lBox_from_XY x) xs

  r = SEltDrawer {
      _sEltDrawer_box = box
      , _sEltDrawer_renderFn = undefined
    }
