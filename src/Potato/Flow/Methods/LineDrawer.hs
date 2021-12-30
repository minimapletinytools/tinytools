{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Methods.LineDrawer (

  -- * exposed for testing
  determineSeparation
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
import Data.Default

import Linear.Vector ((^*))
import Linear.Matrix (M22, (!*))

-- cases
-- '⇇' '⇇'
-- '⇇' '⇉'
-- '⇉' '⇇'
-- '⇇' '⇊'
-- '⇇' '⇈'
-- '⇉' '⇊'

determineSeparation :: (LBox, (Int, Int, Int, Int)) -> (LBox, (Int, Int, Int, Int)) -> (Bool, Bool)
determineSeparation (lbx1, p1) (lbx2, p2) = r where
  (l1,r1,t1,b1) = lBox_to_axis $ lBox_expand lbx1 p1
  (l2,r2,t2,b2) = lBox_to_axis $ lBox_expand lbx2 p2
  hsep = l1 >= r2 || l2 >= r1
  vsep = t1 >= b2 || t2 >= b1
  r = (hsep, vsep)


matrix_cw_90 :: M22 Int
matrix_cw_90 = V2 (V2 0 (-1)) (V2 1 0)
matrix_ccw_90 :: M22 Int
matrix_ccw_90 = V2 (V2 0 1) (V2 (-1) 0)

-- TODO rename so it's lower case
class RotateMe a where
  -- CCW
  rotateMe_Left :: a -> a
  -- CW
  rotateMe_Right :: a -> a
  rotateMe_validate :: (Eq a) => a -> Bool
  rotateMe_validate a = (rotateMe_Left . rotateMe_Right $ a) == a && (rotateMe_Right . rotateMe_Left $ a) == a

instance RotateMe CartDir where
  rotateMe_Left = \case
    CD_Up -> CD_Left
    CD_Down -> CD_Right
    CD_Left -> CD_Down
    CD_Right -> CD_Up
  rotateMe_Right = \case
    CD_Up -> CD_Right
    CD_Down -> CD_Left
    CD_Left -> CD_Up
    CD_Right -> CD_Down

instance RotateMe AnchorType where
  rotateMe_Left = \case
    AT_End_Up -> AT_End_Left
    AT_End_Down -> AT_End_Right
    AT_End_Left -> AT_End_Down
    AT_End_Right -> AT_End_Up
    AT_Elbow_TL -> AT_Elbow_BL
    AT_Elbow_TR -> AT_Elbow_TL
    AT_Elbow_BR -> AT_Elbow_TR
    AT_Elbow_BL -> AT_Elbow_BR
    AT_Elbow_Invalid -> AT_Elbow_Invalid
  rotateMe_Right = \case
    AT_End_Up -> AT_End_Right
    AT_End_Down -> AT_End_Left
    AT_End_Left -> AT_End_Up
    AT_End_Right -> AT_End_Down
    AT_Elbow_TL -> AT_Elbow_TR
    AT_Elbow_TR -> AT_Elbow_BR
    AT_Elbow_BR -> AT_Elbow_BL
    AT_Elbow_BL -> AT_Elbow_TL
    AT_Elbow_Invalid -> AT_Elbow_Invalid

instance RotateMe XY where
  rotateMe_Left = (!*) matrix_ccw_90
  rotateMe_Right = (!*) matrix_cw_90

-- assumes LBox is Canonical)
instance RotateMe LBox where
  rotateMe_Left lbox@(LBox tl (V2 w h))) = assert (lBox_isCanonicalLBox lbox) r where
    (blx, bly) = rotateMe_Left tl
    r = LBox (V2 blx (bly - w)) (V2 h w)
  rotateMe_Right lbox@(LBox tl (V2 w h))) = assert (lBox_isCanonicalLBox lbox) r where
    (trx, try) = rotateMe_Right tl
    r = LBox (V2 (trx-h) try) (V2 h w) 

instance RotateMe AttachmentLocation where
  rotateMe_Left = \case
    AL_TOP -> AL_LEFT
    AL_BOT -> AL_RIGHT
    AL_LEFT -> AL_BOT
    AL_RIGHT -> AL_TOP
  rotateMe_Left = \case
      AL_TOP -> AL_RIGHT
      AL_BOT -> AL_LEFT
      AL_LEFT -> AL_TOP
      AL_RIGHT -> AL_BOT

data CartDir = CD_Up | CD_Down | CD_Left | CD_Right
data AnchorType = AT_End_Up | AT_End_Down | AT_End_Left | AT_End_Right | AT_Elbow_TL | AT_Elbow_TR | AT_Elbow_BR | AT_Elbow_BL | AT_Elbow_Invalid

maybeIndex :: Text -> Int -> Maybe MPChar
maybeIndex t i = if i < T.length t
  then Just $ (Just $ T.index t i)
  else Nothing

renderLine :: SuperStyle -> CartDir -> MPChar
renderLine SuperStyle {..} cd = case cd of
  CD_Up -> _superStyle_vertical
  CD_Down -> _superStyle_vertical
  CD_Left -> _superStyle_horizontal
  CD_Right -> _superStyle_horizontal

renderLineEnd :: SuperStyle -> LineStyle -> CartDir -> Int -> MPChar
renderLineEnd SuperStyle {..} LineStyle {..} cd distancefromend = r where
  r = case cd of
    CD_Up -> fromMaybe _superStyle_vertical $ maybeIndex _lineStyle_upArrows distancefromend
    CD_Down -> fromMaybe _superStyle_vertical $ maybeIndex (T.reverse _lineStyle_downArrows) distancefromend
    CD_Left -> fromMaybe _superStyle_horizontal $ maybeIndex _lineStyle_leftArrows distancefromend
    CD_Right -> fromMaybe _superStyle_horizontal $ maybeIndex (T.reverse _lineStyle_rightArrows) distancefromend


renderAnchorType :: SuperStyle -> LineStyle -> AnchorType -> MPChar
renderAnchorType ss@SuperStyle {..} ls@LineStyle {..} at = r where
  r = case at of
    AT_End_Up -> renderLineEnd ss ls CD_Up 0
    AT_End_Down -> renderLineEnd ss ls CD_Down 0
    AT_End_Left -> renderLineEnd ss ls CD_Left 0
    AT_End_Right -> renderLineEnd ss ls CD_Right 0
    AT_Elbow_TL -> _superStyle_tl
    AT_Elbow_TR -> _superStyle_tr
    AT_Elbow_BR -> _superStyle_br
    AT_Elbow_BL -> _superStyle_bl
    AT_Elbow_Invalid -> Just '?'

flipCartDir :: CartDir -> CartDir
flipCartDir = \case
  CD_Up -> CD_Down
  CD_Down -> CD_Up
  CD_Left -> CD_Right
  CD_Right -> CD_Left

cartDirToUnit :: CartDir -> XY
cartDirToUnit = \case
  CD_Up -> V2 0 (-1)
  CD_Down -> V2 0 1
  CD_Left -> V2 (-1) 0
  CD_Right -> V2 1 0

cartDirToAnchor :: CartDir -> Maybe CartDir -> AnchorType
cartDirToAnchor start mnext = case mnext of
  Nothing -> case start of
    CD_Up -> AT_End_Up
    CD_Down -> AT_End_Down
    CD_Left -> AT_End_Left
    CD_Right -> AT_End_Right
  Just next -> case start of
    CD_Up -> case next of
      CD_Left -> AT_Elbow_TR
      CD_Right -> AT_Elbow_TL
      _ -> AT_Elbow_Invalid
    CD_Down -> case next of
      CD_Left -> AT_Elbow_BR
      CD_Right -> AT_Elbow_BL
      _ -> AT_Elbow_Invalid
    CD_Left -> case next of
      CD_Up -> AT_Elbow_BL
      CD_Down -> AT_Elbow_TL
      _ -> AT_Elbow_Invalid
    CD_Right -> case next of
      CD_Up -> AT_Elbow_BR
      CD_Down -> AT_Elbow_TR
      _ -> AT_Elbow_Invalid


data LineAnchorsForRender = LineAnchorsForRender {
  _lineAnchorsForRender_start :: XY
  , _lineAnchorsForRender_rest :: [(CartDir, Int)]
}


instance RotateMe LineAnchorsForRender where
  rotateMe_Left LineAnchorsForRender {..} = LineAnchorsForRender {
      _lineAnchorsForRender_start = rotateMe_Left _lineAnchorsForRender_start
      ,_lineAnchorsForRender_rest = fmap (\(cd,d) -> (rotateMe_Left cd, d)) _lineAnchorsForRender_rest
    }
  rotateMe_Right LineAnchorsForRender {..} = LineAnchorsForRender {
      _lineAnchorsForRender_start = rotateMe_Right _lineAnchorsForRender_start
      ,_lineAnchorsForRender_rest = fmap (\(cd,d) -> (rotateMe_Right cd, d)) _lineAnchorsForRender_rest
    }

-- TODO
lineAnchorsForRenderToPointList :: LineAnchorsForRender -> [XY]
lineAnchorsForRenderToPointList LineAnchorsForRender {..} = r where
  rest = undefined
  r = _lineAnchorsForRender_start : rest

data SimpleLineSolverParameters = SimpleLineSolverParameters {
  _simpleLineSolverParameters_offsetBorder :: Bool -- whether we attach directly to the box or offset by the border (note this still applies just the same for borderless boxes, w/e)
  , _simpleLineSolverParameters_attachOffset :: Int -- cells to offset attach to box by
}

instance RotateMe SimpleLineSolverParameters where
  rotateMe_Left = id
  rotateMe_Right = id

-- TODO
sSimpleLineSolver :: SimpleLineSolverParameters -> (LBox, AttachmentLocation) -> (LBox, AttachmentLocation) -> LineAnchorsForRender
sSimpleLineSolver sls@SimpleLineSolverParameters {..} (lbx1, al1) (lbx2, al2) = r where
  LBox (V2 x1 y1) (V2 w1 h1) = lbx1
  LBox (V2 x2 y2) (V2 w2 h2) = lbx2
  (hsep, vsep) = determineSeparation (lbx1, (1,1,1,1)) (lbx2, (1,1,1,1))
  lbx1isleft = x1 < x2 
  r = undefined

  -- determine which case we are in
  -- rotate to standard case
  -- translate solution back to original




  -- WIP


  something = case al1 of
    -- 1->  <-2
    AL_RIGHT | bl2 == AL_LEFT && lbx1isleft && hsep -> undefined 
    -- <-2  1-> 
    AL_RIGHT | bl2 == AL_LEFT && not vsep -> undefined 
    -- <-2  
    --      1-> 
    AL_RIGHT | bl2 == AL_LEFT && vsep -> undefined 
    -- 
    AL_RIGHT | bl2 == AL_RIGHT && not vsep -> undefined
  
    _ -> rotateMe_Right $ sSimpleLineSolver (rotateMe_Left sls) (rotateMe_Left lbx1, rotateMe_Left al1) (rotateMe_Left lbx1 2, rotateMe_Left al2)





walkToRender :: Bool -> XY -> (CartDir, Int) -> Maybe (CartDir, Int) -> Int -> (XY, MPChar)
walkToRender isstart begin (tcd, tl) mnext d = r where
  currentpos = begin + (cartDirToUnit tcd) ^* d
  ss@SuperStyle {..} = def
  ls@LineStyle {..} = def
  endorelbow = renderAnchorType ss ls $ cartDirToAnchor tcd (fmap fst mnext)
  startorregular = if isstart
    then if d < tl `div` 2
      -- if we are at the start and near the beginning then render start of line
      then renderLineEnd ss ls (flipCartDir tcd) d
      else if isNothing mnext
        -- if we are not at the start and at the end then render end of line
        then renderLineEnd ss ls tcd (tl-d)
        -- otherwise render line as usual
        else renderLine ss tcd
    else renderLine ss tcd
  r = if d == tl
    then (currentpos, endorelbow)
    else (currentpos, startorregular)





sSimpleLineNewRenderFn :: LineStyle -> LineAnchorsForRender -> SEltDrawer
sSimpleLineNewRenderFn LineStyle {..} anchors = r where
  box = case nonEmpty (lineAnchorsForRenderToPointList anchors) of
    Nothing -> LBox 0 0
    Just (x :| xs) -> foldr add_XY_to_LBox (make_lBox_from_XY x) xs

  r = SEltDrawer {
      _sEltDrawer_box = box
      , _sEltDrawer_renderFn = undefined
    }
