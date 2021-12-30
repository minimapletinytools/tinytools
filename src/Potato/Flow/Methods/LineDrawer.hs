{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Methods.LineDrawer (
  LineAnchorsForRender(..)



  -- * exposed for testing
  , CartDir(..)
  , RotateMe(..)
  , determineSeparation
  , lineAnchorsForRender_simplify
) where

import           Relude

import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.Types
import Potato.Flow.Methods.Types
import Potato.Flow.Attachments

import qualified Data.Map as Map
import           Data.Dependent.Sum (DSum ((:=>)), (==>))
import           Data.Maybe         (fromJust)
import qualified Data.Text          as T
import qualified Potato.Data.Text.Zipper   as TZ
import Data.Default

import Linear.Vector ((^*))
import Linear.Matrix (M22, (!*))

import Control.Exception (assert)

-- cases
-- '⇇' '⇇'
-- '⇇' '⇉'
-- '⇉' '⇇'
-- '⇇' '⇊'
-- '⇇' '⇈'
-- '⇉' '⇊'

-- TODO I think you need notion of half separation?
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
  rotateMe_Left lbox@(LBox tl (V2 w h)) = assert (lBox_isCanonicalLBox lbox) r where
    V2 blx bly = rotateMe_Left tl
    r = LBox (V2 blx (bly - w)) (V2 h w)
  rotateMe_Right lbox@(LBox tl (V2 w h)) = assert (lBox_isCanonicalLBox lbox) r where
    V2 trx try = rotateMe_Right tl
    r = LBox (V2 (trx-h) try) (V2 h w)

instance RotateMe AttachmentLocation where
  rotateMe_Left = \case
    AL_TOP -> AL_LEFT
    AL_BOT -> AL_RIGHT
    AL_LEFT -> AL_BOT
    AL_RIGHT -> AL_TOP
  rotateMe_Right = \case
      AL_TOP -> AL_RIGHT
      AL_BOT -> AL_LEFT
      AL_LEFT -> AL_TOP
      AL_RIGHT -> AL_BOT

data CartDir = CD_Up | CD_Down | CD_Left | CD_Right deriving (Eq, Show)
data AnchorType = AT_End_Up | AT_End_Down | AT_End_Left | AT_End_Right | AT_Elbow_TL | AT_Elbow_TR | AT_Elbow_BR | AT_Elbow_BL | AT_Elbow_Invalid deriving (Eq, Show)

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

cartDirWithDistanceToV2 :: (CartDir, Int) -> V2 Int
cartDirWithDistanceToV2 (cd, d) = cartDirToUnit cd ^* d


data LineAnchorsForRender = LineAnchorsForRender {
  _lineAnchorsForRender_start :: XY
  , _lineAnchorsForRender_rest :: [(CartDir, Int)]
}

lineAnchorsForRender_simplify :: LineAnchorsForRender -> LineAnchorsForRender
lineAnchorsForRender_simplify LineAnchorsForRender {..} = r where
  foldrfn (cd, 0) acc = acc
  foldrfn (cd, d) [] = [(cd, d)]
  foldrfn (cd, d) ((cd',d'):xs) = if cd == cd'
    then (cd, d'+d):xs
    else (cd,d):(cd',d'):xs
  r = LineAnchorsForRender {
      _lineAnchorsForRender_start = _lineAnchorsForRender_start
      , _lineAnchorsForRender_rest = foldr foldrfn [] _lineAnchorsForRender_rest
    }

lineAnchorsForRender_flip :: LineAnchorsForRender -> LineAnchorsForRender
lineAnchorsForRender_flip LineAnchorsForRender {..} = r where
  end = foldl' (\p cdd -> p + cartDirWithDistanceToV2 cdd) _lineAnchorsForRender_start _lineAnchorsForRender_rest
  r = LineAnchorsForRender {
      _lineAnchorsForRender_start = end
      , _lineAnchorsForRender_rest = reverse . fmap (\(cd,d) -> (flipCartDir cd, d)) $ _lineAnchorsForRender_rest
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
sSimpleLineSolver sls@SimpleLineSolverParameters {..} lbal1@(lbx1, al1) lbal2@(lbx2, al2) = r where
  LBox (V2 x1 y1) (V2 w1 h1) = lbx1
  LBox (V2 x2 y2) (V2 w2 h2) = lbx2

  -- TODO set _simpleLineSolverParameters_attachOffset here
  -- TODO NOTE that line ends can connect directly to each other so you may need to hack the offset or something??
  -- e.g. both ends are offset by 2 but they only need a space of 3 between them
  --   +-*
  --   |
  -- *-+
  -- so I think you need determineSeparationForAttachment (only needed for hsep)
  (hsep, vsep) = determineSeparation (lbx1, (1,1,1,1)) (lbx2, (1,1,1,1))
  lbx1isleft = x1 < x2
  r = undefined

  -- determine which case we are in
  -- rotate to standard case
  -- translate solution back to original




  -- WIP

  something = case al1 of
    -- 1->  <-2
    AL_RIGHT | al2 == AL_LEFT && lbx1isleft && hsep -> r where
      start@(V2 r1 y1) = attachLocationFromLBox _simpleLineSolverParameters_offsetBorder lbx1 al1
      (V2 l2 y2) = attachLocationFromLBox _simpleLineSolverParameters_offsetBorder lbx2 al2
      halfway = (l2-r1) `div` 2
      lb1_to_center = (CD_Right, (halfway-r1))
      centerverticalline = if y1 < y2
        then (CD_Up, y2-y1)
        else (CD_Down, y1-y2)
      center_to_lb2 = (CD_Right, (l2-halfway))
      r = LineAnchorsForRender {
          _lineAnchorsForRender_start = start
          , _lineAnchorsForRender_rest = [lb1_to_center, centerverticalline, center_to_lb2]
        }
    -- <-2  1->
    AL_RIGHT | al2 == AL_LEFT && not vsep -> r where
      start@(V2 r1 y1) = attachLocationFromLBox _simpleLineSolverParameters_offsetBorder lbx1 al1
      (V2 l2 y2) = attachLocationFromLBox _simpleLineSolverParameters_offsetBorder lbx2 al2
      (_,_,t1_inc, b1) = lBox_to_axis lbx1
      (_,_,t2_inc, b2) = lBox_to_axis lbx2
      t = min (t1_inc-1) (t2_inc-1)
      b = max b1 b2
      goup = (y1-t)+(y2-t) < (b-y1)+(b-y2)

      lb1_to_right = (CD_Right, _simpleLineSolverParameters_attachOffset)
      right_to_torb = if goup
        then (CD_Up, y1-t)
        else (CD_Down, b-y1)
      torb = (CD_Left, _simpleLineSolverParameters_attachOffset*2 + (r1-l2))
      torb_to_left = if goup
        then (CD_Down, y2-t)
        else (CD_Up, b-y2)
      left_to_lb2 = (CD_Right, _simpleLineSolverParameters_attachOffset)
      r = LineAnchorsForRender {
          _lineAnchorsForRender_start = start
          , _lineAnchorsForRender_rest = [lb1_to_right, right_to_torb, torb, torb_to_left, left_to_lb2]
        }
    -- <-2
    --      1->
    AL_RIGHT | al2 == AL_LEFT && vsep -> undefined
    -- ->1
    --     ->2
    AL_RIGHT | al2 == AL_RIGHT && vsep -> undefined
    -- ->1 ->2
    AL_RIGHT | al2 == AL_RIGHT && lbx1isleft && not vsep -> undefined
    -- ->2 ->1 (will not get covered by rotation)
    AL_RIGHT | al2 == AL_RIGHT && not vsep -> lineAnchorsForRender_flip $ sSimpleLineSolver sls lbal2 lbal1

    -- TODO add restrictions
    -- 1
    -- ^
    -- |   ->2
    AL_TOP | al2 == AL_RIGHT -> undefined
    -- TODO more elbow cases

    --      1
    --      ^
    -- 2<-  | (will not get covered by rotation)
    AL_TOP | al2 == AL_LEFT -> lineAnchorsForRender_flip $ sSimpleLineSolver sls lbal2 lbal1


    _ -> rotateMe_Right $ sSimpleLineSolver (rotateMe_Left sls) (rotateMe_Left lbx1, rotateMe_Left al1) (rotateMe_Left lbx2, rotateMe_Left al2)





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
