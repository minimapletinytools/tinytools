{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Methods.LineDrawer (
  LineAnchorsForRender(..)
  , sSimpleLineNewRenderFn



  -- * exposed for testing
  , CartDir(..)
  , TransformMe(..)
  , determineSeparation
  , lineAnchorsForRender_simplify
) where

import           Relude

import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.Types
import Potato.Flow.Methods.Types
import Potato.Flow.Attachments
import Potato.Flow.Owl

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

-- NOTE our coordinate system is LEFT HANDED
matrix_cw_90 :: M22 Int
matrix_cw_90 = V2 (V2 0 (-1)) (V2 1 0)
matrix_ccw_90 :: M22 Int
matrix_ccw_90 = V2 (V2 0 1) (V2 (-1) 0)

-- TODO rename me so it include reflection
-- TODO rename so it's lower case
class TransformMe a where
  -- CCW
  transformMe_rotateLeft :: a -> a
  transformMe_rotateLeft = transformMe_rotateRight . transformMe_rotateRight . transformMe_rotateRight
  -- CW
  transformMe_rotateRight :: a -> a
  transformMe_rotateRight = transformMe_rotateLeft . transformMe_rotateLeft . transformMe_rotateLeft

  transformMe_reflectHorizontally :: a -> a

instance TransformMe CartDir where
  transformMe_rotateLeft = \case
    CD_Up -> CD_Left
    CD_Down -> CD_Right
    CD_Left -> CD_Down
    CD_Right -> CD_Up
  transformMe_rotateRight = \case
    CD_Up -> CD_Right
    CD_Down -> CD_Left
    CD_Left -> CD_Up
    CD_Right -> CD_Down
  transformMe_reflectHorizontally = \case
    CD_Right -> CD_Left
    CD_Left -> CD_Right
    x -> x

instance TransformMe AnchorType where
  transformMe_rotateLeft = \case
    AT_End_Up -> AT_End_Left
    AT_End_Down -> AT_End_Right
    AT_End_Left -> AT_End_Down
    AT_End_Right -> AT_End_Up
    AT_Elbow_TL -> AT_Elbow_BL
    AT_Elbow_TR -> AT_Elbow_TL
    AT_Elbow_BR -> AT_Elbow_TR
    AT_Elbow_BL -> AT_Elbow_BR
    AT_Elbow_Invalid -> AT_Elbow_Invalid
  transformMe_rotateRight = \case
    AT_End_Up -> AT_End_Right
    AT_End_Down -> AT_End_Left
    AT_End_Left -> AT_End_Up
    AT_End_Right -> AT_End_Down
    AT_Elbow_TL -> AT_Elbow_TR
    AT_Elbow_TR -> AT_Elbow_BR
    AT_Elbow_BR -> AT_Elbow_BL
    AT_Elbow_BL -> AT_Elbow_TL
    AT_Elbow_Invalid -> AT_Elbow_Invalid
  transformMe_reflectHorizontally = \case
    AT_End_Left -> AT_End_Right
    AT_End_Right -> AT_End_Left
    AT_Elbow_TL -> AT_Elbow_TR
    AT_Elbow_TR -> AT_Elbow_TL
    AT_Elbow_BR -> AT_Elbow_BL
    AT_Elbow_BL -> AT_Elbow_BR
    AT_Elbow_Invalid -> AT_Elbow_Invalid

instance TransformMe XY where
  transformMe_rotateLeft p = (!*) matrix_ccw_90 p - (V2 0 1)
  transformMe_rotateRight p = (!*) matrix_cw_90 p - (V2 1 0)
  transformMe_reflectHorizontally (V2 x y) = V2 (-(x+1)) y

instance (TransformMe a, TransformMe b) => TransformMe (a,b) where
  transformMe_rotateLeft (a,b) = (transformMe_rotateLeft a, transformMe_rotateLeft b)
  transformMe_rotateRight (a,b) = (transformMe_rotateRight a, transformMe_rotateRight b)
  transformMe_reflectHorizontally (a,b) = (transformMe_reflectHorizontally a, transformMe_reflectHorizontally b)

-- assumes LBox is Canonical)
instance TransformMe LBox where
  transformMe_rotateLeft lbox@(LBox tl (V2 w h)) = assert (lBox_isCanonicalLBox lbox) r where
    V2 blx bly = (!*) matrix_ccw_90 tl
    r = LBox (V2 blx (bly - w)) (V2 h w)
  transformMe_rotateRight lbox@(LBox tl (V2 w h)) = assert (lBox_isCanonicalLBox lbox) r where
    V2 trx try = (!*) matrix_cw_90 tl
    r = LBox (V2 (trx-h) try) (V2 h w)
  transformMe_reflectHorizontally lbox@(LBox (V2 x y) (V2 w h)) = assert (lBox_isCanonicalLBox lbox) r where
    r = LBox (V2 (-(x+w)) y) (V2 w h)

instance TransformMe AttachmentLocation where
  transformMe_rotateLeft = \case
    AL_Top -> AL_Left
    AL_Bot -> AL_Right
    AL_Left -> AL_Bot
    AL_Right -> AL_Top
    AL_Any -> AL_Any
  transformMe_rotateRight = \case
    AL_Top -> AL_Right
    AL_Bot -> AL_Left
    AL_Left -> AL_Top
    AL_Right -> AL_Bot
    AL_Any -> AL_Any
  transformMe_reflectHorizontally = \case
    AL_Left -> AL_Right
    AL_Right -> AL_Left
    x -> x

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
} deriving (Show)

emptyLineAnchorsForRender :: LineAnchorsForRender
emptyLineAnchorsForRender = LineAnchorsForRender {
  _lineAnchorsForRender_start = 0
  , _lineAnchorsForRender_rest = []
}

lineAnchorsForRender_simplify :: LineAnchorsForRender -> LineAnchorsForRender
lineAnchorsForRender_simplify lafr@LineAnchorsForRender {..} = r where
  -- remove 0 distance lines except at front and back
  withoutzeros = case _lineAnchorsForRender_rest of
    [] -> []
    x:xs -> x:withoutzerosback xs
    where
      withoutzerosback = \case
        [] -> []
        x:[] -> [x]
        (cd, 0):xs -> xs
        x:xs -> x:withoutzerosback xs

  foldrfn (cd, d) [] = [(cd, d)]
  foldrfn (cd, d) ((cd',d'):xs) = if cd == cd'
    then (cd, d+d'):xs
    else (cd,d):(cd',d'):xs
  withoutdoubles = foldr foldrfn [] withoutzeros
  r = LineAnchorsForRender {
      _lineAnchorsForRender_start = _lineAnchorsForRender_start
      , _lineAnchorsForRender_rest = withoutdoubles
    }

lineAnchorsForRender_reverse :: LineAnchorsForRender -> LineAnchorsForRender
lineAnchorsForRender_reverse LineAnchorsForRender {..} = r where
  end = foldl' (\p cdd -> p + cartDirWithDistanceToV2 cdd) _lineAnchorsForRender_start _lineAnchorsForRender_rest
  r = LineAnchorsForRender {
      _lineAnchorsForRender_start = end
      , _lineAnchorsForRender_rest = reverse . fmap (\(cd,d) -> (flipCartDir cd, d)) $ _lineAnchorsForRender_rest
    }

instance TransformMe LineAnchorsForRender where
  transformMe_rotateLeft LineAnchorsForRender {..} = LineAnchorsForRender {
      _lineAnchorsForRender_start = transformMe_rotateLeft _lineAnchorsForRender_start
      ,_lineAnchorsForRender_rest = fmap (\(cd,d) -> (transformMe_rotateLeft cd, d)) _lineAnchorsForRender_rest
    }
  transformMe_rotateRight LineAnchorsForRender {..} = LineAnchorsForRender {
      _lineAnchorsForRender_start = transformMe_rotateRight _lineAnchorsForRender_start
      ,_lineAnchorsForRender_rest = fmap (\(cd,d) -> (transformMe_rotateRight cd, d)) _lineAnchorsForRender_rest
    }
  transformMe_reflectHorizontally LineAnchorsForRender {..} = LineAnchorsForRender {
      _lineAnchorsForRender_start = transformMe_reflectHorizontally _lineAnchorsForRender_start
      ,_lineAnchorsForRender_rest = fmap (\(cd,d) -> (transformMe_reflectHorizontally cd, d)) _lineAnchorsForRender_rest
    }

lineAnchorsForRenderToPointList :: LineAnchorsForRender -> [XY]
lineAnchorsForRenderToPointList LineAnchorsForRender {..} = r where
  scanlfn pos (cd,d) = pos + (cartDirToUnit cd) ^* d
  r = scanl scanlfn _lineAnchorsForRender_start _lineAnchorsForRender_rest

data SimpleLineSolverParameters = SimpleLineSolverParameters {
  _simpleLineSolverParameters_offsetBorder :: Bool -- whether we attach directly to the box or offset by the border (note this still applies just the same for borderless boxes, w/e)
  , _simpleLineSolverParameters_attachOffset :: Int -- cells to offset attach to box by
}

instance TransformMe SimpleLineSolverParameters where
  transformMe_rotateLeft = id
  transformMe_rotateRight = id
  transformMe_reflectHorizontally = id

sSimpleLineSolver :: SimpleLineSolverParameters -> (LBox, AttachmentLocation) -> (LBox, AttachmentLocation) -> LineAnchorsForRender
sSimpleLineSolver sls@SimpleLineSolverParameters {..} lbal1@(lbx1, al1) lbal2@(lbx2, al2) = lineAnchorsForRender_simplify anchors where
  --LBox (V2 x1 y1) (V2 w1 h1) = lbx1
  LBox (V2 _ y2) (V2 _ h2) = lbx2

  start@(V2 ax1 ay1) = attachLocationFromLBox _simpleLineSolverParameters_offsetBorder lbx1 al1
  (V2 ax2 ay2) = attachLocationFromLBox _simpleLineSolverParameters_offsetBorder lbx2 al2


  -- TODO set _simpleLineSolverParameters_attachOffset here
  -- TODO NOTE that line ends can connect directly to each other so you may need to hack the offset or something??
  -- e.g. both ends are offset by 2 but they only need a space of 3 between them
  --   +-*
  --   |
  -- *-+
  -- so I think you need determineSeparationForAttachment (only needed for hsep)
  (hsep, vsep) = determineSeparation (lbx1, (1,1,1,1)) (lbx2, (1,1,1,1))

  lbx1isstrictlyleft = ax1 < ax2
  lbx1isleft = ax1 <= ax2
  lbx1isstrictlyabove = ay1 < ay2
  lbx1isabove = ay1 <= ay2
  ay1isvsepfromlbx2 = ay1 < y2 || ay1 >= y2 + h2

  --traceStep = trace
  traceStep _ x = x

  -- WIP

  (l1_inc,r1,t1_inc,b1) = lBox_to_axis lbx1
  (l2_inc,r2,t2_inc,b2) = lBox_to_axis lbx2

  -- TODO offset by boundaryoffset from parameters
  l = min (l1_inc-1) (l2_inc-1)
  t = min (t1_inc-1) (t2_inc-1)
  b = max b1 b2

  --anchors = trace (show al1 <> " " <> show al2) $ case al1 of
  anchors = case al1 of
    -- WORKING
    -- degenerate case
    AL_Right | ax1 == ax2 && ay1 == ay2 -> LineAnchorsForRender {
        _lineAnchorsForRender_start = start
        , _lineAnchorsForRender_rest = []
      }
    -- WORKING
    -- 1->  <-2
    AL_Right | al2 == AL_Left && lbx1isstrictlyleft && hsep -> traceStep "case 1" $ r where

      halfway = (ax2+ax1) `div` 2
      lb1_to_center = (CD_Right, (halfway-ax1))
      centerverticalline = if ay1 < ay2
        then (CD_Down, ay2-ay1)
        else (CD_Up, ay1-ay2)
      center_to_lb2 = (CD_Right, (ax2-halfway))
      r = LineAnchorsForRender {
          _lineAnchorsForRender_start = start
          , _lineAnchorsForRender_rest = [lb1_to_center, centerverticalline, center_to_lb2]
        }

    -- WORKING
    -- <-2  1->
    AL_Right | al2 == AL_Left && not vsep -> traceStep "case 2" $ r where

      goup = (ay1-t)+(ay2-t) < (b-ay1)+(b-ay2)

      lb1_to_right = (CD_Right, _simpleLineSolverParameters_attachOffset)
      right_to_torb = if goup
        then (CD_Up, ay1-t)
        else (CD_Down, b-ay1)
      torb = (CD_Left, _simpleLineSolverParameters_attachOffset*2 + (ax1-ax2))
      torb_to_left = if goup
        then (CD_Down, ay2-t)
        else (CD_Up, b-ay2)
      left_to_lb2 = (CD_Right, _simpleLineSolverParameters_attachOffset)
      r = LineAnchorsForRender {
          _lineAnchorsForRender_start = start
          , _lineAnchorsForRender_rest = [lb1_to_right, right_to_torb, torb, torb_to_left, left_to_lb2]
        }

    -- WORKING
    -- <-2
    --      1->
    AL_Right | al2 == AL_Left && vsep -> traceStep "case 3" $ r where
      halfway = (ay2+ay1) `div` 2
      lb1_to_right = (CD_Right, _simpleLineSolverParameters_attachOffset)
      right_to_center = if lbx1isstrictlyabove
        then (CD_Down, halfway-ay1)
        else (CD_Up, ay1-halfway)
      center = (CD_Left, _simpleLineSolverParameters_attachOffset*2 + (ax1-ax2))
      center_to_left = if lbx1isstrictlyabove
        then (CD_Down, ay2-halfway)
        else (CD_Up, halfway-ay2)
      left_to_lb2 = (CD_Right, _simpleLineSolverParameters_attachOffset)
      r = LineAnchorsForRender {
          _lineAnchorsForRender_start = start
          , _lineAnchorsForRender_rest = [lb1_to_right, right_to_center, center, center_to_left, left_to_lb2]
        }

    -- WORKING
    -- not vsep is the wrong condition here, we want ay1 to be above or below lbx2
    -- 1->
    --     2->
    AL_Right | al2 == AL_Right && ay1isvsepfromlbx2 -> traceStep "case 4" $ answer where
      rightedge = max r1 r2 + _simpleLineSolverParameters_attachOffset
      lb1_to_right1 = (CD_Right, rightedge-r1)
      right1_to_right2 = if lbx1isstrictlyabove
        then (CD_Down, ay2-ay1)
        else (CD_Up, ay1-ay2)
      right2_to_lb2 = (CD_Left, rightedge-r2)
      answer = LineAnchorsForRender {
          _lineAnchorsForRender_start = start
          , _lineAnchorsForRender_rest = [lb1_to_right1, right1_to_right2, right2_to_lb2]
        }

    -- WORKING
    -- ->1 ->2
    AL_Right | al2 == AL_Right && lbx1isleft && not ay1isvsepfromlbx2 -> traceStep "case 5" $  r where
      t = min (t1_inc-1) (t2_inc-1)
      b = max b1 b2
      goup = (ay1-t)+(ay2-t) < (b-ay1)+(b-ay2)

      -- TODO maybe it would be nice if this traveled a little further right
      lb1_to_right1 = (CD_Right, _simpleLineSolverParameters_attachOffset)

      right1_to_torb = if goup
        then (CD_Up, ay1-t)
        else (CD_Down, b-ay1)
      torb = (CD_Right, r2-r1)
      torb_to_right2 = if goup
        then (CD_Down, ay2-t)
        else (CD_Up, b-ay2)
      right2_to_lb2 = (CD_Left, _simpleLineSolverParameters_attachOffset)
      r = LineAnchorsForRender {
          _lineAnchorsForRender_start = start
          , _lineAnchorsForRender_rest = [lb1_to_right1, right1_to_torb, torb, torb_to_right2, right2_to_lb2]
        }
    -- ->2 ->1 (will not get covered by rotation)
    AL_Right | al2 == AL_Right && not ay1isvsepfromlbx2 -> traceStep "case 6 (flip)" $ lineAnchorsForRender_reverse $ sSimpleLineSolver sls lbal2 lbal1

    -- ^
    -- |
    -- 1
    --     2-> (this only handles lbx1isstrictlyabove case)
    AL_Top | al2 == AL_Right && lbx1isleft && lbx1isstrictlyabove -> traceStep "case 7" $ r where
      upd = if not lbx1isstrictlyabove then min _simpleLineSolverParameters_attachOffset (ay1-ay2) else _simpleLineSolverParameters_attachOffset
      lb1_to_up = (CD_Up, upd)
      up_to_right1 = (CD_Right, (max ax2 r1)-ax1+_simpleLineSolverParameters_attachOffset)
      right1_to_right2 = if lbx1isstrictlyabove
        then (CD_Down, ay2-ay1+upd)
        else (CD_Up, max 0 (ay1-ay2-upd))
      right2_to_lb2 = (CD_Left, _simpleLineSolverParameters_attachOffset)
      r = LineAnchorsForRender {
          _lineAnchorsForRender_start = start
          , _lineAnchorsForRender_rest = [lb1_to_up,up_to_right1,right1_to_right2,right2_to_lb2]
        }

    --     2->
    -- ^
    -- |
    -- 1 (this will get handled by the next case, it might have been better to add a third case for the not lbx1isstrictlyabove case)
    AL_Top | al2 == AL_Right && lbx1isleft -> traceStep "case 8 (flip)" $ lineAnchorsForRender_reverse $ sSimpleLineSolver sls lbal2 lbal1

    --     <-2
    -- ^
    -- |
    -- 1   <-2 (this one handles both lbx1isstrictlyabove cases)
    AL_Top | al2 == AL_Left && lbx1isleft -> traceStep "case 9" $ r where
      topedge = min (ay1 - _simpleLineSolverParameters_attachOffset) ay2
      leftedge = l
      halfway = (ax1 + ax2) `div` 2

      lb1_to_up = (CD_Up, ay1-topedge)
      (up_to_over, up_to_over_xpos) = if lbx1isstrictlyabove && not hsep
        -- go around from the left
        then ((CD_Left, ax1-leftedge), leftedge)
        else ((CD_Right, halfway-ax1), halfway)
      over_to_down = (CD_Down, ay2-topedge)
      down_to_lb2 = (CD_Right, ax2-up_to_over_xpos)
      r = LineAnchorsForRender {
          _lineAnchorsForRender_start = start
          , _lineAnchorsForRender_rest = [lb1_to_up, up_to_over,over_to_down,down_to_lb2]
        }

    --        ^
    --        |
    -- <-2->  1 (will not get covered by rotation)
    AL_Top | al2 == AL_Left || al2 == AL_Right -> traceStep "case 10 (flip)" $  transformMe_reflectHorizontally $ sSimpleLineSolver (transformMe_reflectHorizontally sls) (transformMe_reflectHorizontally lbal1) (transformMe_reflectHorizontally lbal2)

    -- TODO be smarter about how you transform to existing case
    AL_Top | al2 == AL_Any -> traceStep "case 11 (any)" $ sSimpleLineSolver sls lbal1 (lbx2, AL_Left)
    AL_Any | al2 == AL_Top -> traceStep "case 12 (any)" $ sSimpleLineSolver sls (lbx1, AL_Right) lbal2

    AL_Any | al2 == AL_Any -> traceStep "case 13 (any)" $ sSimpleLineSolver sls (lbx1, AL_Right) (lbx2, AL_Left)

    _ -> traceStep "case 14 (rotate)" $ transformMe_rotateRight $ sSimpleLineSolver (transformMe_rotateLeft sls) (transformMe_rotateLeft lbal1) (transformMe_rotateLeft lbal2)


doesLineContain :: XY -> XY -> (CartDir, Int) -> Maybe Int
doesLineContain (V2 px py) (V2 sx sy) (tcd, tl) = case tcd of
  CD_Left | py == sy -> if px <= sx && px >= sx-tl then Just (sx-px) else Nothing
  CD_Right | py == sy -> if px >= sx && px <= sx+tl then Just (px-sx) else Nothing
  CD_Up | px == sx -> if py <= sy && py >= sy-tl then Just (sy-py) else Nothing
  CD_Down | px == sx -> if py >= sy && py <= sy+tl then Just (py-sy) else Nothing
  _ -> Nothing

walkToRender :: Bool -> XY -> (CartDir, Int) -> Maybe (CartDir, Int) -> Int -> (XY, MPChar)
walkToRender isstart begin (tcd, tl) mnext d = r where
  currentpos = begin + (cartDirToUnit tcd) ^* d

  -- TODO set these correctly
  ss@SuperStyle {..} = def
  ls@LineStyle {..} = def

  endorelbow = renderAnchorType ss ls $ cartDirToAnchor tcd (fmap fst mnext)
  startorregular = if isstart
    then if d <= tl `div` 2
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


-- DELETE not what you need
mapAccumFind :: (a -> b -> (a, Maybe c)) -> a -> [b] -> Maybe c
mapAccumFind f acc l = case l of
  [] -> Nothing
  x:xs -> case f acc x of
    (nacc, Nothing) -> mapAccumFind f nacc xs
    (_, manswer) -> manswer

-- VERY UNTESTED
lineAnchorsForRender_renderAt :: LineAnchorsForRender -> XY -> MPChar
lineAnchorsForRender_renderAt LineAnchorsForRender {..} pos = r where
  walk (isstart, curbegin) ls = case ls of
    [] -> Nothing
    x:xs -> case doesLineContain pos curbegin x of
      Nothing ->  walk (False, nextbegin) xs
      Just d -> Just $ case xs of
        [] -> walkToRender isstart curbegin x Nothing d
        y:ys -> walkToRender isstart curbegin x (Just y) d
      where
        nextbegin = curbegin + cartDirWithDistanceToV2 x

  manswer = walk (True, _lineAnchorsForRender_start) _lineAnchorsForRender_rest
  r = case manswer of
    Nothing -> Nothing
    Just (pos', mpchar) -> assert (pos == pos') mpchar

sSimpleLineNewRenderFn :: SSimpleLine -> Maybe LineAnchorsForRender -> SEltDrawer
sSimpleLineNewRenderFn ssline@SSimpleLine {..} mcache = drawer where
  params = SimpleLineSolverParameters {
      _simpleLineSolverParameters_offsetBorder = True
      -- TODO maybe set this based on arrow head size (will differ for each end so you need 4x)
      , _simpleLineSolverParameters_attachOffset = 1
    }

  -- inefficient since this gets evaled both for renderfn and boxfn
  -- TODO figure out a way to do this smarter (either generate render/boxfn at the same time and/or use RenderCache)
  getAnchors :: (HasOwlTree a) => a -> LineAnchorsForRender
  --getAnchors ot = traceShowId $ anchors where
  getAnchors ot = anchors where
    maybeGetBox mattachment = do
      Attachment rid al <- mattachment
      sowl <- hasOwlTree_findSuperOwl ot rid
      sbox <- getSEltBox $ hasOwlElt_toSElt_hack sowl
      return (sbox, al)

    lbal1 = fromMaybe (LBox _sSimpleLine_start 0, AL_Any) $ maybeGetBox _sSimpleLine_attachStart
    lbal2 = fromMaybe (LBox _sSimpleLine_end 0, AL_Any) $ maybeGetBox _sSimpleLine_attachEnd

    -- NOTE for some reason sticking trace statements in sSimpleLineSolver will causes regenanchors to get called infinite times :(
    regenanchors = sSimpleLineSolver params lbal1 lbal2

    anchors = case mcache of
      Just x -> x
      Nothing -> regenanchors

  renderfn :: SEltDrawerRenderFn
  renderfn ot xy = r where
    anchors = getAnchors ot

    r = lineAnchorsForRender_renderAt anchors xy

  boxfn :: SEltDrawerBoxFn
  boxfn ot = case nonEmpty (lineAnchorsForRenderToPointList (getAnchors ot)) of
    Nothing -> LBox 0 0
    -- add_XY_to_lBox is non-inclusive with bottom/right so we expand by 1 to make it inclusive
    Just (x :| xs) -> lBox_expand (foldl' (flip add_XY_to_lBox) (make_0area_lBox_from_XY x) xs) (0,1,0,1)



  drawer = SEltDrawer {
      _sEltDrawer_box = boxfn
      , _sEltDrawer_renderFn = renderfn
    }
