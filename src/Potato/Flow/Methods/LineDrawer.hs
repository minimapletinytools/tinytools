
{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Methods.LineDrawer (
  LineAnchorsForRender(..)
  , lineAnchorsForRender_doesIntersectPoint
  , lineAnchorsForRender_doesIntersectBox
  , lineAnchorsForRender_findIntersectingSubsegment
  , lineAnchorsForRender_length

  , sAutoLine_to_lineAnchorsForRenderList
  , sSimpleLineNewRenderFn
  , sSimpleLineNewRenderFnComputeCache

  , getSAutoLineLabelPosition
  , getSAutoLineLabelPositionFromLineAnchorsForRender
  , getSortedSAutoLineLabelPositions
  , getClosestPointOnLineFromLineAnchorsForRenderList



  -- * exposed for testing
  , CartDir(..)
  , TransformMe(..)
  , determineSeparation
  , lineAnchorsForRender_simplify
  , internal_getSAutoLineLabelPosition_walk
) where


import           Relude                         hiding (tail)
import           Relude.Unsafe                  (tail)

import           Potato.Flow.Attachments
import           Potato.Flow.Math
import           Potato.Flow.Methods.LineTypes
import           Potato.Flow.Methods.TextCommon
import           Potato.Flow.Methods.Types
import           Potato.Flow.Owl
import           Potato.Flow.OwlItem
import           Potato.Flow.Serialization.Snake

import qualified Data.List                      as L
import qualified Data.List.Index                as L
import qualified Data.Text                      as T
import           Data.Tuple.Extra
import qualified Potato.Data.Text.Zipper        as TZ


import           Linear.Metric                  (norm)
import           Linear.Vector                  ((^*))

import           Control.Exception              (assert)

-- TODO I think you need notion of half separation?
determineSeparation :: (LBox, (Int, Int, Int, Int)) -> (LBox, (Int, Int, Int, Int)) -> (Bool, Bool)
determineSeparation (lbx1, p1) (lbx2, p2) = r where
  (l1,r1,t1,b1) = lBox_to_axis $ lBox_expand lbx1 p1
  (l2,r2,t2,b2) = lBox_to_axis $ lBox_expand lbx2 p2
  hsep = l1 >= r2 || l2 >= r1
  vsep = t1 >= b2 || t2 >= b1
  r = (hsep, vsep)

determineSeparationForAttachment_custom :: (LBox, (Int, Int, Int, Int)) -> (LBox, (Int, Int, Int, Int)) -> (Bool, Bool)
determineSeparationForAttachment_custom = determineSeparation


-- TODO DELETE this version was to help support arrows very close to each other but not in one line (see diagram), however it causes undesireable behavior in other cases so we don't use it anymore, it needs to be fixed on an ad-hoc bases
-- in order to be separated for attachment, there must be space for a line in between the two boxes
-- e.g. both ends are offset by 2 but they only need a space of 3 between them
--   +-*
--   |
-- *-+
--determineSeparationForAttachment_custom :: (LBox, (Int, Int, Int, Int)) -> (LBox, (Int, Int, Int, Int)) -> (Bool, Bool)
--determineSeparationForAttachment_custom (lbx1, p1) (lbx2, p2) = r where
--  (l1,r1,t1,b1) = lBox_to_axis $ lBox_expand lbx1 p1
--  (l2,r2,t2,b2) = lBox_to_axis $ lBox_expand lbx2 p2
--  hsep = l1 >= r2+1 || l2 >= r1+1
--  vsep = t1 >= b2+1 || t2 >= b1+1
--  r = (hsep, vsep)


determineSeparationForAttachment :: (LBox, Int) -> (LBox, Int) -> (Bool, Bool)
determineSeparationForAttachment (lbx1, amt1') (lbx2, amt2') = determineSeparationForAttachment_custom (lbx1, amt1) (lbx2, amt2) where
  amt1 = (amt1',amt1',amt1',amt1')
  amt2 = (amt2',amt2',amt2',amt2')



maybeIndex :: Text -> Int -> Maybe MPChar
maybeIndex t i = if i < T.length t
  then Just $ (Just $ T.index t i)
  else Nothing

renderLine :: SuperStyle -> CartDir -> MPChar
renderLine SuperStyle {..} cd = case cd of
  CD_Up    -> _superStyle_vertical
  CD_Down  -> _superStyle_vertical
  CD_Left  -> _superStyle_horizontal
  CD_Right -> _superStyle_horizontal

renderLineEnd :: SuperStyle -> LineStyle -> CartDir -> Int -> MPChar
renderLineEnd SuperStyle {..} LineStyle {..} cd distancefromend = r where
  r = case cd of
    CD_Up -> fromMaybe _superStyle_vertical $ maybeIndex _lineStyle_upArrows distancefromend
    CD_Down -> fromMaybe _superStyle_vertical $ maybeIndex (T.reverse _lineStyle_downArrows) distancefromend
    CD_Left -> fromMaybe _superStyle_horizontal $ maybeIndex _lineStyle_leftArrows distancefromend
    CD_Right -> fromMaybe _superStyle_horizontal $ maybeIndex (T.reverse _lineStyle_rightArrows) distancefromend


renderAnchorType :: SuperStyle -> LineStyle -> AnchorType -> MPChar
renderAnchorType ss@SuperStyle {..} ls at = r where
  r = case at of
    AT_End_Up        -> renderLineEnd ss ls CD_Up 0
    AT_End_Down      -> renderLineEnd ss ls CD_Down 0
    AT_End_Left      -> renderLineEnd ss ls CD_Left 0
    AT_End_Right     -> renderLineEnd ss ls CD_Right 0
    AT_Elbow_TL      -> _superStyle_tl
    AT_Elbow_TR      -> _superStyle_tr
    AT_Elbow_BR      -> _superStyle_br
    AT_Elbow_BL      -> _superStyle_bl
    AT_Elbow_Invalid -> Just '?'


lineAnchorsForRender_simplify :: LineAnchorsForRender -> LineAnchorsForRender
lineAnchorsForRender_simplify LineAnchorsForRender {..} = r where
  -- remove 0 distance lines except at front and back
  withoutzeros = case _lineAnchorsForRender_rest of
    []   -> []
    x:xs -> x:withoutzerosback xs
    where
      withoutzerosback = \case
        [] -> []
        x:[] -> [x]
        (_, 0, False):xs -> xs
        -- this can happen now in a few cases, I don't think it's a big deal
        -- it does mess up our subsegmenting starting flags but I think in that case the midpoint probably got removed entirely due to it being too close to another one maybe??
        --(_, 0, True):_ -> error "unexpected 0 length subsegment starting anchor"
        (_, 0, True):xs -> xs
        x:xs -> x:withoutzerosback xs

  foldrfn (cd, d, s) [] = [(cd, d, s)]
  foldrfn (cd, d, firstisstart) ((cd',d', nextisstart):xs) = if cd == cd'
    then (cd, d+d', firstisstart):xs
    else (cd,d,firstisstart):(cd',d',nextisstart):xs
  withoutdoubles = foldr foldrfn [] withoutzeros
  r = LineAnchorsForRender {
      _lineAnchorsForRender_start = _lineAnchorsForRender_start
      , _lineAnchorsForRender_rest = withoutdoubles
    }

lineAnchorsForRender_end :: LineAnchorsForRender -> XY
lineAnchorsForRender_end LineAnchorsForRender {..} = foldl' (\p cdd -> p + cartDirWithDistanceToV2 cdd) _lineAnchorsForRender_start _lineAnchorsForRender_rest

lineAnchorsForRender_reverse :: LineAnchorsForRender -> LineAnchorsForRender
lineAnchorsForRender_reverse lafr@LineAnchorsForRender {..} = r where
  end = lineAnchorsForRender_end lafr
  revgo acc [] = acc
  revgo acc ((cd,d,False):[]) = (flipCartDir cd,d,True):acc
  revgo _ ((_,_,True):[]) = error "unexpected subsegment starting anchor at end"
  revgo acc ((cd,d,False):xs) = revgo ((flipCartDir cd, d, False):acc) xs
  revgo _ ((_,_,True):xs) = error "TODO this does not handle midpoint subsegment starting anchors correctly (not that it needs to right now)"
  revgostart [] = []
  revgostart ((cd,d,True):xs) = revgo [(flipCartDir cd,d,False)] xs
  revgostart _ = error "unexpected non-subsegment starting anchor at start"
  r = LineAnchorsForRender {
      _lineAnchorsForRender_start = end
      , _lineAnchorsForRender_rest = revgostart _lineAnchorsForRender_rest
    }

lineAnchorsForRender_toPointList :: LineAnchorsForRender -> [XY]
lineAnchorsForRender_toPointList LineAnchorsForRender {..} = r where
  scanlfn pos (cd,d,_) = pos + (cartDirToUnit cd) ^* d
  r = scanl scanlfn _lineAnchorsForRender_start _lineAnchorsForRender_rest

data SimpleLineSolverParameters_NEW = SimpleLineSolverParameters_NEW {
  _simpleLineSolverParameters_NEW_attachOffset :: Int -- cells to offset attach to box by
}

instance TransformMe SimpleLineSolverParameters_NEW where
  transformMe_rotateLeft = id
  transformMe_rotateRight = id
  transformMe_reflectHorizontally = id


restify :: [(CartDir, Int)] -> [(CartDir, Int, Bool)]
restify []          = []
restify ((cd,d):xs) = (cd,d,True):fmap (\(a,b) -> (a,b,False)) xs

-- used to convert AL_ANY at (ax, ay) to an AttachmentLocation based on target position (tx, ty)
-- TODO test that this function is rotationally/reflectively symmetric (although it doesn't really matter if it isn't, however due to recursive implementation of sSimpleLineSolver it's kind of awkward if it's not)
makeAL :: XY -> XY -> AttachmentLocation
makeAL (V2 ax ay) (V2 tx ty) = r where
  dx = tx - ax
  dy = ty - ay
  r = if abs dx > abs dy
    then if dx > 0
      then AL_Right
      else AL_Left
    else if dy > 0
      then AL_Bot
      else AL_Top

-- | configuration to determine whether the attachment point is offest by the border of the box
newtype OffsetBorder = OffsetBorder { unOffsetBorder :: Bool } deriving (Show)

instance TransformMe OffsetBorder where
  transformMe_rotateLeft = id
  transformMe_rotateRight = id
  transformMe_reflectHorizontally = id


-- 🙈🙈🙈
-- TODO update to be (LBox, AttachmentLocation, AttachmentOffsetRatio, OffsetBorder)
sSimpleLineSolver_NEW :: (Text, Int) -> CartRotationReflection -> SimpleLineSolverParameters_NEW -> (BoxWithAttachmentLocation, OffsetBorder) -> (BoxWithAttachmentLocation, OffsetBorder) -> LineAnchorsForRender
sSimpleLineSolver_NEW (errormsg, depth) crr sls ((lbx1, al1_, af1), offb1) ((lbx2, al2_, af2), offb2) =  finaloutput where
  --LBox (V2 x1 y1) (V2 w1 h1) = lbx1
  LBox (V2 _ y2) (V2 _ h2) = lbx2

  attachoffset = _simpleLineSolverParameters_NEW_attachOffset sls

  al1 = case al1_ of
    AL_Any -> assert (af1 == attachment_offset_rel_default) $ makeAL (_lBox_tl lbx1) $ case al2_ of
      AL_Any -> _lBox_tl lbx2
      _      -> end
    x -> x
  al2 = case al2_ of
    AL_Any -> assert (af2 == attachment_offset_rel_default) $ makeAL (_lBox_tl lbx2) $ case al1_ of
      AL_Any -> _lBox_tl lbx1
      _      -> start
    x -> x

  lbal1 = ((lbx1, al1, af1), offb1)
  lbal2 = ((lbx2, al2, af2), offb2)

  start@(V2 ax1 ay1) = attachLocationFromLBox_conjugateCartRotationReflection crr (unOffsetBorder offb1) (lbx1, al1, af1)
  end@(V2 ax2 ay2) = attachLocationFromLBox_conjugateCartRotationReflection crr (unOffsetBorder offb2) (lbx2, al2, af2)


  -- TODO need to selectively remove offset border based on whether there is an arrow or not (you need to set sSimpleLineSolver_NEW OffsetBorder parameter, issue isn't here)
  -- this causes stuff like this right now
              -- ╔═════════════╗
              -- ║╔GoatState═══║═══════════╗
              -- ║║            ║           ║
              -- ║║            ║           ║
              -- ║║            ║           ║
              -- ║║            ║           ║
              -- ║╚════════════║═══════════╝
              -- ║            ║║
              -- ║            ║v
              -- ║╔OwlPFWorksp║ce══════════╗
              -- ║║           ║            ║
              -- ║║           ║            ║
              -- ║║           ║            ║
              -- ║║           ║            ║
              -- ║╚═══════════║════════════╝
              -- ╚════════════╝
  (hsep, vsep) = determineSeparationForAttachment (lbx1, if unOffsetBorder offb1 then 1 else 0) (lbx2, if unOffsetBorder offb2 then 1 else 0)

  lbx1isstrictlyleft = ax1 < ax2
  lbx1isleft = ax1 <= ax2
  lbx1isstrictlyabove = ay1 < ay2
  ay1isvsepfromlbx2 = ay1 < y2 || ay1 >= y2 + h2

  traceStep :: String -> a -> a
  traceStep _ x = x
  --traceStep = trace  
  stepdetail = show lbal1 <> " | " <> show lbal2 <> "\n"
  nextmsg step = (errormsg <> " " <> step <> ": " <> stepdetail, depth+1)

  (l1_inc,r1,t1_inc,b1) = lBox_to_axis lbx1
  (l2_inc,r2,t2_inc,b2) = lBox_to_axis lbx2

  -- TODO offset by boundaryoffset from parameters
  l = min (l1_inc-1) (l2_inc-1)
  t = min (t1_inc-1) (t2_inc-1)
  b = max b1 b2

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
          , _lineAnchorsForRender_rest = restify [lb1_to_center, centerverticalline, center_to_lb2]
        }

    -- WORKING
    -- <-2  1->
    AL_Right | al2 == AL_Left && not vsep -> traceStep "case 2" $ r where

      goup = (ay1-t)+(ay2-t) < (b-ay1)+(b-ay2)

      rightedge = if (not goup && b2 < ay1) || (goup && ay1 < t2_inc)
        then r1 + attachoffset
        else (max (r1+attachoffset) r2)

      lb1_to_right = (CD_Right, rightedge-ax1)
      right_to_torb = if goup
        then (CD_Up, ay1-t)
        else (CD_Down, b-ay1)

      leftedge = if (goup && t2_inc <= t1_inc) || (not goup && b2 > b1)
        then ax2-attachoffset
        else min (ax2-attachoffset) (l1_inc-attachoffset)

      torb = (CD_Left, rightedge - leftedge)

      torb_to_left = if goup
        then (CD_Down, ay2-t)
        else (CD_Up, b-ay2)
      left_to_lb2 = (CD_Right, ax2-leftedge)
      r = LineAnchorsForRender {
          _lineAnchorsForRender_start = start
          , _lineAnchorsForRender_rest = restify [lb1_to_right, right_to_torb, torb, torb_to_left, left_to_lb2]
        }

    -- WORKING
    -- <-2
    --      1->
    AL_Right | al2 == AL_Left && vsep -> traceStep "case 3" $ r where
      halfway = if b1 < t2_inc
        then (b1+t2_inc) `div` 2
        else (b2+t1_inc) `div` 2
      lb1_to_right = (CD_Right, attachoffset)
      right_to_center = if lbx1isstrictlyabove
        then (CD_Down, halfway-ay1)
        else (CD_Up, ay1-halfway)
      center = (CD_Left, attachoffset*2 + (ax1-ax2))
      center_to_left = if lbx1isstrictlyabove
        then (CD_Down, ay2-halfway)
        else (CD_Up, halfway-ay2)
      left_to_lb2 = (CD_Right, attachoffset)
      r = LineAnchorsForRender {
          _lineAnchorsForRender_start = start
          , _lineAnchorsForRender_rest = restify [lb1_to_right, right_to_center, center, center_to_left, left_to_lb2]
        }

    -- WORKING
    --
    -- 1->
    --     2->
    -- ay1isvsepfromlbx2 (different boxes)
    --
    -- OR
    --
    -- ->1
    -- ->2
    -- r1 == r2 (special case when the 2 boxes are the same)
    AL_Right | al2 == AL_Right && (ay1isvsepfromlbx2 || r1 == r2) -> traceStep "case 4" $ answer where
      rightedge = max r1 r2 + attachoffset
      lb1_to_right1 = (CD_Right, rightedge-r1)
      right1_to_right2 = if lbx1isstrictlyabove
        then (CD_Down, ay2-ay1)
        else (CD_Up, ay1-ay2)
      right2_to_lb2 = (CD_Left, rightedge-r2)
      answer = LineAnchorsForRender {
          _lineAnchorsForRender_start = start
          , _lineAnchorsForRender_rest = restify [lb1_to_right1, right1_to_right2, right2_to_lb2]
        }

    -- WORKING
    -- ->1 ->2
    AL_Right | al2 == AL_Right && lbx1isleft && not ay1isvsepfromlbx2 -> traceStep "case 5b" $  answer where


      goupordown = (ay1-t)+(ay2-t) < (b-ay1)+(b-ay2)

      -- TODO maybe it would be nice if this traveled a little further right
      lb1_to_right1 = (CD_Right, attachoffset)

      right1_to_torb = if goupordown
        then (CD_Up, ay1-t)
        else (CD_Down, b-ay1)
      torb = (CD_Right, r2-r1)
      torb_to_right2 = if goupordown
        then (CD_Down, ay2-t)
        else (CD_Up, b-ay2)
      right2_to_lb2 = (CD_Left, attachoffset)
      answer = LineAnchorsForRender {
          _lineAnchorsForRender_start = start
          , _lineAnchorsForRender_rest = restify [lb1_to_right1, right1_to_torb, torb, torb_to_right2, right2_to_lb2]
        }

    -- ->2 ->1 (will not get covered by rotation)
    AL_Right | al2 == AL_Right && not ay1isvsepfromlbx2 -> traceStep "case 6 (reverse)" $ lineAnchorsForRender_reverse $ sSimpleLineSolver_NEW (nextmsg "case 6") crr sls lbal2 lbal1

    --     2->
    --  ^
    --  |
    --  1
    --     2->
    AL_Top | al2 == AL_Right && lbx1isleft -> traceStep "case 7" $ r where
      upd = if vsep
        then attachoffset
        else ay1-t + attachoffset
      topline = ay1-upd
      lb1_to_up = (CD_Up, upd)
      right = if topline < ay2
        then (max ax2 r1) + attachoffset
        else ax2 + attachoffset
      up_to_right1 =  (CD_Right, right-ax1)
      right1_to_right2 = if topline < ay2
        then (CD_Down, ay2-topline)
        else (CD_Up, topline-ay2)
      right2_to_lb2 = (CD_Left, right-ax2)
      r = LineAnchorsForRender {
          _lineAnchorsForRender_start = start
          , _lineAnchorsForRender_rest = restify [lb1_to_up,up_to_right1,right1_to_right2,right2_to_lb2]
        }
    --     <-2
    --  ^
    --  |
    --  1   <-2 (this one handles both vsep cases)
    AL_Top | al2 == AL_Left && lbx1isleft -> traceStep "case 9" $ r where
      topedge = min (ay1 - attachoffset) ay2
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
          , _lineAnchorsForRender_rest = restify [lb1_to_up, up_to_over,over_to_down,down_to_lb2]
        }

    --        ^
    --        |
    -- <-2->  1 (will not get covered by rotation)
    AL_Top | al2 == AL_Left || al2 == AL_Right -> traceStep "case 10 (flip)" $  transformMe_reflectHorizontally $ sSimpleLineSolver_NEW (nextmsg "case 10") (transformMe_reflectHorizontally crr) (transformMe_reflectHorizontally sls) (transformMe_reflectHorizontally lbal1) (transformMe_reflectHorizontally lbal2)

    AL_Top | al2 == AL_Any -> error "should have been handled by earlier substitution"
    AL_Any | al2 == AL_Top -> error "should have been handled by earlier substitution"
    AL_Any | al2 == AL_Any -> error "should have been handled by earlier substitution"

    _ -> traceStep "case 14 (rotate)" $ transformMe_rotateRight $ sSimpleLineSolver_NEW (nextmsg "case 14") (transformMe_rotateLeft crr) (transformMe_rotateLeft sls) (transformMe_rotateLeft lbal1) (transformMe_rotateLeft lbal2)

  finaloutput = if depth > 10
    then error errormsg
    else lineAnchorsForRender_simplify anchors

doesLineContain :: XY -> XY -> (CartDir, Int, Bool) -> Maybe Int
doesLineContain (V2 px py) (V2 sx sy) (tcd, tl, _) = case tcd of
  CD_Left | py == sy -> if px <= sx && px >= sx-tl then Just (sx-px) else Nothing
  CD_Right | py == sy -> if px >= sx && px <= sx+tl then Just (px-sx) else Nothing
  CD_Up | px == sx -> if py <= sy && py >= sy-tl then Just (sy-py) else Nothing
  CD_Down | px == sx -> if py >= sy && py <= sy+tl then Just (py-sy) else Nothing
  _ -> Nothing

-- TODO test
doesLineContainBox :: LBox -> XY -> (CartDir, Int, Bool) -> Bool
doesLineContainBox lbox (V2 sx sy) (tcd, tl, _) = r where
  (x,y, w,h) = case tcd of
    CD_Left  -> (sx-tl, sy, tl+1, 1)
    CD_Right -> (sx, sy, tl+1, 1)
    CD_Up    -> (sx, sy-tl, 1, tl+1)
    CD_Down  -> (sx, sy, 1, tl+1)
  lbox2 = LBox (V2 x y) (V2 w h)
  r = does_lBox_intersect lbox lbox2


walkToRender :: SuperStyle -> LineStyle -> LineStyle -> Bool -> XY -> (CartDir, Int, Bool) -> Maybe (CartDir, Int, Bool) -> Int -> (XY, MPChar)
walkToRender ss ls lse isstart begin (tcd, tl, _) mnext d = r where
  currentpos = begin + (cartDirToUnit tcd) ^* d

  endorelbow = renderAnchorType ss lse $ cartDirToAnchor tcd (fmap fst3 mnext)
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

lineAnchorsForRender_length :: LineAnchorsForRender -> Int
lineAnchorsForRender_length LineAnchorsForRender {..} = r where
  foldfn (_,d,_) acc = acc + d
  r = foldr foldfn 1 _lineAnchorsForRender_rest

lineAnchorsForRender_renderAt :: SuperStyle -> LineStyle -> LineStyle -> LineAnchorsForRender -> XY -> MPChar
lineAnchorsForRender_renderAt ss ls lse LineAnchorsForRender {..} pos = r where
  walk (isstart, curbegin) a = case a of
    [] -> Nothing
    x:xs -> case doesLineContain pos curbegin x of
      Nothing ->  walk (False, nextbegin) xs
      Just d -> Just $ case xs of
        []  -> walkToRender ss ls lse isstart curbegin x Nothing d
        y:_ -> walkToRender ss ls lse isstart curbegin x (Just y) d
      where
        nextbegin = curbegin + cartDirWithDistanceToV2 x

  manswer = walk (True, _lineAnchorsForRender_start) _lineAnchorsForRender_rest
  r = case manswer of
    Nothing             -> Nothing
    Just (pos', mpchar) -> assert (pos == pos') mpchar

-- UNTESTED
-- returns index of subsegment that intersects with pos
-- e.g.
--      0 ---(x)-- 1 ------ 2
-- returns Just 0
lineAnchorsForRender_findIntersectingSubsegment :: LineAnchorsForRender -> XY -> Maybe Int
lineAnchorsForRender_findIntersectingSubsegment  LineAnchorsForRender {..} pos = r where
  walk i curbegin a = case a of
    [] -> Nothing
    x@(_,_,s):xs -> case doesLineContain pos curbegin x of
      Nothing ->  walk new_i (curbegin + cartDirWithDistanceToV2 x) xs
      Just _  -> Just new_i
      where new_i = if s then i+1 else i
  r = walk (-1) _lineAnchorsForRender_start _lineAnchorsForRender_rest

lineAnchorsForRender_doesIntersectPoint :: LineAnchorsForRender -> XY -> Bool
lineAnchorsForRender_doesIntersectPoint LineAnchorsForRender {..} pos = r where
  walk curbegin a = case a of
    [] -> False
    x:xs -> case doesLineContain pos curbegin x of
      Nothing ->  walk (curbegin + cartDirWithDistanceToV2 x) xs

      Just _  -> True
  r = walk _lineAnchorsForRender_start _lineAnchorsForRender_rest


lineAnchorsForRender_doesIntersectBox :: LineAnchorsForRender -> LBox -> Bool
lineAnchorsForRender_doesIntersectBox LineAnchorsForRender {..} lbox = r where
  walk curbegin a = case a of
    [] -> False
    x:xs -> if doesLineContainBox lbox curbegin x
      then True
      else walk (curbegin + cartDirWithDistanceToV2 x) xs
  r = walk _lineAnchorsForRender_start _lineAnchorsForRender_rest



renderLabelFn :: (XY, SAutoLineLabel) -> XY -> MPChar
renderLabelFn (V2 llx lly, llabel) (V2 x y) = r where
  text = _sAutoLineLabel_text llabel
  tz = TZ.top (TZ.fromText text)
  dl = TZ.displayLinesWithAlignment TZ.TextAlignment_Left maxBound 0 1 tz
  offset = (- (T.length text) `div` 2, 0)
  r = join $ displayLinesToChar (llx, lly) dl (x,y) offset



-- TODO also render labels
sSimpleLineNewRenderFn :: SAutoLine -> Maybe LineAnchorsForRender -> SEltDrawer
sSimpleLineNewRenderFn ssline@SAutoLine {..} mcache = drawer where

  getAnchors :: (HasOwlTree a) => a -> LineAnchorsForRender
  getAnchors ot = case mcache of
    Just x  -> x
    Nothing -> sSimpleLineNewRenderFnComputeCache ot ssline

  renderfn :: SEltDrawerRenderFn
  renderfn ot xy = r where
    anchors = getAnchors ot


    -- m1 takes priority over m2
    mergeMaybe :: MPChar -> MPChar -> MPChar
    mergeMaybe m1 m2 = maybe m2 Just m1

    -- TODO someday cache this too
    llabels = getSortedSAutoLineLabelPositions ot ssline
    llabelsrendered = fmap (\(pos,_,llabel) -> renderLabelFn (pos, llabel) xy) llabels
    mlabelchar = foldr mergeMaybe Nothing llabelsrendered
    mlinechar = lineAnchorsForRender_renderAt _sAutoLine_superStyle _sAutoLine_lineStyle _sAutoLine_lineStyleEnd anchors xy

    -- render label over lines
    r = mergeMaybe mlabelchar mlinechar

  boxfn :: SEltDrawerBoxFn
  boxfn ot = r where
    anchorbox = case nonEmpty (lineAnchorsForRender_toPointList (getAnchors ot)) of
      Nothing -> LBox 0 0
      -- add_XY_to_lBox is non-inclusive with bottom/right so we expand by 1 to make it inclusive
      Just (x :| xs) -> lBox_expand (foldl' (flip add_XY_to_lBox) (make_0area_lBox_from_XY x) xs) (0,1,0,1)

    -- UNTESTED
    -- TODO someday cache this too
    llabels = getSortedSAutoLineLabelPositions ot ssline
    llabelbox (V2 x y) llabel = LBox (V2 (x - wover2) y) (V2 w 1) where
      w = T.length $ _sAutoLineLabel_text llabel
      wover2 = (w+1) `div` 2
    mlabelbox = foldr (\(pos, _, llabel) mbox -> maybe (Just $ llabelbox pos llabel) (\box -> Just $ box `union_lBox` llabelbox pos llabel) mbox) Nothing llabels

    r = case mlabelbox of
      Nothing       -> anchorbox
      Just labelbox -> union_lBox anchorbox labelbox



  drawer = SEltDrawer {
      _sEltDrawer_box = boxfn
      , _sEltDrawer_renderFn = renderfn

      -- TODO
      , _sEltDrawer_maxCharWidth = 1
    }

lineAnchorsForRender_concat :: [LineAnchorsForRender] -> LineAnchorsForRender
lineAnchorsForRender_concat [] = error "expected at least one LineAnchorsForRender"
lineAnchorsForRender_concat (x:xs) = foldl' foldfn x xs where
  -- TODO re-enable assert when it gets fixed
  foldfn h c = --assert (lineAnchorsForRender_end h == _lineAnchorsForRender_start c) $
    h { _lineAnchorsForRender_rest = _lineAnchorsForRender_rest h <> _lineAnchorsForRender_rest c }


pairs :: [a] -> [(a, a)]
pairs [] = []
pairs xs = zip xs (tail xs)


maybeGetAttachBox_NEW2 :: (HasOwlTree a) => a -> Maybe Attachment -> Maybe BoxWithAttachmentLocation
maybeGetAttachBox_NEW2 ot mattachment = do
  Attachment rid al ratio <- mattachment
  sowl <- hasOwlTree_findSuperOwl ot rid
  sbox <- getSEltBox_naive $ hasOwlItem_toSElt_hack sowl
  return (sbox, al, ratio)

-- returns a list of LineAnchorsForRender, one for each segment separated by midpoints
sAutoLine_to_lineAnchorsForRenderList :: (HasOwlTree a) => a -> SAutoLine -> [LineAnchorsForRender]
sAutoLine_to_lineAnchorsForRenderList ot SAutoLine {..} = anchorss where

  -- TODO set properly
  params = SimpleLineSolverParameters_NEW {
      -- TODO maybe set this based on arrow head size (will differ for each end so you need 4x)
      _simpleLineSolverParameters_NEW_attachOffset = 1
    }

  startlbal = case maybeGetAttachBox_NEW2 ot _sAutoLine_attachStart of
    Nothing    -> ((LBox _sAutoLine_start 1, AL_Any, attachment_offset_rel_default), OffsetBorder False)
    Just bal -> (bal, OffsetBorder True)
  endlbal = case maybeGetAttachBox_NEW2 ot _sAutoLine_attachEnd of
    Nothing    -> ((LBox _sAutoLine_end 1, AL_Any, attachment_offset_rel_default), OffsetBorder False)
    Just bal -> (bal, OffsetBorder True)
  midlbals = fmap (\(SAutoLineConstraintFixed xy) ->   ((LBox xy 1, AL_Any, attachment_offset_rel_default), OffsetBorder False)) _sAutoLine_midpoints

  -- ???? TODO BUG this is a problem, you need selective offsetting for each side of the box, in particular, midpoints can't offset and the point needs to land exactly on the midpoint
  -- NOTE for some reason sticking trace statements in sSimpleLineSolver will causes regenanchors to get called infinite times :(
  anchorss = fmap (\(lbal1, lbal2) -> sSimpleLineSolver_NEW ("",0) cartRotationReflection_identity params lbal1 lbal2) $ pairs ((startlbal : midlbals) <> [endlbal])

sSimpleLineNewRenderFnComputeCache :: (HasOwlTree a) => a -> SAutoLine -> LineAnchorsForRender
sSimpleLineNewRenderFnComputeCache ot sline = anchors where
  anchors = lineAnchorsForRender_simplify . lineAnchorsForRender_concat $ sAutoLine_to_lineAnchorsForRenderList ot sline

internal_getSAutoLineLabelPosition_walk :: LineAnchorsForRender -> Int -> XY
internal_getSAutoLineLabelPosition_walk lar targetd = r where
  walk [] curbegin _ = curbegin
  walk (x@(cd,d,_):rest) curbegin traveld = r2 where
    nextbegin = curbegin + cartDirWithDistanceToV2 x
    r2 = if traveld + d >= targetd
      then curbegin + cartDirWithDistanceToV2 (cd, targetd - traveld, undefined)
      else walk rest nextbegin (traveld + d)
  r = walk (_lineAnchorsForRender_rest lar) (_lineAnchorsForRender_start lar) 0

-- TODO remove SAutoLine arg
internal_getSAutoLineLabelPosition :: LineAnchorsForRender -> SAutoLine -> SAutoLineLabel -> XY
internal_getSAutoLineLabelPosition lar _ SAutoLineLabel {..} = r where
  totall = lineAnchorsForRender_length lar
  targetd = case _sAutoLineLabel_position of
    SAutoLineLabelPositionRelative rp -> max 0 . floor $ (fromIntegral totall * rp)
  r = internal_getSAutoLineLabelPosition_walk lar targetd

getSAutoLineLabelPositionFromLineAnchorsForRender :: LineAnchorsForRender -> SAutoLine -> SAutoLineLabel -> XY
getSAutoLineLabelPositionFromLineAnchorsForRender lar sal sall = internal_getSAutoLineLabelPosition lar sal sall

-- the SAutoLineLabel does not have to be one of labels contained in the SAutoLine _sAutoLine_labels
-- which is useful for positioning SAutoLineLabel before adding them to SAutoLine
-- however the midpoint index in SAutoLineLabel is expected to map correctly to the SAutoLine
getSAutoLineLabelPosition :: (HasOwlTree a) => a -> SAutoLine -> SAutoLineLabel -> XY
getSAutoLineLabelPosition ot sal sall = getSAutoLineLabelPositionFromLineAnchorsForRender lar sal sall where
  lar = sAutoLine_to_lineAnchorsForRenderList ot sal L.!! (_sAutoLineLabel_index sall)

-- get SAutoLineLabel positions in visual order (which may not be the same as logical order)
-- return includes SAutoLineLabel and its original logical index for convenience
getSortedSAutoLineLabelPositions :: (HasOwlTree a) => a -> SAutoLine -> [(XY, Int, SAutoLineLabel)]
getSortedSAutoLineLabelPositions ot sal@SAutoLine {..} = r where
  sortfn (_,a) (_,b) = case compare (_sAutoLineLabel_index a) (_sAutoLineLabel_index b) of
    EQ -> case _sAutoLineLabel_position a of
      SAutoLineLabelPositionRelative x -> case _sAutoLineLabel_position b of
        SAutoLineLabelPositionRelative y -> compare x y
    x -> x
  sortedlls = sortBy sortfn $ L.indexed _sAutoLine_labels

  larlist = sAutoLine_to_lineAnchorsForRenderList ot sal

  r = fmap (\(i, sall) -> (internal_getSAutoLineLabelPosition (larlist L.!! _sAutoLineLabel_index sall) sal sall, i, sall)) sortedlls


-- takes a list of line anchors as returned by sAutoLine_to_lineAnchorsForRenderList and a position
-- returns closest orthognally projected point on the line as a tuple (projected position, index into larlist, relative distance along the LineAnchorsForRender that the point is on)
getClosestPointOnLineFromLineAnchorsForRenderList :: [LineAnchorsForRender] -> XY -> (XY, Int, Float)
getClosestPointOnLineFromLineAnchorsForRenderList larlist pos@(V2 posx posy) = r where

  foldlfn ::
    (Int, (XY, Int, Float), Int) -- (previous closest distance to line, (prev closest position, index into larlist, rel distance on segment))
    -> LineAnchorsForRender
    -> (Int, (XY, Int, Float), Int)
  foldlfn (closestd, closestp, curindex) lar = r2 where


    foldlfn2 ::
      (Int, XY, Int, Maybe (Int, XY)) -- (total distance we traveled so far, current anchor position, prev closest distance to line (includes second fold results up until now), Maybe (how far we traveled to new closest point on line, new closest point))
      -> (CartDir, Int, Bool)
      -> (Int, XY, Int, Maybe (Int, XY))
    foldlfn2 (traveld, curp@(V2 curx cury), closestd2, mnewclosestpos2) cdwd@(cd,d,_) = r3 where

      between :: Int -> Int -> Int -> Bool
      between p a b = (p >= a && p <= b) || (p <= a && p >= b)

      xydistance :: XY -> XY -> Float
      xydistance (V2 ax ay) (V2 bx by) = norm (V2 (fromIntegral ax - fromIntegral bx) (fromIntegral ay - fromIntegral by))


      endp@(V2 endx endy) = curp + cartDirWithDistanceToV2  cdwd

      dtoend = (xydistance pos endp)
      dtocur = (xydistance pos curp)

      dandpostostartorend = if dtocur < dtoend
        then (dtocur, curp)
        else (dtoend, endp)

      -- project pos onto each segment
      (projd, projp) = if cd == CD_Up || cd == CD_Down
        -- project horizontally
        then if between posy cury endy
          -- if projection in bounds
          then (fromIntegral $ abs (curx - posx), V2 curx posy)
          else  dandpostostartorend
        -- project vertically
        else if between posx curx endx
          -- if projection in bounds
          then (fromIntegral $ abs (cury - posy), V2 posx cury)
          else dandpostostartorend

      -- if we are closer than previous closest point
      r3 = if projd < fromIntegral closestd2
        -- update the new closest point
        then (traveld + d, endp, ceiling projd, Just (traveld + floor (xydistance curp projp), projp))
        -- same as before, keep going
        else (traveld + d, endp, closestd2, mnewclosestpos2)

    -- walk through each segment in lar
    (totald, _, newclosestd, mnewclosestpos) = L.foldl foldlfn2 (0, _lineAnchorsForRender_start lar, closestd, Nothing) (_lineAnchorsForRender_rest lar)

    r2 = case mnewclosestpos of
      -- did not find a closer point on lar
      Nothing -> (closestd, closestp, curindex+1)
      Just (newclosesttraveld, newclosestp) -> (newclosestd, (newclosestp, curindex, fromIntegral newclosesttraveld / fromIntegral totald), curindex+1)

  (_,r,_) = L.foldl foldlfn (maxBound :: Int, (0,0,0), 0) larlist
