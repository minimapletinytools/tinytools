{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Potato.Flow.Math (
  XY
  , LBox(..)
  , nilLBox

  -- TODO rename this to lBox for consistency...
  , make_lBox_from_XY
  , make_lBox_from_XYs
  , does_lBox_contains_XY
  , lBox_tl
  , lBox_area
  , lBox_to_axis
  , add_XY_to_LBox

  , make_lBox_from_axis
  , union_lBox
  , intersect_lBox
  , intersect_lBox_include_zero_area
  , does_lBox_intersect
  , does_lBox_intersect_include_zero_area
  , substract_lBox

  -- these helpers maybe belong in a different file, they have very specific usages
  , CanonicalLBox(..)
  , canonicalLBox_from_lBox
  , canonicalLBox_from_lBox_
  , lBox_from_canonicalLBox
  , deltaLBox_via_canonicalLBox
  , lBox_isCanonicalLBox

  , Delta(..)
  , DeltaXY(..)
  , DeltaLBox(..)

  , module Linear.V2
) where

import           Relude

import           Control.Exception (assert)
import           Data.Aeson
import           Data.Binary
import           Linear.V2
import qualified Text.Show

{-
 CORDINATE SYSTEM
 UPPER LEFT CORNER is 0 0
 (0,0)--- +x
  |
  |
  +y
-}

type XY = V2 Int
instance FromJSON XY
instance ToJSON XY
instance FromJSONKey XY
instance ToJSONKey XY

-- | a point in screen space
-- should only be used by VC, so does not belong here
--newtype VPoint = VPoint (Int, Int) deriving (Generic, Show, FromJSON, ToJSON)

-- | a box in logical space
-- note size is non inclusive
-- e.g. an LBox with size (1,1) is exactly 1 point at ul
-- e.g. an LBox with size (0,0) contains nothing
data LBox = LBox {
  _lBox_tl     :: XY
  , _lBox_size :: XY
} deriving (Eq, Generic)

instance Show LBox where
  show (LBox (V2 x y) (V2 w h)) = "LBox: " <> show x <> " " <> show y <> " " <> show w <> " " <> show h

instance FromJSON LBox
instance ToJSON LBox
instance Binary LBox
instance NFData LBox

nilLBox :: LBox
nilLBox = LBox 0 0

lBox_area :: LBox -> Int
lBox_area (LBox _ (V2 w h)) = w*h

lBox_tl :: LBox -> XY
lBox_tl (LBox p _) = p

-- | returns a 1 area LBox
make_lBox_from_XY :: XY -> LBox
make_lBox_from_XY p = LBox p 1

-- | always returns a canonical LBox
make_lBox_from_XYs :: XY -> XY -> LBox
make_lBox_from_XYs (V2 x1 y1) (V2 x2 y2) =
  LBox {
    _lBox_tl= V2 (min x1 x2) (min y1 y2)
    , _lBox_size  = V2 (abs (x1 - x2)) (abs (y1 - y2))
  }

-- | always returns a canonical LBox
add_XY_to_LBox :: XY -> LBox -> LBox
add_XY_to_LBox (V2 px py) lbox = r where
  (LBox (V2 bx by) (V2 bw bh)) = canonicalLBox_from_lBox_ lbox
  r = LBox {
    _lBox_tl = V2 (min px bx) (min py by)
    , _lBox_size  = V2 (max bw (abs (px-bx))) (max bh (abs (py-by)))
  }

does_lBox_contains_XY :: LBox -> XY -> Bool
does_lBox_contains_XY (LBox (V2 bx by) (V2 bw bh)) (V2 px py) =
  px >= bx && py >= by && px < (bx + bw) && py < (by + bh)

-- | right and bottom axis are non-inclusive
make_lBox_from_axis :: (Int, Int, Int, Int) -> LBox
make_lBox_from_axis (x1,x2,y1,y2) = LBox (V2 rx ry) (V2 rw rh) where
  rx = min x1 x2
  ry = min y1 y2
  rw = abs (x1-x2)
  rh = abs (y1-y2)

-- | (left, right, top, bottom)
-- right and bottom are non-inclusive
lBox_to_axis :: LBox -> (Int, Int, Int, Int)
lBox_to_axis (LBox (V2 x y) (V2 w h)) = (min x (x+w), max x (x+w), min y (y+h), max y (y+h))

min4 :: (Ord a) => a -> a -> a -> a -> a
min4 a1 a2 a3 a4 = min (min (min a1 a2) a3) a4

max4 :: (Ord a) => a -> a -> a -> a -> a
max4 a1 a2 a3 a4 = max (max (max a1 a2) a3) a4

-- | inverted LBox are treated as if not inverted
union_lBox :: LBox -> LBox -> LBox
union_lBox (LBox (V2 x1 y1) (V2 w1 h1)) (LBox (V2 x2 y2) (V2 w2 h2)) = combined where
  cx1 = x1 + w1
  cy1 = y1 + h1
  cx2 = x2 + w2
  cy2 = y2 + h2
  combined = make_lBox_from_axis (min4 x1 cx1 x2 cx2, max4 x1 cx1 x2 cx2, min4 y1 cy1 y2 cy2, max4 y1 cy1 y2 cy2)

-- | inverted LBox are treated as if not inverted
intersect_lBox :: LBox -> LBox -> Maybe LBox
intersect_lBox lb1@(LBox (V2 x1 y1) (V2 w1 h1)) lb2@(LBox (V2 x2 y2) (V2 w2 h2)) = r where
  cx1 = x1 + w1
  cy1 = y1 + h1
  cx2 = x2 + w2
  cy2 = y2 + h2
  l1 = min cx1 x1
  l2 = min cx2 x2
  r1 = max cx1 x1
  r2 = max cx2 x2
  t1 = min cy1 y1
  t2 = min cy2 y2
  b1 = max cy1 y1
  b2 = max cy2 y2
  r = if does_lBox_intersect lb1 lb2
    then Just $ make_lBox_from_axis (max l1 l2, min r1 r2, max t1 t2, min b1 b2)
    else Nothing

intersect_lBox_include_zero_area :: LBox -> LBox -> Maybe LBox
intersect_lBox_include_zero_area lb1@(LBox (V2 x1 y1) (V2 w1 h1)) lb2@(LBox (V2 x2 y2) (V2 w2 h2)) = r where
  cx1 = x1 + w1
  cy1 = y1 + h1
  cx2 = x2 + w2
  cy2 = y2 + h2
  l1 = min cx1 x1
  l2 = min cx2 x2
  r1 = max cx1 x1
  r2 = max cx2 x2
  t1 = min cy1 y1
  t2 = min cy2 y2
  b1 = max cy1 y1
  b2 = max cy2 y2
  r = if does_lBox_intersect_include_zero_area lb1 lb2
    then Just $ make_lBox_from_axis (max l1 l2, min r1 r2, max t1 t2, min b1 b2)
    else Nothing


does_lBox_intersect :: LBox -> LBox -> Bool
does_lBox_intersect lb1 lb2 = r where
  (l1,r1,t1,b1) = lBox_to_axis lb1
  (l2,r2,t2,b2) = lBox_to_axis lb2
  r | lBox_area lb1 == 0 = False
    | lBox_area lb2 == 0 = False
    | l1 >= r2 = False
    | l2 >= r1 = False
    | t1 >= b2 = False
    | t2 >= b1 = False
    | otherwise = True

does_lBox_intersect_include_zero_area :: LBox -> LBox -> Bool
does_lBox_intersect_include_zero_area lb1 lb2 = r where
  (l1,r1,t1,b1) = lBox_to_axis lb1
  (l2,r2,t2,b2) = lBox_to_axis lb2
  r | lb1 == lb2 = True -- this covers the case of 2 0 area boxes over each other
    | l1 >= r2 = False
    | l2 >= r1 = False
    | t1 >= b2 = False
    | t2 >= b1 = False
    | otherwise = True


-- | substract lb2 from lb1 and return [LBox] representing the difference
substract_lBox :: LBox -> LBox -> [LBox]
substract_lBox lb1@(LBox _ (V2 w1 h1)) lb2 = r where
  (l1,r1,t1,b1) = lBox_to_axis lb1
  (l2,r2,t2,b2) = lBox_to_axis lb2
  mleft = if l1 < l2
    then Just $ LBox (V2 l1 t1) (V2 (min (l2-l1) w1) h1)
    else Nothing
  mright = if r1 > r2
    then Just $ LBox (V2 (max r2 l1) t1) (V2 (min (r1-r2) w1) h1)
    else Nothing
  mtop' =  if t1 < t2
    then Just $ LBox (V2 l1 t1) (V2 w1 (min (t2-t1) h1))
    else Nothing
  mbot' = if b1 > b2
    then Just $ LBox (V2 l1 (max b2 t1)) (V2 w1 (min (b1-b2) h1))
    else Nothing
  -- TODO crop away mleft/mright from mtop'/mbot'
  mtop = mtop'
  mbot = mbot'
  r = catMaybes [mleft,mright,mtop, mbot]


-- | CanonicalLBox is always has non-negative width/height
-- and tracks which axis are flipped to return back to original LBox
-- first Bool is if x values are flipped, second is for y
data CanonicalLBox = CanonicalLBox Bool Bool LBox

canonicalLBox_from_lBox :: LBox -> CanonicalLBox
canonicalLBox_from_lBox (LBox (V2 x y) (V2 w h)) = r where
  fx = w < 0
  fy = h < 0
  r = CanonicalLBox fx fy $ make_lBox_from_axis (x, x+w, y, y+h)

-- | same as canonicalLBox_from_lBox but returns just the canonical LBox
canonicalLBox_from_lBox_ :: LBox -> LBox
canonicalLBox_from_lBox_ lbox = r where
  (CanonicalLBox _ _ r) = canonicalLBox_from_lBox lbox

lBox_from_canonicalLBox :: CanonicalLBox -> LBox
lBox_from_canonicalLBox (CanonicalLBox fx fy (LBox (V2 x y) (V2 w h))) = LBox (V2 x' y') (V2 w' h') where
  x' = if fx then x+w else x
  y' = if fy then y+h else y
  w' = if fx then -w else w
  h' = if fy then -h else h

deltaLBox_via_canonicalLBox :: CanonicalLBox -> DeltaLBox -> DeltaLBox
deltaLBox_via_canonicalLBox (CanonicalLBox fx fy _) DeltaLBox {..} = r where
  V2 tx ty = _deltaLBox_translate
  V2 sx sy = _deltaLBox_resizeBy
  (rtx, rsx) = if fx then (sx, tx) else (tx, sx)
  (rty, rsy) = if fy then (sy, ty) else (ty, sy)
  r = DeltaLBox (V2 rtx rty) (V2 rsx rsy)

lBox_isCanonicalLBox :: LBox -> Bool
lBox_isCanonicalLBox lbx = canonicalLBox_from_lBox_ lbx == lbx


class Delta x dx where
  plusDelta :: x -> dx -> x
  minusDelta :: x -> dx -> x

instance Delta XY XY where
  plusDelta = (+)
  minusDelta = (-)


newtype DeltaXY = DeltaXY XY deriving (Eq, Generic, Show)

instance NFData DeltaXY

instance Delta XY DeltaXY where
  plusDelta xy (DeltaXY dxy) = xy + dxy
  minusDelta xy (DeltaXY dxy) = xy - dxy

instance (Delta a c, Delta b d) => Delta (a,b) (c,d) where
  plusDelta (a,b) (c,d) = (plusDelta a c, plusDelta b d)
  minusDelta (a,b) (c,d) = (minusDelta a c, minusDelta b d)


-- TODO rename with _ in front
data DeltaLBox = DeltaLBox {
  _deltaLBox_translate  :: XY
  , _deltaLBox_resizeBy :: XY
}  deriving (Eq, Generic, Show)

instance NFData DeltaLBox

instance Delta LBox DeltaLBox where
  plusDelta LBox {..} DeltaLBox {..} = LBox {
      _lBox_tl = plusDelta _lBox_tl _deltaLBox_translate
      , _lBox_size = plusDelta _lBox_size _deltaLBox_resizeBy
    }
  minusDelta LBox {..} DeltaLBox {..} =  LBox {
      _lBox_tl = minusDelta _lBox_tl _deltaLBox_translate
      , _lBox_size = minusDelta _lBox_size _deltaLBox_resizeBy
    }
