{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Potato.Flow.Math (
  XY
  , LBox(..)
  , nilLBox

  -- TODO rename this to lBox for consistency...
  , make_LBox_from_XYs
  , does_LBox_contains_XY
  , lBox_area

  , make_LBox_from_axis
  , union_LBox
  , intersect_LBox
  , does_LBox_intersect

  -- these helpers maybe belong in a different file, they have very specific usages
  , CanonicalLBox(..)
  , canonicalLBox_from_lBox
  , canonicalLBox_from_lBox_
  , lBox_from_canonicalLBox
  , deltaLBox_via_canonicalLBox

  , Delta(..)
  , DeltaLBox(..)
  , DeltaText

  , module Linear.V2
) where

import           Relude

import           Control.Exception (assert)
import           Data.Aeson
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
instance NFData LBox

nilLBox :: LBox
nilLBox = LBox 0 0

lBox_area :: LBox -> Int
lBox_area (LBox (V2 _ _) (V2 w h)) = w*h

-- | always returns a canonical LBox
make_LBox_from_XYs :: XY -> XY -> LBox
make_LBox_from_XYs (V2 x1 y1) (V2 x2 y2) =
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

does_LBox_contains_XY :: LBox -> XY -> Bool
does_LBox_contains_XY (LBox (V2 bx by) (V2 bw bh)) (V2 px py) =
  px >= bx && py >= by && px < (bx + bw) && py < (by + bh)

-- | right and bottom axis are non-inclusive
make_LBox_from_axis :: (Int, Int, Int, Int) -> LBox
make_LBox_from_axis (x1,x2,y1,y2) = LBox (V2 rx ry) (V2 rw rh) where
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
union_LBox :: LBox -> LBox -> LBox
union_LBox (LBox (V2 x1 y1) (V2 w1 h1)) (LBox (V2 x2 y2) (V2 w2 h2)) = combined where
  cx1 = x1 + w1
  cy1 = y1 + h1
  cx2 = x2 + w2
  cy2 = y2 + h2
  combined = make_LBox_from_axis (min4 x1 cx1 x2 cx2, max4 x1 cx1 x2 cx2, min4 y1 cy1 y2 cy2, max4 y1 cy1 y2 cy2)

-- | inverted LBox are treated as if not inverted
intersect_LBox :: LBox -> LBox -> Maybe LBox
intersect_LBox lb1@(LBox (V2 x1 y1) (V2 w1 h1)) lb2@(LBox (V2 x2 y2) (V2 w2 h2)) = r where
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
  r = if does_LBox_intersect lb1 lb2
    then Just $ make_LBox_from_axis (max l1 l2, min r1 r2, max t1 t2, min b1 b2)
    else Nothing

does_LBox_intersect :: LBox -> LBox -> Bool
does_LBox_intersect lb1 lb2 = r where
  (l1,r1,t1,b1) = lBox_to_axis lb1
  (l2,r2,t2,b2) = lBox_to_axis lb2
  r | lBox_area lb1 == 0 = False
    | lBox_area lb2 == 0 = False
    | l1 >= r2 = False
    | l2 >= r1 = False
    | t1 >= b2 = False
    | t2 >= b1 = False
    | otherwise = True

-- | CanonicalLBox is always has non-negative width/height
-- and tracks which axis are flipped to return back to original LBox
-- first Bool is if x values are flipped, second is for y
data CanonicalLBox = CanonicalLBox Bool Bool LBox

canonicalLBox_from_lBox :: LBox -> CanonicalLBox
canonicalLBox_from_lBox (LBox (V2 x y) (V2 w h)) = r where
  fx = w < 0
  fy = h < 0
  r = CanonicalLBox fx fy $ make_LBox_from_axis (x, x+w, y, y+h)

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



class Delta x dx where
  plusDelta :: x -> dx -> x
  minusDelta :: x -> dx -> x

instance Delta XY XY where
  plusDelta = (+)
  minusDelta = (-)

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

-- | (old text, new text)
type DeltaText = (Text,Text)
-- TODO more efficient to do this with zippers prob?
-- is there a way to make this more generic?
instance Delta Text DeltaText where
  plusDelta s (b, a) = assert (b == s) a
  minusDelta s (b, a) = assert (a == s) b
