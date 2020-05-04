{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Potato.Flow.Math (
  XY
  , LBox(..)
  , nilLBox

  -- TODO rename this to lBox for consistency...
  , make_LBox_from_ul_br
  , does_LBox_contains_XY
  , lBox_area


  -- untested
  , make_LBox_from_axis
  , union_LBox
  , does_LBox_intersect

  -- untested
  -- these helpers maybe belong in a different file, they have very specific usages
  , CanonicalLBox(..)
  , canonicalLBox_from_lBox
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

-- TODO rename params maybe?
-- | a box in logical space
-- note size is non inclusive
-- e.g. an LBox with size (1,1) is exactly 1 point at ul
-- e.g. an LBox with size (0,0) contains nothing
data LBox = LBox {
  _lBox_ul     :: XY
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

make_LBox_from_ul_br :: XY -> XY -> LBox
make_LBox_from_ul_br (V2 x1 y1) (V2 x2 y2) =
  LBox {
    _lBox_ul= V2 (min x1 x2) (min y1 y2)
    , _lBox_size  = V2 (abs (x1 - x2)) (abs (y1 - y2))
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

union_LBox :: LBox -> LBox -> LBox
union_LBox (LBox (V2 x1 y1) (V2 w1 h1)) (LBox (V2 x2 y2) (V2 w2 h2)) = r where
  cx1 = x1 + w1
  cy1 = y1 + h1
  cx2 = x2 + w2
  cy2 = y2 + h2
  r = make_LBox_from_axis (cx1, cx2, cy1, cy2)

does_LBox_intersect :: LBox -> LBox -> Bool
does_LBox_intersect lb1 lb2 = r where
  (l1,r1,t1,b1) = lBox_to_axis lb1
  (l2,r2,t2,b2) = lBox_to_axis lb2
  r | l1 >= r2 = False
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
}  deriving (Eq, Show)

instance Delta LBox DeltaLBox where
  plusDelta LBox {..} DeltaLBox {..} = LBox {
      _lBox_ul = plusDelta _lBox_ul _deltaLBox_translate
      , _lBox_size = plusDelta _lBox_size _deltaLBox_resizeBy
    }
  minusDelta LBox {..} DeltaLBox {..} =  LBox {
      _lBox_ul = minusDelta _lBox_ul _deltaLBox_translate
      , _lBox_size = minusDelta _lBox_size _deltaLBox_resizeBy
    }

type DeltaText = (Text,Text)
-- TODO more efficient to do this with zippers prob?
-- is there a way to make this more generic?
instance Delta Text DeltaText where
  plusDelta s (b, a) = assert (b == s) a
  minusDelta s (b, a) = assert (a == s) b
