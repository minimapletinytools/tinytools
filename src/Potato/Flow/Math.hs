{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Potato.Flow.Math (
  XY
  , LBox(..)
  , nilLBox
  , make_LBox_from_ul_br
  , does_LBox_contains_XY
  , union_LBox
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


make_LBox_from_ul_br :: XY -> XY -> LBox
make_LBox_from_ul_br (V2 x1 y1) (V2 x2 y2) =
  LBox {
    _lBox_ul= V2 (min x1 x2) (min y1 y2)
    , _lBox_size  = V2 (abs (x1 - x2)) (abs (y1 - y2))
  }

does_LBox_contains_XY :: LBox -> XY -> Bool
does_LBox_contains_XY (LBox (V2 bx by) (V2 bw bh)) (V2 px py) =
  px >= bx && py >= by && px < (bx + bw) && py < (by + bh)

-- TODO
union_LBox :: LBox -> LBox -> LBox
union_LBox lb1 = const lb1

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
  deltaLBox_translate  :: XY
  , deltaLBox_resizeBy :: XY
}  deriving (Eq, Show)

instance Delta LBox DeltaLBox where
  plusDelta LBox {..} DeltaLBox {..} = LBox {
      _lBox_ul = plusDelta _lBox_ul deltaLBox_translate
      , _lBox_size = plusDelta _lBox_size deltaLBox_resizeBy
    }
  minusDelta LBox {..} DeltaLBox {..} =  LBox {
      _lBox_ul = minusDelta _lBox_ul deltaLBox_translate
      , _lBox_size = minusDelta _lBox_size deltaLBox_resizeBy
    }

type DeltaText = (Text,Text)
-- TODO more efficient to do this with zippers prob?
-- is there a way to make this more generic?
instance Delta Text DeltaText where
  plusDelta s (b, a) = assert (b == s) a
  minusDelta s (b, a) = assert (a == s) b
