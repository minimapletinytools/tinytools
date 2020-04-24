{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Potato.Flow.Math (
  XY
  , LSize(..)
  , LPoint(..)
  , LBox(..)
  , nilLBox
  , make_LBox_from_LPoints
  , does_LBox_contains_LPoint
  , union_LBox
  , Delta(..)
  , DeltaLBox(..)
  , DeltaText

  , module Linear.V2
) where

import           Relude

import           Data.Aeson
import           Linear.V2

import           Control.Exception (assert)
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

newtype LSize = LSize { unLSize :: XY } deriving (Eq, Ord, Num, Generic, Show, FromJSON, ToJSON)
newtype LPoint = LPoint { unLPoint :: XY } deriving (Eq, Ord, Num, Generic, Show, FromJSON, ToJSON)

-- | a point in screen space
-- should only be used by VC, so does not belong here
--newtype VPoint = VPoint (Int, Int) deriving (Generic, Show, FromJSON, ToJSON)

-- TODO rename params maybe?
-- | a box in logical space
-- note size is non inclusive
-- e.g. an LBox with size (1,1) is exactly 1 point at ul
-- e.g. an LBox with size (0,0) contains nothing
data LBox = LBox {
  ul     :: LPoint
  , size :: LSize
} deriving (Eq, Generic)

instance Show LBox where
  show (LBox (LPoint (V2 x y)) (LSize (V2 w h))) = "LBox: " <> show x <> " " <> show y <> " " <> show w <> " " <> show h

nilLBox :: LBox
nilLBox = LBox 0 0


make_LBox_from_LPoints :: LPoint -> LPoint -> LBox
make_LBox_from_LPoints (LPoint (V2 x1 y1)) (LPoint (V2 x2 y2)) =
  LBox {
    ul = LPoint $ V2 (min x1 x2) (min y1 y2)
    , size = LSize $ V2 (abs (x1 - x2)) (abs (y1 - y2))
  }

does_LBox_contains_LPoint :: LBox -> LPoint -> Bool
does_LBox_contains_LPoint (LBox (LPoint (V2 bx by)) (LSize (V2 bw bh))) (LPoint (V2 px py)) =
  px >= bx && py >= by && px < (bx + bw) && py < (by + bh)

-- TODO
union_LBox :: LBox -> LBox -> LBox
union_LBox lb1 = const lb1

instance FromJSON LBox
instance ToJSON LBox

class Delta x dx where
  plusDelta :: x -> dx -> x
  minusDelta :: x -> dx -> x

instance Delta XY XY where
  plusDelta = (+)
  minusDelta = (-)

instance (Delta a c, Delta b d) => Delta (a,b) (c,d) where
  plusDelta (a,b) (c,d) = (plusDelta a c, plusDelta b d)
  minusDelta (a,b) (c,d) = (minusDelta a c, minusDelta b d)


instance Delta LPoint LPoint where
  plusDelta = (+)
  minusDelta = (-)

instance Delta LSize LSize where
  plusDelta = (+)
  minusDelta = (-)

data DeltaLBox = DeltaLBox {
  deltaLBox_translate  :: LPoint
  , deltaLBox_resizeBy :: LSize
}

instance Delta LBox DeltaLBox where
  plusDelta LBox {..} DeltaLBox {..} = LBox {
      ul = plusDelta ul deltaLBox_translate
      , size = plusDelta size deltaLBox_resizeBy
    }
  minusDelta LBox {..} DeltaLBox {..} =  LBox {
      ul = minusDelta ul deltaLBox_translate
      , size = minusDelta size deltaLBox_resizeBy
    }

type DeltaText = (Text,Text)
-- TODO more efficient to do this with zippers prob?
-- is there a way to make this more generic?
instance Delta Text DeltaText where
  plusDelta s (b, a) = assert (b == s) a
  minusDelta s (b, a) = assert (a == s) b
