{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}

module Potato.Flow.Math (
  XY(..), X(..), Y(..)
  , zeroXY
  , LSize(..)
  , LPoint(..)
  , LBox(..)

  , Delta(..)
  , DeltaLBox(..)
) where

import           Relude

import           Data.Aeson

-- TODO switch to math library
newtype XY = XY { unXY :: (Int, Int) } deriving (Generic, Show, FromJSON, ToJSON)
newtype X = X { unX :: Int } deriving (Generic, Show, FromJSON, ToJSON)
newtype Y = Y { unY :: Int } deriving (Generic, Show, FromJSON, ToJSON)

zeroXY :: XY
zeroXY = XY (0,0)

newtype LSize = LSize { unLSize :: XY } deriving (Generic, Show, FromJSON, ToJSON)
newtype LPoint = LPoint { unLPoint :: XY } deriving (Generic, Show, FromJSON, ToJSON)

-- | a point in screen space
-- should only be used by VC, so does not belong here
--newtype VPoint = VPoint (Int, Int) deriving (Generic, Show, FromJSON, ToJSON)

-- | a box in logical space
data LBox = LBox {
  ul     :: LPoint
  , size :: LSize
} deriving (Generic, Show)

instance FromJSON LBox
instance ToJSON LBox

class Delta x where
  type DeltaType x :: Type
  plusDelta :: x -> DeltaType x -> x
  minusDelta :: x -> DeltaType x -> x

instance Delta Int where
  type DeltaType Int = Int
  plusDelta = (+)
  minusDelta = (-)

instance (Delta a, Delta b) => Delta (a,b) where
  type DeltaType (a,b) = (DeltaType a, DeltaType b)
  plusDelta (a,b) (c,d) = (plusDelta a c, plusDelta b d)
  minusDelta (a,b) (c,d) = (minusDelta a c, minusDelta b d)

deriving instance Delta X
deriving instance Delta Y

instance Delta XY where
  type DeltaType XY = XY
  plusDelta xy d = XY $ plusDelta (unXY xy) (unXY d)
  minusDelta xy d = XY $ minusDelta (unXY xy) (unXY d)

deriving instance Delta LPoint
deriving instance Delta LSize

data DeltaLBox = DeltaLBox {
  deltaLBox_translate  :: XY
  , deltaLBox_resizeBy :: XY
}
instance Delta LBox where
  type DeltaType LBox = DeltaLBox
  plusDelta LBox {..} DeltaLBox {..} = LBox {
      ul = plusDelta ul deltaLBox_translate
      , size = plusDelta size deltaLBox_resizeBy
    }
  minusDelta LBox {..} DeltaLBox {..} =  LBox {
      ul = minusDelta ul deltaLBox_translate
      , size = minusDelta size deltaLBox_resizeBy
    }
