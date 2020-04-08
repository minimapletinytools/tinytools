module Potato.Flow.Math (
  XY(..), X(..), Y(..)
  , zeroXY
  , LSize(..)
  , LPoint(..)
  , VPoint(..)
  , PFTrans(..)
  , LBox(..)
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
newtype VPoint = VPoint (Int, Int) deriving (Generic, Show, FromJSON, ToJSON)

-- | a point in work space
data PFTrans = PFTrans {
  logical   :: LPoint
  , virtual :: VPoint
} deriving (Generic, Show)

instance FromJSON PFTrans
instance ToJSON PFTrans

-- | a box in logical space
data LBox = LBox {
  ul     :: LPoint
  , size :: LSize
} deriving (Generic, Show)

instance FromJSON LBox
instance ToJSON LBox
