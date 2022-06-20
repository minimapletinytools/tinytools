{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Methods.LineTypes where

import           Relude

import           Potato.Flow.Math
import           Potato.Flow.SElts

import qualified Data.Map as Map
import           Data.Maybe         (fromJust)
import Data.Default

import Linear.Vector ((^*))
import Linear.Matrix (M22, (!*))

import Control.Exception (assert)


data CartDir = CD_Up | CD_Down | CD_Left | CD_Right deriving (Eq, Generic, Show)
instance NFData CartDir


data AnchorType = AT_End_Up | AT_End_Down | AT_End_Left | AT_End_Right | AT_Elbow_TL | AT_Elbow_TR | AT_Elbow_BR | AT_Elbow_BL | AT_Elbow_Invalid deriving (Eq, Show)

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

cartDirWithDistanceToV2 :: (CartDir, Int, Bool) -> V2 Int
cartDirWithDistanceToV2 (cd, d, _) = cartDirToUnit cd ^* d


data LineAnchorsForRender = LineAnchorsForRender {
  _lineAnchorsForRender_start :: XY
  -- `Bool` parameter is whether we are at the start of a subsegment (i.e. a midpoint or endpoint)
  , _lineAnchorsForRender_rest :: [(CartDir, Int, Bool)]
} deriving (Show, Generic, Eq)

instance NFData LineAnchorsForRender

emptyLineAnchorsForRender :: LineAnchorsForRender
emptyLineAnchorsForRender = LineAnchorsForRender {
  _lineAnchorsForRender_start = 0
  , _lineAnchorsForRender_rest = []
}

-- NOTE our coordinate system is LEFT HANDED
--  --> +x
-- |
-- v
-- +y
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
