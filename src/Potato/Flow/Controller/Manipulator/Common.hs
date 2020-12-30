{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Manipulator.Common (
  SelectionManipulatorType(..)
  , computeSelectionType
  , restrict4
  , restrict8
  , selectionToSuperSEltLabel
  , lastPositionInSelection
) where

import           Relude

import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.Types
import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.Types

import           Control.Exception
import qualified Data.List                    as L
import qualified Data.Sequence                as Seq
import           Data.Tuple.Extra

data SelectionManipulatorType = SMTNone | SMTBox | SMTLine | SMTText | SMTBoundingBox deriving (Show, Eq)

computeSelectionType :: Selection -> SelectionManipulatorType
computeSelectionType = foldl' foldfn SMTNone where
  foldfn accType (_,_,SEltLabel _ selt) = case accType of
    SMTNone -> case selt of
      SEltBox _  -> SMTBox
      SEltLine _ -> SMTLine
      SEltText _ -> SMTText
      -- TODO
      --SEltFolderStart _ -> SMTNone
      --SEltFolderEnd _ -> SMTNone
      --SEltNone -> SMTNone
      _          -> SMTBoundingBox
    _ -> SMTBoundingBox

restrict4 :: XY -> XY
restrict4 (V2 x y) = if abs x > abs y then V2 x 0 else V2 0 y

restrict8 :: XY -> XY
restrict8 (V2 x y) = r where
  normx = abs x
  normy = abs y
  r = if normx > normy
    then if normx*2 > normy
      then (V2 x 0)
      else (V2 x y)
    else if normy*2 > normx
      then (V2 0 y)
      else (V2 x y)

selectionToSuperSEltLabel :: Selection -> SuperSEltLabel
selectionToSuperSEltLabel selection = assert (Seq.length selection == 1) $ Seq.index selection 0

lastPositionInSelection :: Selection -> LayerPos
lastPositionInSelection selection = r where
  lastSelectionLps = fmap snd3 $ selection
  r = if Seq.null lastSelectionLps then 0 else L.minimum lastSelectionLps
