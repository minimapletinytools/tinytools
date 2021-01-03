{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Manipulator.Common (
  SelectionManipulatorType(..)
  , computeSelectionType
  , restrict4
  , restrict8
  , selectionToSuperSEltLabel
  , selectionToMaybeSuperSEltLabel
  , selectionToFirstSuperSEltLabel
  , selectionToMaybeFirstSuperSEltLabel
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

data SelectionManipulatorType = SMTNone | SMTBox | SMTBoxText | SMTLine | SMTText | SMTBoundingBox deriving (Show, Eq)

computeSelectionType :: Selection -> SelectionManipulatorType
computeSelectionType = foldl' foldfn SMTNone where
  foldfn accType (_,_,SEltLabel _ selt) = case accType of
    SMTNone -> case selt of
      SEltBox sbox -> if sBoxType_isText (_sBox_boxType sbox) then SMTBoxText else SMTBox
      SEltLine _   -> SMTLine
      SEltTextArea _   -> SMTText
      -- TODO
      --SEltFolderStart _ -> SMTNone
      --SEltFolderEnd _ -> SMTNone
      --SEltNone -> SMTNone
      _            -> SMTBoundingBox
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

selectionToMaybeSuperSEltLabel :: Selection -> Maybe SuperSEltLabel
selectionToMaybeSuperSEltLabel selection = assert (Seq.length selection <= 1) $ Seq.lookup 0 selection

selectionToFirstSuperSEltLabel :: Selection -> SuperSEltLabel
selectionToFirstSuperSEltLabel selection = assert (Seq.length selection > 0) $ Seq.index selection 0

selectionToMaybeFirstSuperSEltLabel :: Selection -> Maybe SuperSEltLabel
selectionToMaybeFirstSuperSEltLabel selection = Seq.lookup 0 selection

lastPositionInSelection :: Selection -> LayerPos
lastPositionInSelection selection = r where
  lastSelectionLps = fmap snd3 $ selection
  r = if Seq.null lastSelectionLps then 0 else L.minimum lastSelectionLps
