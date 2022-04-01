{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Manipulator.Common (
  SelectionManipulatorType(..)
  , computeSelectionType
  , restrict4
  , restrict8
  , selectionToSuperOwl
  , selectionToMaybeSuperOwl
  , selectionToFirstSuperOwl
  , selectionToMaybeFirstSuperOwl
  , lastPositionInSelection
) where

import           Relude

import           Potato.Flow.Controller.Types
import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.OwlItem
import Potato.Flow.Owl
import Potato.Flow.DebugHelpers

import qualified Data.Sequence                as Seq

data SelectionManipulatorType = SMTNone | SMTBox | SMTBoxText | SMTLine | SMTTextArea | SMTBoundingBox deriving (Show, Eq)

computeSelectionType :: CanvasSelection -> SelectionManipulatorType
computeSelectionType (CanvasSelection selection)= foldl' foldfn SMTNone selection where
  foldfn accType sowl = case accType of
    SMTNone -> case superOwl_toSElt_hack sowl of
      SEltBox sbox -> if sBoxType_isText (_sBox_boxType sbox) then SMTBoxText else SMTBox
      SEltLine _   -> SMTLine
      SEltTextArea _   -> SMTTextArea
      SEltFolderStart -> error "this should never happen by assumption of CanvasSelection type"
      SEltFolderEnd -> error "this should never happen by assumption of CanvasSelection type"
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

selectionToSuperOwl :: (HasCallStack) => CanvasSelection -> SuperOwl
selectionToSuperOwl (CanvasSelection selection) = assertShowAndDump selection (Seq.length selection == 1) $ Seq.index selection 0

selectionToMaybeSuperOwl :: (HasCallStack) => CanvasSelection -> Maybe SuperOwl
selectionToMaybeSuperOwl (CanvasSelection selection) = assertShowAndDump selection (Seq.length selection <= 1) $ Seq.lookup 0 selection

selectionToFirstSuperOwl :: (HasCallStack) => CanvasSelection -> SuperOwl
selectionToFirstSuperOwl (CanvasSelection selection) = assertShowAndDump selection (Seq.length selection > 0) $ Seq.index selection 0

selectionToMaybeFirstSuperOwl :: CanvasSelection -> Maybe SuperOwl
selectionToMaybeFirstSuperOwl (CanvasSelection selection) = Seq.lookup 0 selection

lastPositionInSelection :: OwlTree -> Selection -> OwlSpot
lastPositionInSelection ot (SuperOwlParliament selection) = r where
  r = case Seq.lookup (Seq.length selection - 1) selection of
    Nothing -> topSpot
    Just x -> owlTree_owlEltMeta_toOwlSpot ot (_superOwl_meta x)
