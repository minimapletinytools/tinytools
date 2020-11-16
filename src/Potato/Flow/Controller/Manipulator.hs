{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.Manipulator (
  BoxHandleType(..)
  , makeHandleBox
  , makeDeltaBox
  , ManipulatorIndex
  , MouseManipulator(..)
  , MouseManipulatorSet
  , toMouseManipulators
  , findFirstMouseManipulator
  , newManipulate

) where

import           Relude

import           Potato.Flow.BroadPhase
import           Potato.Flow.Controller.Input
import           Potato.Flow.Math
import           Potato.Flow.Render
import           Potato.Flow.SEltMethods
import           Potato.Flow.SElts
import           Potato.Flow.State
import           Potato.Flow.Types

-- erhm, maybe move PFEventTag to somewhere else? Could just duplicate it in this file
import           Potato.Flow.Entry            (PFEventTag)

import           Control.Exception            (assert)
import           Data.Dependent.Sum           (DSum ((:=>)), (==>))
import qualified Data.IntMap                  as IM
import qualified Data.List                    as L
import qualified Data.Sequence                as Seq
import           Data.Tuple.Extra
import           Potato.Flow.Entry



computeSelectionType :: Selection -> SelectionManipulatorType
computeSelectionType = foldl' foldfn SMTNone where
  foldfn accType (_,_,SEltLabel _ selt) = case accType of
    SMTNone -> case selt of
      SEltBox _  -> SMTBox
      SEltLine _ -> SMTLine
      SEltText _ -> SMTText
      _          -> SMTNone
    _ -> SMTBoundingBox


data MouseManipulatorType = MouseManipulatorType_Corner | MouseManipulatorType_Side | MouseManipulatorType_Point | MouseManipulatorType_Area deriving (Show, Eq)

data MouseManipulator = MouseManipulator {
  _mouseManipulator_box    :: LBox
  , _mouseManipulator_type :: MouseManipulatorType
  -- back reference to object being manipulated?
  -- or just use a function
}

type MouseManipulatorSet = [MouseManipulator]
type ManipulatorIndex = Int

-- TODO finish
toMouseManipulators :: Selection -> MouseManipulatorSet
toMouseManipulators selection = if Seq.length selection > 1
  then
    case Seq.lookup 0 selection of
      Nothing -> []
      Just (rid, _, SEltLabel _ selt) -> case selt of
        SEltBox SBox {..}   -> fmap (flip makeHandleBox _sBox_box) [BH_TL .. BH_A]
        SEltLine SLine {..} -> undefined
          --_sLine_start
          --_sLine_end
        SEltText SText {..} -> undefined
          --_sText_box
          --_sText_text
        _                   -> []
  else bb where
    union_LBoxes :: NonEmpty LBox -> LBox
    union_LBoxes (x:|xs) = foldl' union_LBox x xs
    fmapfn (rid, _, seltl) = do
      box <- getSEltBox . _sEltLabel_sElt $ seltl
      return box
    msboxes = fmap fmapfn selection
    sboxes = catMaybes (toList msboxes)
    bb = case sboxes of
      [] -> []
      x:xs  -> fmap (flip makeHandleBox (union_LBoxes (x:|xs))) [BH_TL .. BH_A]

-- questionable manipulator helper functions
findFirstMouseManipulator :: XY -> MouseManipulatorSet -> Maybe ManipulatorIndex
findFirstMouseManipulator pos = L.findIndex (\mm -> does_LBox_contains_XY (_mouseManipulator_box mm) pos)

newManipulate :: RelMouseDrag -> Selection -> ManipulatorIndex -> Bool -> (ManipulatorIndex, PFEventTag)
newManipulate (RelMouseDrag MouseDrag {..}) selection lastmi undoFirst =  (mi, op) where
  mms = toMouseManipulators selection
  smt = computeSelectionType selection
  (m, mi) = continueManipulate _mouseDrag_to lastmi smt mms
  dragDelta = _mouseDrag_to - _mouseDrag_from

  -- TODO conisder embedding in MouseManipulator instead of using switch statement below
  op = case smt of
    SMTBox -> PFEManipulate (undoFirst, IM.fromList (fmap (,controller) (toList . fmap fst3 $ selection))) where
          controller = CTagBox :=> (Identity $ CBox {
              _cBox_deltaBox = makeDeltaBox (toEnum mi) dragDelta
        })
    SMTBoundingBox -> PFEManipulate (undoFirst, IM.fromList (fmap (,controller) (toList . fmap fst3 $ selection))) where
          -- TODO scaling rather than absolute if modifier is held?
          controller = CTagBoundingBox :=> (Identity $ CBoundingBox {
              _cBoundingBox_deltaBox = makeDeltaBox (toEnum mi) dragDelta
        })
    _ -> undefined


continueManipulate :: XY -> ManipulatorIndex -> SelectionManipulatorType ->  MouseManipulatorSet -> (MouseManipulator, ManipulatorIndex)
continueManipulate pos mi smt mms = let
    boxRules = undefined -- TODO rules for choosing box manipulator
  in case smt of
    _ -> (mms L.!! mi, mi)
    -- TODO specific rulse for each
    --SMTBox         -> undefined
    --SMTLine        -> undefined
    --SMTText        -> undefined
    --SMTBoundingBox -> undefined
    --_              -> error "unknown selection type"


-- BOX MANIPULATOR STUFF
-- TODO move to diff file
-- order is manipulator index
data BoxHandleType = BH_TL | BH_TR | BH_BL | BH_BR | BH_A | BH_T | BH_B | BH_L | BH_R  deriving (Show, Eq, Enum)

makeHandleBox ::
  BoxHandleType
  -> LBox -- ^ box being manipulated
  -> MouseManipulator
makeHandleBox bht (LBox (V2 x y) (V2 w h)) = case bht of
  BH_BR -> MouseManipulator box MouseManipulatorType_Corner
  BH_TL -> MouseManipulator box MouseManipulatorType_Corner
  BH_TR -> MouseManipulator box MouseManipulatorType_Corner
  BH_BL -> MouseManipulator box MouseManipulatorType_Corner
  BH_A  -> MouseManipulator box MouseManipulatorType_Area
  _     -> MouseManipulator box MouseManipulatorType_Side
  where
    (px, py) = (0,0) -- pan position
    CanonicalLBox _ _ clbox = canonicalLBox_from_lBox $ LBox (V2 (x+px) (y+py)) (V2 w h)
    nudgex = if w < 0 then 1 else 0
    nudgey = if h < 0 then 1 else 0
    l = x+px-1 + nudgex
    t = y+py-1 + nudgey
    r = x+px+w - nudgex
    b = y+py+h - nudgey
    box = case bht of
      BH_BR -> LBox (V2 r b) (V2 1 1)
      BH_TL -> LBox (V2 l t) (V2 1 1)
      BH_TR -> LBox (V2 r t) (V2 1 1)
      BH_BL -> LBox (V2 l b) (V2 1 1)
      BH_A  -> clbox
      _     -> error "not supported yet"

makeDeltaBox :: BoxHandleType -> XY -> DeltaLBox
makeDeltaBox bht (V2 dx dy) = case bht of
  BH_BR -> DeltaLBox 0 $ V2 dx dy
  BH_TL -> DeltaLBox (V2 dx dy) (V2 (-dx) (-dy))
  BH_TR -> DeltaLBox (V2 0 dy) (V2 dx (-dy))
  BH_BL -> DeltaLBox (V2 dx 0) (V2 (-dx) dy)
  BH_T  -> DeltaLBox (V2 0 dy) (V2 0 (-dy))
  BH_B  -> DeltaLBox 0 (V2 0 dy)
  BH_L  -> DeltaLBox (V2 dx 0) (V2 (-dx) 0)
  BH_R  -> DeltaLBox 0 (V2 dx 0)
  BH_A  -> DeltaLBox (V2 dx dy) (V2 0 0)
