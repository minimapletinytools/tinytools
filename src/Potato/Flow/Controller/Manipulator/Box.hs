{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Manipulator.Box (
  BoxHandleType(..)
  , makeHandleBox
  , makeDeltaBox
  , MouseManipulator(..)
  , toMouseManipulators
) where

import           Relude

import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input
import           Potato.Flow.Entry
import           Potato.Flow.Math
import           Potato.Flow.SEltMethods
import           Potato.Flow.SElts
import           Potato.Flow.Types

import           Control.Exception              (assert)
import           Data.Default
import           Data.Dependent.Sum             (DSum ((:=>)))
import qualified Data.IntMap                    as IM
import qualified Data.List                      as L
import qualified Data.Sequence                  as Seq
import           Data.Tuple.Extra



computeSelectionType :: Selection -> SelectionManipulatorType
computeSelectionType = foldl' foldfn SMTNone where
  foldfn accType (_,_,SEltLabel _ selt) = case accType of
    SMTNone -> case selt of
      SEltBox _  -> SMTBox
      SEltLine _ -> SMTLine
      SEltText _ -> SMTText
      _          -> SMTBoundingBox
    _ -> SMTBoundingBox


-- TODO rework this stuff
data MouseManipulatorType = MouseManipulatorType_Corner | MouseManipulatorType_Side | MouseManipulatorType_Point | MouseManipulatorType_Area | MouseManipulatorType_Text deriving (Show, Eq)
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
      Just (_, _, SEltLabel _ selt) -> case selt of
        SEltBox SBox {..}   -> fmap (flip makeHandleBox _sBox_box) [BH_TL .. BH_A]
        SEltLine SLine {..} -> undefined
          --_sLine_start
          --_sLine_end
        SEltText SText {..} -> fmap (flip makeHandleBox _sText_box) [BH_TL .. BH_A]
          -- add at end to preserve indexing of [BH_TL .. BH_A]
          <> [(makeHandleBox BH_A _sText_box) { _mouseManipulator_type = MouseManipulatorType_Text }]
        _                   -> []
  else bb where
    union_LBoxes :: NonEmpty LBox -> LBox
    union_LBoxes (x:|xs) = foldl' union_LBox x xs
    fmapfn (_, _, seltl) = do
      box <- getSEltBox . _sEltLabel_sElt $ seltl
      return box
    msboxes = fmap fmapfn selection
    sboxes = catMaybes (toList msboxes)
    bb = case sboxes of
      [] -> []
      x:xs  -> fmap (flip makeHandleBox (union_LBoxes (x:|xs))) [BH_TL .. BH_A]

findFirstMouseManipulator :: RelMouseDrag -> Selection -> Maybe ManipulatorIndex
findFirstMouseManipulator (RelMouseDrag MouseDrag {..}) selection = r where
  mms = toMouseManipulators selection
  smt = computeSelectionType selection
  normalSel = L.findIndex (\mm -> does_LBox_contains_XY (_mouseManipulator_box mm) _mouseDrag_from) mms
  r = case smt of
    SMTText -> normalSel -- TODO figure out how to differentiate between area / text manipulator
    _       -> normalSel


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



-- new handler stuff
data BoxHandler = BoxHandler {

    _boxHandler_handle       :: BoxHandleType -- the current handle we are dragging
    , _boxHandler_undoFirst  :: Bool

    -- with this you can use same code for both create and manipulate (create the handler and immediately pass input to it)
    , _boxHandler_isCreation :: Bool
  }

instance PotatoHandler BoxHandler where
  pHandlerName _ = "BoxHandler"
  pHandleMouse bh@BoxHandler {..} PotatoHandlerInput {..} rmd = let
      pfs = _potatoHandlerInput_pFState
      selection = _potatoHandlerInput_selection
      RelMouseDrag MouseDrag {..} = rmd
      dragDelta = _mouseDrag_to - _mouseDrag_from
      shiftClick = elem KeyModifier_Shift _mouseDrag_modifiers
    in case _mouseDrag_state of
        MouseDragState_Down -> Just r where

          mmi = findFirstMouseManipulator rmd selection
          r = case mmi of
            -- didn't click on a manipulator, so cancel everything and pass on the state
            Nothing -> (Nothing, Nothing, Nothing)
            Just mi -> (Just (SomePotatoHandler newbh), Nothing, Nothing) where
              newbh = bh {
                  _boxHandler_handle = toEnum mi
                }

        MouseDragState_Dragging -> Just (Just (SomePotatoHandler newbh), Nothing, Just op) where
          -- TODO may change handle when dragging
          --(m, mi) = continueManipulate _mouseDrag_to lastmi smt mms
          m = _boxHandler_handle

          boxRestrictedDelta = if shiftClick
            then restrict8 dragDelta
            else dragDelta
          selectedBox = assert (computeSelectionType selection == SMTBox) $ Seq.index selection 0

          -- for creating new elt
          lastSelectionLps = fmap snd3 $ selection
          newEltPos = if Seq.null lastSelectionLps then 0 else L.minimum lastSelectionLps

          -- for manipulate
          controller = CTagBox :=> (Identity $ CBox {
              _cBox_deltaBox = Just $ makeDeltaBox m boxRestrictedDelta
              , _cBox_deltaStyle = Nothing
            })

          op = if _boxHandler_isCreation
            then PFEAddElt (_boxHandler_undoFirst, (newEltPos, SEltLabel "<box>" $ SEltBox $ SBox (LBox (_mouseDrag_from) (dragDelta)) def))
            else PFEManipulate (_boxHandler_undoFirst, IM.fromList (fmap (,controller) (toList . fmap fst3 $ selection)))

          newbh = bh { _boxHandler_undoFirst = True }

        MouseDragState_Up -> Just (Nothing, Nothing, Nothing)
          -- TODO consider handling special case, handle when you click and release create a box in one spot, create a box that has size 1 (rather than 0 if we did it during MouseDragState_Down normal way)

        MouseDragState_Cancelled -> error "unexpected mouse state passed to handler"

  -- TODO keyboard movement
  pHandleKeyboard _ _ _ = Nothing

  pHandleKeyboard _ _ _ = Nothing

  pValidateMouse _ (RelMouseDrag MouseDrag {..}) = case _mouseDrag_state of
    MouseDragState_Down      -> False
    MouseDragState_Cancelled -> False
    _                        -> True
