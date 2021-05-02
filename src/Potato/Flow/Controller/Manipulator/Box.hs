{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Manipulator.Box (
  computeSelectionType
  , BoxHandleType(..)
  , BoxHandler(..)
  , BoxCreationType(..)
  , makeHandleBox
  , makeDeltaBox
  --, MouseManipulator(..)
) where

import           Relude

import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.Manipulator.BoxText
import           Potato.Flow.Controller.Manipulator.Common
import           Potato.Flow.Controller.Types
import           Potato.Flow.Math
import           Potato.Flow.SEltMethods
import           Potato.Flow.SElts
import           Potato.Flow.Types
import           Potato.Flow.Deprecated.Workspace

import           Data.Default
import           Data.Dependent.Sum                         (DSum ((:=>)))
import qualified Data.IntMap                                as IM
import qualified Data.List                                  as L
import qualified Data.Sequence                              as Seq
import           Data.Tuple.Extra

-- TODO rework this stuff, it was written with old assumptions that don't make sense anymore
data MouseManipulatorType = MouseManipulatorType_Corner | MouseManipulatorType_Side | MouseManipulatorType_Point | MouseManipulatorType_Area | MouseManipulatorType_Text deriving (Show, Eq)
data MouseManipulator = MouseManipulator {
  _mouseManipulator_box    :: LBox
  , _mouseManipulator_type :: MouseManipulatorType
  -- back reference to object being manipulated?
  -- or just use a function
}
type MouseManipulatorSet = [MouseManipulator]
type ManipulatorIndex = Int

toMouseManipulators :: Selection -> MouseManipulatorSet
toMouseManipulators selection = bb where
  union_lBoxes :: NonEmpty LBox -> LBox
  union_lBoxes (x:|xs) = foldl' union_lBox x xs
  fmapfn (_, _, seltl) = do
    box <- getSEltBox . _sEltLabel_sElt $ seltl
    return box
  msboxes = fmap fmapfn selection
  sboxes = catMaybes (toList msboxes)
  bb = case sboxes of
    []   -> []
    x:xs -> fmap (flip makeHandleBox (union_lBoxes (x:|xs))) [BH_TL .. BH_A]

findFirstMouseManipulator :: RelMouseDrag -> Selection -> Maybe ManipulatorIndex
findFirstMouseManipulator (RelMouseDrag MouseDrag {..}) selection = r where
  mms = toMouseManipulators selection
  smt = computeSelectionType selection
  normalSel = L.findIndex (\mm -> does_lBox_contains_XY (_mouseManipulator_box mm) _mouseDrag_from) mms
  r = case smt of
    SMTText -> normalSel -- TODO figure out how to differentiate between area / text manipulator
    _       -> normalSel


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



-- TODO rename to BoxHandlerType or something
data BoxCreationType = BoxCreationType_None | BoxCreationType_Box | BoxCreationType_Text | BoxCreationType_DragSelect deriving (Show, Eq)

boxCreationType_isCreation :: BoxCreationType -> Bool
boxCreationType_isCreation bct = bct /= BoxCreationType_None && bct /= BoxCreationType_DragSelect

-- new handler stuff
data BoxHandler = BoxHandler {

    _boxHandler_handle      :: BoxHandleType -- the current handle we are dragging, TODO should this be Maybe BoxHandleType?
    , _boxHandler_undoFirst :: Bool

    -- with this you can use same code for both create and manipulate (create the handler and immediately pass input to it)
    , _boxHandler_creation  :: BoxCreationType
    , _boxHandler_active    :: Bool

  } deriving (Show)

makeDragOperation :: Bool -> BoxHandleType -> PotatoHandlerInput -> RelMouseDrag -> WSEvent
makeDragOperation undoFirst bht PotatoHandlerInput {..} rmd = op where
  selection = _potatoHandlerInput_selection
  RelMouseDrag MouseDrag {..} = rmd
  dragDelta = _mouseDrag_to - _mouseDrag_from
  shiftClick = elem KeyModifier_Shift _mouseDrag_modifiers

  -- TODO may change handle when dragging through axis
  --(m, mi) = continueManipulate _mouseDrag_to lastmi smt mms
  m = bht

  boxRestrictedDelta = if shiftClick
    then restrict8 dragDelta
    else dragDelta

  dbox = makeDeltaBox m boxRestrictedDelta

  -- TODO STILL BROKEN ;__; only works if yo uscale from BR oops
  makeControllerNoNeg (_,_,SEltLabel _ selt) dbox = cmd where
    DeltaLBox tr (V2 dw dh) = dbox
    mlbox = getSEltBox selt
    -- ensures delta does not create negative box (does allow for 0 size boxes though)
    clampDelta orig delta = - (min orig (-delta))
    -- this version enforces 1 size boxes (orig box must be not be a 0 size box though, can we enforce this invariant?)
    --clampDelta orig delta = - (min (orig-1) (-delta))
    -- do not allow negative boxes for now
    modifieddbox = case mlbox of
      Nothing -> dbox -- TODO we should really just remove from list of modified elts...
      Just (LBox _ (V2 w h)) -> DeltaLBox tr (V2 (clampDelta w dw) (clampDelta h dh))

    cmd = CTagBoundingBox :=> (Identity $ CBoundingBox {
      _cBoundingBox_deltaBox = modifieddbox
    })

  makeController (_,_,SEltLabel _ selt) dbox = cmd where
    cmd = CTagBoundingBox :=> (Identity $ CBoundingBox {
      _cBoundingBox_deltaBox = dbox
    })

  op = WSEManipulate (undoFirst, IM.fromList (fmap (\s -> (fst3 s, makeController s dbox)) (toList selection)))

-- TODO split this handler in two handlers
-- one for resizing selection (including boxes)
-- and one exclusively for creating boxes
instance Default BoxHandler where
  def = BoxHandler {
      _boxHandler_handle       = BH_BR -- does this matter?
      , _boxHandler_undoFirst  = False
      , _boxHandler_creation = BoxCreationType_None
      , _boxHandler_active = False
      -- TODO whatever
      --, _boxHandler_wasDragged = False
    }

instance PotatoHandler BoxHandler where
  pHandlerName _ = handlerName_box
  pHandleMouse bh@BoxHandler {..} phi@PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) = case _mouseDrag_state of
    MouseDragState_Down | boxCreationType_isCreation _boxHandler_creation ->  Just $ def {
        _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler bh { _boxHandler_active = True }
      }
    -- if shift is held down, ignore inputs, this allows us to shift + click to deselect
    -- TODO consider moving this into GoatWidget since it's needed by many manipulators
    MouseDragState_Down | elem KeyModifier_Shift _mouseDrag_modifiers -> Nothing
    MouseDragState_Down -> r where
      mmi = findFirstMouseManipulator rmd _potatoHandlerInput_selection
      r = case mmi of
        -- didn't click on a manipulator, don't capture input
        Nothing -> Nothing
        Just mi -> Just def { _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler newbh } where
          newbh = bh {
              _boxHandler_handle = toEnum mi
              , _boxHandler_active = True
            }

    MouseDragState_Dragging -> Just r where
      dragDelta = _mouseDrag_to - _mouseDrag_from
      newEltPos = lastPositionInSelection _potatoHandlerInput_selection

      -- TODO do I use this for box creation? Prob want to restrictDiag or something though
      --shiftClick = elem KeyModifier_Shift _mouseDrag_modifiers
      --boxRestrictedDelta = if shiftClick then restrict8 dragDelta else dragDelta

      boxToAdd = def {
          _sBox_box     = canonicalLBox_from_lBox_ $ LBox _mouseDrag_from dragDelta
          , _sBox_boxType  = if _boxHandler_creation == BoxCreationType_Text
            then SBoxType_BoxText -- TODO pull from params
            else SBoxType_Box
          , _sBox_style = def { _superStyle_fill = FillStyle_Simple ' '}
          --, _sBox_title :: Maybe SBoxTitle -- TODO pull from params
        }

      nameToAdd = if _boxHandler_creation == BoxCreationType_Text then "<text>" else "<box>"

      op = if boxCreationType_isCreation _boxHandler_creation
        then WSEAddElt (_boxHandler_undoFirst, (newEltPos, SEltLabel nameToAdd $ SEltBox $ boxToAdd))
        else makeDragOperation _boxHandler_undoFirst _boxHandler_handle phi rmd

      newbh = bh { _boxHandler_undoFirst = True }

      -- NOTE, that if we did create a new box, it wil get auto selected and a new BoxHandler will be created for it

      r = def {
          _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler newbh
          , _potatoHandlerOutput_pFEvent = Just op
        }

    MouseDragState_Up -> r where
      isText = case selectionToMaybeFirstSuperSEltLabel _potatoHandlerInput_selection of
        Just (_,_,SEltLabel _ (SEltBox SBox{..})) -> sBoxType_isText _sBox_boxType
        _                                    -> False



      -- TODO right now if you click realease on a text box it will go straight into BoxText handler
      -- TODO only do this if it was already selected


      r = if isText
          -- HACK if _boxHandler_undoFirst is false, that means we didn't actually do anything since the last time we clicked which means we want to enter BoxText mode
          && (not _boxHandler_undoFirst || _boxHandler_creation == BoxCreationType_Text)
          -- if we are drag selecting, then don't enter BoxText mode
          && not (_boxHandler_creation == BoxCreationType_DragSelect)
        -- create box handler and pass on the input
        then pHandleMouse (makeBoxTextHandler (SomePotatoHandler (def :: BoxHandler)) _potatoHandlerInput_selection rmd) phi rmd
        else Just def

      -- TODO consider handling special case, handle when you click and release create a box in one spot, create a box that has size 1 (rather than 0 if we did it during MouseDragState_Down normal way)

    MouseDragState_Cancelled -> Just $ def { _potatoHandlerOutput_pFEvent = Just WSEUndo }


  pHandleKeyboard sh PotatoHandlerInput {..} kbd = case kbd of
    -- TODO keyboard movement
    _ -> Nothing

  pRenderHandler BoxHandler {..} PotatoHandlerInput {..} = r where
    handlePoints = fmap _mouseManipulator_box . filter (\mm -> _mouseManipulator_type mm == MouseManipulatorType_Corner) $ toMouseManipulators _potatoHandlerInput_selection
    -- TODO highlight active manipulator if active
    --if (_boxHandler_active)
    r = HandlerRenderOutput handlePoints
  pIsHandlerActive = _boxHandler_active
