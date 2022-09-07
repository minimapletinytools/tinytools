{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Manipulator.Box (
  BoxHandleType(..)
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
import           Potato.Flow.Controller.Manipulator.TextArea
import           Potato.Flow.Controller.Manipulator.Common
import           Potato.Flow.Controller.Types
import           Potato.Flow.Math
import           Potato.Flow.SEltMethods
import           Potato.Flow.SElts
import           Potato.Flow.Types
import           Potato.Flow.OwlItem
import Potato.Flow.Owl
import           Potato.Flow.OwlItem
import Potato.Flow.OwlState
import           Potato.Flow.OwlItem
import Potato.Flow.OwlWorkspace
import Potato.Flow.Methods.Types
import Potato.Flow.Llama

import           Data.Default
import           Data.Dependent.Sum                         (DSum ((:=>)))
import qualified Data.IntMap                                as IM
import qualified Data.Map as Map
import qualified Data.List                                  as L
import qualified Data.Sequence as Seq

import Control.Exception (assert)



superOwl_isTransformable :: (HasOwlTree o) => SuperOwl -> o -> Bool
superOwl_isTransformable sowl ot = case _owlItem_subItem (_superOwl_elt sowl) of
  OwlSubItemNone -> False
  OwlSubItemFolder _ -> False
  OwlSubItemLine sline _ -> not $
    (fromMaybe False $ _sAutoLine_attachStart sline <&> (\att -> hasOwlTree_exists ot (_attachment_target att)))
    && (fromMaybe False $ _sAutoLine_attachEnd sline <&> (\att -> hasOwlTree_exists ot (_attachment_target att)))
  _ -> True

transformableSelection :: PotatoHandlerInput -> Seq SuperOwl
transformableSelection PotatoHandlerInput {..} = transformableSelection' _potatoHandlerInput_pFState _potatoHandlerInput_canvasSelection

transformableSelection' :: OwlPFState -> CanvasSelection -> Seq SuperOwl
transformableSelection' pfs sel = Seq.filter (flip superOwl_isTransformable pfs) (unCanvasSelection sel)


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

toMouseManipulators :: OwlPFState -> CanvasSelection -> MouseManipulatorSet
toMouseManipulators pfs selection' = bb where
  union_lBoxes :: NonEmpty LBox -> LBox
  union_lBoxes (x:|xs) = foldl' union_lBox x xs
  selection = transformableSelection' pfs selection'
  fmapfn sowl = _sEltDrawer_box (getDrawer . hasOwlItem_toOwlSubItem $ sowl) pfs
  -- consider filtering out boxes with 0 area, but really _sEltDrawer_box should have return type Maybe LBox
  sboxes = toList $ fmap fmapfn selection
  bb = case sboxes of
    []   -> []
    x:xs -> fmap (flip makeHandleBox (union_lBoxes (x:|xs))) [BH_TL .. BH_A]

findFirstMouseManipulator :: OwlPFState -> RelMouseDrag -> CanvasSelection -> Maybe ManipulatorIndex
findFirstMouseManipulator pfs (RelMouseDrag MouseDrag {..}) selection = r where
  mms = toMouseManipulators pfs selection
  smt = computeSelectionType selection

  -- TODO use select magic here
  normalSel = L.findIndex (\mm -> does_lBox_contains_XY (_mouseManipulator_box mm) _mouseDrag_from) mms
  r = case smt of
    SMTTextArea -> normalSel -- TODO figure out how to differentiate between area / text manipulator
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
data BoxCreationType = BoxCreationType_None | BoxCreationType_Box | BoxCreationType_Text | BoxCreationType_TextArea | BoxCreationType_DragSelect deriving (Show, Eq)

boxCreationType_isCreation :: BoxCreationType -> Bool
boxCreationType_isCreation bct = bct /= BoxCreationType_None && bct /= BoxCreationType_DragSelect


-- new handler stuff
data BoxHandler = BoxHandler {

    _boxHandler_handle      :: BoxHandleType -- the current handle we are dragging
    , _boxHandler_undoFirst :: Bool

    -- with this you can use same code for both create and manipulate (create the handler and immediately pass input to it)
    , _boxHandler_creation  :: BoxCreationType
    , _boxHandler_active    :: Bool

    , _boxHandler_downOnLabel :: Bool

  } deriving (Show)

makeDragDeltaBox :: BoxHandleType -> RelMouseDrag -> DeltaLBox
makeDragDeltaBox bht rmd = r where
  RelMouseDrag MouseDrag {..} = rmd
  dragDelta = _mouseDrag_to - _mouseDrag_from
  shiftClick = elem KeyModifier_Shift _mouseDrag_modifiers

  boxRestrictedDelta = if shiftClick
    then restrict8 dragDelta
    else dragDelta

  r = makeDeltaBox bht boxRestrictedDelta

makeDragOperation :: Bool -> PotatoHandlerInput -> DeltaLBox -> Maybe WSEvent
makeDragOperation undoFirst phi dbox = op where
  selection = transformableSelection phi

  makeController _ = cmd where
    cmd = CTagBoundingBox :=> (Identity $ CBoundingBox {
      _cBoundingBox_deltaBox = dbox
    })

  op = if Seq.null selection
    then Nothing
    else Just $ WSEApplyLlama (undoFirst, makePFCLlama . OwlPFCManipulate $ IM.fromList (fmap (\s -> (_superOwl_id s, makeController s)) (toList selection)))

-- TODO split this handler in two handlers
-- one for resizing selection (including boxes)
-- and one exclusively for creating boxes
instance Default BoxHandler where
  def = BoxHandler {
      _boxHandler_handle       = BH_BR
      , _boxHandler_undoFirst  = False
      , _boxHandler_creation = BoxCreationType_None
      , _boxHandler_active = False
      , _boxHandler_downOnLabel = False
      -- TODO whatever
      --, _boxHandler_wasDragged = False
    }



selectionOnlySBox :: CanvasSelection -> Maybe SBox
selectionOnlySBox (CanvasSelection selection) = if Seq.length selection == 1
  then case superOwl_toSElt_hack (Seq.index selection 0) of
    SEltBox sbox -> Just sbox
    _ -> Nothing
  else Nothing


isMouseOnSelectionSBoxBorder :: CanvasSelection -> RelMouseDrag -> Bool
isMouseOnSelectionSBoxBorder cs (RelMouseDrag MouseDrag {..}) = case selectionOnlySBox cs of
  -- not an SBox selected
  Nothing -> False
  Just sbox -> if sBoxType_hasBorder (_sBox_boxType sbox) && does_lBox_contains_XY (lBox_to_boxLabelBox (_sBox_box sbox)) _mouseDrag_from
    then True
    else False


instance PotatoHandler BoxHandler where
  pHandlerName _ = handlerName_box
  pHandleMouse bh@BoxHandler {..} phi@PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) = case _mouseDrag_state of

    -- TODO creation should be a separate handler
    MouseDragState_Down | boxCreationType_isCreation _boxHandler_creation ->  Just $ def {
        _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler bh { _boxHandler_active = True }
      }
    -- if shift is held down, ignore inputs, this allows us to shift + click to deselect
    -- TODO consider moving this into GoatWidget since it's needed by many manipulators
    MouseDragState_Down | elem KeyModifier_Shift _mouseDrag_modifiers -> Nothing
    -- in DragSelect case we already have a selection
    MouseDragState_Down | _boxHandler_creation == BoxCreationType_DragSelect  -> assert (not . isParliament_null $ _potatoHandlerInput_selection) r where
        newbh = bh {
            -- drag select case is always BH_A
            _boxHandler_handle = BH_A
            , _boxHandler_active = True
          }
        r = Just def { _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler newbh }
    MouseDragState_Down -> case findFirstMouseManipulator _potatoHandlerInput_pFState rmd _potatoHandlerInput_canvasSelection of
      Nothing -> Nothing



      -- clicked on a manipulator, begin dragging
      Just mi -> r where
        newbh = bh {
            _boxHandler_handle = bht
            , _boxHandler_active = True
            -- label position always intersects BH_A so we do the test in here to see if we clicked on the label area
            , _boxHandler_downOnLabel = if bht == BH_A then isMouseOnSelectionSBoxBorder _potatoHandlerInput_canvasSelection rmd else False
          }
        bht = toEnum mi
        -- special case behavior for BH_A require actually clicking on something on selection
        clickOnSelection = any (doesSEltIntersectPoint _mouseDrag_to . superOwl_toSElt_hack) $ unCanvasSelection _potatoHandlerInput_canvasSelection
        r = if bht /= BH_A || clickOnSelection
          then Just def { _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler newbh }
          else Nothing


    MouseDragState_Dragging -> Just r where
      dragDelta = _mouseDrag_to - _mouseDrag_from
      newEltPos = lastPositionInSelection (_owlPFState_owlTree _potatoHandlerInput_pFState) _potatoHandlerInput_selection

      -- TODO do I use this for box creation? Prob want to restrictDiag or something though
      --shiftClick = elem KeyModifier_Shift _mouseDrag_modifiers
      --boxRestrictedDelta = if shiftClick then restrict8 dragDelta else dragDelta

      boxToAdd = def {
          _sBox_box     = canonicalLBox_from_lBox_ $ LBox _mouseDrag_from dragDelta
          -- consider using _potatoDefaultParameters_boxType instead
          , _sBox_boxType  = if _boxHandler_creation == BoxCreationType_Text
            then SBoxType_BoxText -- TODO pull from params
            else SBoxType_Box
          , _sBox_style = _potatoDefaultParameters_superStyle _potatoHandlerInput_potatoDefaultParameters
          , _sBox_title = def { _sBoxTitle_align = _potatoDefaultParameters_box_label_textAlign _potatoHandlerInput_potatoDefaultParameters }
          , _sBox_text = def { _sBoxText_style = def { _textStyle_alignment = _potatoDefaultParameters_box_text_textAlign _potatoHandlerInput_potatoDefaultParameters } }
        }

      textAreaToAdd = def {
          _sTextArea_box   =  canonicalLBox_from_lBox_ $ LBox _mouseDrag_from dragDelta
          , _sTextArea_text        = Map.empty
          , _sTextArea_transparent = True
        }

      nameToAdd = case _boxHandler_creation of
        BoxCreationType_Box -> "<box>"
        BoxCreationType_Text -> "<text>"
        BoxCreationType_TextArea -> "<textarea>"
        _ -> error "invalid BoxCreationType"

      mop = case _boxHandler_creation of
        x | x == BoxCreationType_Box || x == BoxCreationType_Text -> Just $ WSEAddElt (_boxHandler_undoFirst, newEltPos, OwlItem (OwlInfo nameToAdd) (OwlSubItemBox boxToAdd))
        BoxCreationType_TextArea -> Just $ WSEAddElt (_boxHandler_undoFirst, newEltPos, OwlItem (OwlInfo nameToAdd) (OwlSubItemTextArea textAreaToAdd))
        _ -> makeDragOperation _boxHandler_undoFirst phi (makeDragDeltaBox _boxHandler_handle rmd)

      newbh = bh {
          _boxHandler_undoFirst = True
          -- if we drag, we are no longer in label case
          , _boxHandler_downOnLabel = False
        }

      -- NOTE, that if we did create a new box, it wil get auto selected and a new BoxHandler will be created for it

      r = def {
          _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler newbh
          , _potatoHandlerOutput_pFEvent = mop
        }

    MouseDragState_Up | _boxHandler_downOnLabel -> if isMouseOnSelectionSBoxBorder _potatoHandlerInput_canvasSelection rmd
      -- clicked on the box label area
      -- pass on mouse as MouseDragState_Down is a hack but whatever it works
      -- TODO fix this hack, just have mouse up handle selection in this special case
      then pHandleMouse (makeBoxLabelHandler (SomePotatoHandler (def :: BoxHandler)) _potatoHandlerInput_canvasSelection rmd) phi rmd
      else Nothing
    MouseDragState_Up -> r where

      -- TODO do selectMagic here so we can enter text edit modes from multi-selections (you will also need to modify the selection)
      nselected = Seq.length (unCanvasSelection _potatoHandlerInput_canvasSelection)
      selt = superOwl_toSElt_hack <$> selectionToMaybeFirstSuperOwl _potatoHandlerInput_canvasSelection
      isText = nselected == 1 && case selt of
        Just (SEltBox SBox{..}) -> sBoxType_isText _sBox_boxType
        _                                    -> False
      isTextArea = nselected == 1 && case selt of
        Just (SEltTextArea _) -> True
        _ -> False


      -- only enter sub handler if we weren't drag selecting (this includes selecting it from an unselect state without dragging)
      wasNotDragSelecting = not (_boxHandler_creation == BoxCreationType_DragSelect)
      -- only enter subHandler we did not drag (hack, we do this by testing form _boxHandler_undoFirst)
      wasNotActuallyDragging = not _boxHandler_undoFirst
      -- always go straight to handler after creating a new SElt
      isCreation = boxCreationType_isCreation _boxHandler_creation
      r = if isText
          && (wasNotActuallyDragging || isCreation)
          && wasNotDragSelecting
        -- create box handler and pass on the input
        then pHandleMouse (makeBoxTextHandler (SomePotatoHandler (def :: BoxHandler)) _potatoHandlerInput_canvasSelection rmd) phi rmd
        else if isTextArea
          && (wasNotActuallyDragging || isCreation)
          && wasNotDragSelecting
          then pHandleMouse (makeTextAreaHandler (SomePotatoHandler (def :: BoxHandler)) _potatoHandlerInput_canvasSelection rmd isCreation) phi rmd
          -- This clears the handler and causes selection to regenerate a new handler.
          -- Why do we do it this way instead of returning a handler? Not sure, doesn't matter.
          else Just def

      -- TODO consider handling special case, handle when you click and release create a box in one spot, create a box that has size 1 (rather than 0 if we did it during MouseDragState_Down normal way)

    -- TODO check undo first condition
    MouseDragState_Cancelled -> if _boxHandler_undoFirst then Just def { _potatoHandlerOutput_pFEvent = Just WSEUndo } else Just def


  pHandleKeyboard bh phi@PotatoHandlerInput {..} (KeyboardData key _) = r where

    todlbox (x,y) = Just $ DeltaLBox (V2 x y) 0
    mmove = case key of
      KeyboardKey_Left -> todlbox (-1,0)
      KeyboardKey_Right -> todlbox (1,0)
      KeyboardKey_Up -> todlbox (0,-1)
      KeyboardKey_Down -> todlbox (0,1)
      _ -> Nothing

    r = if _boxHandler_active bh
      -- ignore inputs when we're in the middle of dragging
      then Nothing
      else case mmove of
        Nothing -> Nothing
        Just move -> Just r2 where
          mop = makeDragOperation False phi move
          r2 = def {
              _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler bh
              , _potatoHandlerOutput_pFEvent = mop
            }

  pRenderHandler BoxHandler {..} PotatoHandlerInput {..} = r where
    handlePoints = fmap _mouseManipulator_box . filter (\mm -> _mouseManipulator_type mm == MouseManipulatorType_Corner) $ toMouseManipulators _potatoHandlerInput_pFState _potatoHandlerInput_canvasSelection
    -- TODO highlight active manipulator if active
    --if (_boxHandler_active)
    r = HandlerRenderOutput (fmap defaultRenderHandle handlePoints)
  pIsHandlerActive = _boxHandler_active

  pHandlerTool BoxHandler {..} = case _boxHandler_creation of
    BoxCreationType_Box -> Just Tool_Box
    BoxCreationType_Text -> Just Tool_Text
    BoxCreationType_TextArea -> Just Tool_TextArea
    _ -> Nothing
