{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Manipulator.Line (
  AutoLineHandler(..)
) where

import           Relude

import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.Manipulator.Common
import           Potato.Flow.Controller.Types
import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.OwlWorkspace
import Potato.Flow.BroadPhase
import           Potato.Flow.OwlState
import           Potato.Flow.Owl
import           Potato.Flow.Attachments
import Potato.Flow.Llama

import           Control.Exception
import           Data.Default
import qualified Data.Sequence                             as Seq

maybeGetSLine :: CanvasSelection -> Maybe (REltId, SAutoLine)
maybeGetSLine selection = if Seq.length (unCanvasSelection selection) /= 1
  then Nothing
  else case superOwl_toSElt_hack sowl of
    SEltLine sline  -> Just (rid, sline)
    selt -> Nothing
    where
      sowl = selectionToSuperOwl selection
      rid = _superOwl_id sowl


-- TODO TEST
-- TODO move me elsewhere
getAvailableAttachments :: Bool -> OwlPFState -> BroadPhaseState -> LBox -> [(Attachment, XY)]
getAvailableAttachments offsetBorder pfs bps screenRegion = r where
  culled = broadPhase_cull screenRegion (_broadPhaseState_bPTree bps)
  -- you could silently fail here by ignoring maybes but that would definitely be an indication of a bug so we fail here instead (you could do a better job about dumping debug info though)
  sowls = fmap (hasOwlTree_mustFindSuperOwl pfs) culled
  -- TODO sort sowls
  fmapfn sowl = fmap (\(a,p) -> (Attachment (_superOwl_id sowl) a, p)) $ owlElt_availableAttachments offsetBorder (_superOwl_elt sowl)
  r = join $ fmap fmapfn sowls

-- TODO move me elsewhere
getAttachmentPosition :: Bool -> OwlPFState -> Attachment -> XY
getAttachmentPosition offsetBorder pfs a = r where
  target = hasOwlTree_mustFindSuperOwl pfs (_attachment_target a)
  r = case hasOwlElt_owlElt target of
    OwlEltSElt _ selt -> case selt of
      SEltBox sbox -> attachLocationFromLBox offsetBorder (_sBox_box sbox) (_attachment_location a)
      _ -> error "expected SEltBox"
    _ -> error "expecteed OwlEltSelt"

maybeLookupAttachment :: Maybe Attachment -> Bool -> OwlPFState -> Maybe XY
maybeLookupAttachment matt offsetBorder pfs = getAttachmentPosition offsetBorder pfs <$> matt

renderAttachments :: PotatoHandlerInput -> (Maybe Attachment, Maybe Attachment) -> [RenderHandle]
renderAttachments PotatoHandlerInput {..} (mstart, mend) = r where
  attachments = getAvailableAttachments True _potatoHandlerInput_pFState _potatoHandlerInput_broadPhase _potatoHandlerInput_screenRegion
  fmapattachmentfn (a,p) = RenderHandle {
      _renderHandle_box = (LBox p 1)
      , _renderHandle_char = Just (attachmentRenderChar a)
      , _renderHandle_color = if matches mstart || matches mend
        then RHC_AttachmentHighlight
        else RHC_Attachment
    } where
      rid = _attachment_target a
      matches ma = fmap (\a' -> _attachment_target a' == rid) ma == Just True
  r = fmap fmapattachmentfn attachments

data AutoLineHandler = AutoLineHandler {
    _autoLineHandler_isStart      :: Bool -- either we are manipulating start, or we are manipulating end

    , _autoLineHandler_undoFirst  :: Bool
    , _autoLineHandler_isCreation :: Bool
    , _autoLineHandler_active     :: Bool

    , _autoLineHandler_offsetAttach :: Bool -- who sets this?

    -- where the current modified line is attached to (_autoLineHandler_attachStart will differ from actual line in the case when we start creating a line on mouse down)
    , _autoLineHandler_attachStart :: Maybe Attachment
    , _autoLineHandler_attachEnd :: Maybe Attachment
  } deriving (Show)

instance Default AutoLineHandler where
  def = AutoLineHandler {
      _autoLineHandler_isStart = False
      , _autoLineHandler_undoFirst = False
      , _autoLineHandler_isCreation = False
      , _autoLineHandler_active = False
      , _autoLineHandler_offsetAttach = True
      , _autoLineHandler_attachStart = Nothing
      , _autoLineHandler_attachEnd = Nothing
    }


findFirstLineManipulator :: Bool -> OwlPFState -> RelMouseDrag -> CanvasSelection -> Maybe Bool
findFirstLineManipulator offsetBorder pfs (RelMouseDrag MouseDrag {..}) (CanvasSelection selection) = assert (Seq.length selection == 1) $ r where
  msowl = Seq.lookup 0 selection
  selt = case msowl of
    Nothing -> error "expected selection"
    Just sowl -> superOwl_toSElt_hack sowl
  r = case selt of
    SEltLine SAutoLine {..} ->
      let
        start = fromMaybe _sAutoLine_start $ maybeLookupAttachment _sAutoLine_attachStart offsetBorder pfs
        end = fromMaybe _sAutoLine_end $ maybeLookupAttachment _sAutoLine_attachEnd offsetBorder pfs
      in
        if _mouseDrag_to == start then Just True
          else if _mouseDrag_to == end then Just False
            else Nothing
    x -> error $ "expected SAutoLine in selection but got " <> show x <> " instead"


instance PotatoHandler AutoLineHandler where
  pHandlerName _ = handlerName_simpleLine
  pHandleMouse slh@AutoLineHandler {..} PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) = let
    mridssline = maybeGetSLine _potatoHandlerInput_canvasSelection
    attachments = getAvailableAttachments True _potatoHandlerInput_pFState _potatoHandlerInput_broadPhase _potatoHandlerInput_screenRegion
    mattachend = fmap fst . isOverAttachment _mouseDrag_to $ attachments

    in case _mouseDrag_state of

      MouseDragState_Down | _autoLineHandler_isCreation -> Just $ def {
          _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler slh {
              _autoLineHandler_active = True
              , _autoLineHandler_isStart = False
              , _autoLineHandler_attachStart = mattachend
            }
        }
      -- if shift is held down, ignore inputs, this allows us to shift + click to deselect
      -- TODO consider moving this into GoatWidget since it's needed by many manipulators
      MouseDragState_Down | elem KeyModifier_Shift _mouseDrag_modifiers -> Nothing
      MouseDragState_Down -> r where
        mistart = findFirstLineManipulator _autoLineHandler_offsetAttach _potatoHandlerInput_pFState rmd _potatoHandlerInput_canvasSelection
        r = case mistart of
          Nothing -> Nothing -- did not click on manipulator, no capture
            -- OR create a new handler?
            -- TODO mark no manipulator dragging position here
          Just isstart -> Just $ def {
              _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler slh {
                  _autoLineHandler_isStart = isstart
                  , _autoLineHandler_active = True
                }
            }
      MouseDragState_Dragging -> Just r where
        rid = _superOwl_id $ selectionToSuperOwl _potatoHandlerInput_canvasSelection

        ssline = case mridssline of
          Just (_,x) -> x
          Nothing -> def

        sslinestart = _sAutoLine_attachStart ssline
        sslineend = _sAutoLine_attachEnd ssline

        -- only attach on non trivial changes so we don't attach to our starting point
        nontrivialline = if _autoLineHandler_isStart
          then Just _mouseDrag_to /= (getAttachmentPosition _autoLineHandler_offsetAttach _potatoHandlerInput_pFState <$> sslineend)
          else Just _mouseDrag_from /= (getAttachmentPosition _autoLineHandler_offsetAttach _potatoHandlerInput_pFState <$> sslinestart)
        mattachendnontrivial = if nontrivialline
          then mattachend
          else Nothing

        -- for modifying an existing elt
        modifiedline = if _autoLineHandler_isStart
          then ssline {
              _sAutoLine_start       = _mouseDrag_to
              , _sAutoLine_attachStart = mattachendnontrivial
            }
          else ssline {
              _sAutoLine_end       = _mouseDrag_to
              , _sAutoLine_attachEnd = mattachendnontrivial
            }
        llama = makeSetLlama (rid, SEltLine modifiedline)

        -- for creating new elt
        newEltPos = lastPositionInSelection (_owlPFState_owlTree _potatoHandlerInput_pFState) _potatoHandlerInput_selection
        lineToAdd = SEltLine $ def {
            _sAutoLine_start = _mouseDrag_from
            , _sAutoLine_end = _mouseDrag_to
            , _sAutoLine_superStyle = _potatoDefaultParameters_superStyle _potatoHandlerInput_potatoDefaultParameters
            , _sAutoLine_lineStyle = _potatoDefaultParameters_lineStyle _potatoHandlerInput_potatoDefaultParameters
            , _sAutoLine_attachStart = _autoLineHandler_attachStart
            , _sAutoLine_attachEnd = mattachendnontrivial
          }

        op = if _autoLineHandler_isCreation
          then WSEAddElt (_autoLineHandler_undoFirst, newEltPos, OwlEltSElt (OwlInfo "<line>") $ lineToAdd)
          else WSEApplyLlama (_autoLineHandler_undoFirst, llama)

        r = def {
            _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler slh {
                _autoLineHandler_undoFirst = True
                , _autoLineHandler_attachStart = if _autoLineHandler_isStart then mattachendnontrivial else _autoLineHandler_attachStart
                , _autoLineHandler_attachEnd = if not _autoLineHandler_isStart then mattachendnontrivial else _autoLineHandler_attachEnd
              }
            , _potatoHandlerOutput_pFEvent = Just op
          }
      MouseDragState_Up -> Just def
      MouseDragState_Cancelled -> Just def
  pHandleKeyboard _ PotatoHandlerInput {..} kbd = case kbd of
    -- TODO keyboard movement
    _                              -> Nothing
  pRenderHandler AutoLineHandler {..} phi@PotatoHandlerInput {..} = r where
    mselt = selectionToMaybeSuperOwl _potatoHandlerInput_canvasSelection >>= return . superOwl_toSElt_hack

    boxes = case mselt of
      Just (SEltLine SAutoLine {..}) -> if _autoLineHandler_active
        -- TODO if active, color selected handler
        then [make_1area_lBox_from_XY startHandle, make_1area_lBox_from_XY endHandle]
        else [make_1area_lBox_from_XY startHandle, make_1area_lBox_from_XY endHandle]
        where
          startHandle = fromMaybe _sAutoLine_start (maybeLookupAttachment _sAutoLine_attachStart _autoLineHandler_offsetAttach _potatoHandlerInput_pFState)
          endHandle = fromMaybe _sAutoLine_end (maybeLookupAttachment _sAutoLine_attachEnd _autoLineHandler_offsetAttach _potatoHandlerInput_pFState)
      _ -> []

    attachmentBoxes = renderAttachments phi (_autoLineHandler_attachStart, _autoLineHandler_attachEnd)

    r = HandlerRenderOutput (attachmentBoxes <> fmap defaultRenderHandle boxes)

  pIsHandlerActive = _autoLineHandler_active



--- WORK IN PROGRESS BELOW HERE

-- handles dragging endpoints (which can be attached) and creating new lines
data AutoLineEndPointHandler = AutoLineEndPointHandler {
  _autoLineEndPointHandler_isStart      :: Bool -- either we are manipulating start, or we are manipulating end

  , _autoLineEndPointHandler_undoFirst  :: Bool
  , _autoLineEndPointHandler_isCreation :: Bool

  , _autoLineEndPointHandler_offsetAttach :: Bool -- who sets this?

  -- where the current modified line is attached to (_autoLineEndPointHandler_attachStart will differ from actual line in the case when we start creating a line on mouse down)
  , _autoLineEndPointHandler_attachStart :: Maybe Attachment
  , _autoLineEndPointHandler_attachEnd :: Maybe Attachment
}

instance PotatoHandler AutoLineEndPointHandler where
  pHandlerName _ = handlerName_simpleLine_endPoint
  pHandleMouse slh@AutoLineEndPointHandler {..} PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) = undefined
  pRenderHandler AutoLineEndPointHandler {..} phi@PotatoHandlerInput {..} = undefined
  pIsHandlerActive _ = True

-- handles dragging and creating new midpoints
data AutoLineMidPointHandler = AutoLineMidPointHandler{
  _autoLineMidPointHandler_dummy :: ()
}

instance PotatoHandler AutoLineMidPointHandler where
  pHandlerName _ = handlerName_simpleLine_midPoint
  pHandleMouse slh@AutoLineMidPointHandler {..} PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) = undefined
  pRenderHandler AutoLineMidPointHandler {..} phi@PotatoHandlerInput {..} = undefined
  pIsHandlerActive _ = undefined

-- handles creating and modifying text labels
data AutoLineTextLabelHandler = AutoLineTextLabelHandler {
  _autoLineTextLabelHandler_dummy :: ()
}

instance PotatoHandler AutoLineTextLabelHandler where
  pHandlerName _ = handlerName_simpleLine_textLabel
  pHandleMouse slh@AutoLineTextLabelHandler {..} PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) = undefined
  pRenderHandler AutoLineTextLabelHandler {..} phi@PotatoHandlerInput {..} = undefined
  pIsHandlerActive _ = undefined
