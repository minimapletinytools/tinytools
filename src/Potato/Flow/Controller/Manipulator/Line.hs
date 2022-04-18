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
import           Potato.Flow.OwlItem
import Potato.Flow.OwlWorkspace
import Potato.Flow.BroadPhase
import           Potato.Flow.OwlItem
import Potato.Flow.OwlState
import           Potato.Flow.OwlItem
import Potato.Flow.Owl
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
  fmapfn sowl = fmap (\(a,p) -> (Attachment (_superOwl_id sowl) a, p)) $ owlItem_availableAttachments offsetBorder (_superOwl_elt sowl)
  r = join $ fmap fmapfn sowls

-- TODO move me elsewhere
getAttachmentPosition :: Bool -> OwlPFState -> Attachment -> XY
getAttachmentPosition offsetBorder pfs a = r where
  target = hasOwlTree_mustFindSuperOwl pfs (_attachment_target a)
  r = case hasOwlItem_owlItem target of
    OwlItem _ (OwlSubItemBox sbox) -> attachLocationFromLBox offsetBorder (_sBox_box sbox) (_attachment_location a)
    _ -> error "expecteed OwlSubItemBox"

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

renderEndPoints :: (Bool,Bool) -> Bool -> PotatoHandlerInput -> [RenderHandle]
renderEndPoints (highlightstart, highlightend) offsetAttach PotatoHandlerInput {..} = r where
  mselt = selectionToMaybeSuperOwl _potatoHandlerInput_canvasSelection >>= return . superOwl_toSElt_hack
  boxes = case mselt of
    -- TODO highlight
    Just (SEltLine SAutoLine {..}) -> [make_1area_lBox_from_XY startHandle, make_1area_lBox_from_XY endHandle]
      where
        startHandle = fromMaybe _sAutoLine_start (maybeLookupAttachment _sAutoLine_attachStart offsetAttach _potatoHandlerInput_pFState)
        endHandle = fromMaybe _sAutoLine_end (maybeLookupAttachment _sAutoLine_attachEnd offsetAttach _potatoHandlerInput_pFState)
    _ -> []
  r = fmap defaultRenderHandle boxes

data AutoLineHandler = AutoLineHandler {
    _autoLineHandler_isCreation :: Bool
    , _autoLineHandler_mDownPos :: Maybe XY
    -- TODO who sets this?
    , _autoLineHandler_offsetAttach :: Bool
  } deriving (Show)

instance Default AutoLineHandler where
  def = AutoLineHandler {
      _autoLineHandler_isCreation = False
      , _autoLineHandler_mDownPos = Nothing
      , _autoLineHandler_offsetAttach = False
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
  pHandleMouse slh@AutoLineHandler {..} phi@PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) = let
    mridssline = maybeGetSLine _potatoHandlerInput_canvasSelection
    attachments = getAvailableAttachments True _potatoHandlerInput_pFState _potatoHandlerInput_broadPhase _potatoHandlerInput_screenRegion
    mattachend = fmap fst . isOverAttachment _mouseDrag_to $ attachments

    in case _mouseDrag_state of

      MouseDragState_Down | _autoLineHandler_isCreation -> Just $ def {
          _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler AutoLineEndPointHandler {
              _autoLineEndPointHandler_isStart      = False
              , _autoLineEndPointHandler_undoFirst  = False
              , _autoLineEndPointHandler_isCreation = True
              , _autoLineEndPointHandler_offsetAttach = _autoLineHandler_offsetAttach
              , _autoLineEndPointHandler_attachStart = mattachend
              , _autoLineEndPointHandler_attachEnd = Nothing
            }
        }
      -- if shift is held down, ignore inputs, this allows us to shift + click to deselect
      -- TODO consider moving this into GoatWidget since it's needed by many manipulators
      MouseDragState_Down | elem KeyModifier_Shift _mouseDrag_modifiers -> Nothing
      MouseDragState_Down -> r where
        -- TODO replace this with a function that also finds midpoints
        mistart = findFirstLineManipulator _autoLineHandler_offsetAttach _potatoHandlerInput_pFState rmd _potatoHandlerInput_canvasSelection
        -- TODO
        clickonline = False
        r = case mistart of

          -- if clicked on line but not on a handler, track the position
          Nothing | clickonline -> Just $ def {
              _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler slh {
                  _autoLineHandler_mDownPos = Just _mouseDrag_to
                }
            }

          -- did not click on manipulator, no capture
          Nothing -> Nothing

          -- TODO click on midpoint, make midpoint handler

          Just isstart -> Just $ def {
              _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler AutoLineEndPointHandler {
                  _autoLineEndPointHandler_isStart      = isstart
                  , _autoLineEndPointHandler_undoFirst  = False
                  , _autoLineEndPointHandler_isCreation = False
                  , _autoLineEndPointHandler_offsetAttach = _autoLineHandler_offsetAttach
                  -- TODO I'm pretty sure you need to set these in order for things to be rendered correctly
                  , _autoLineEndPointHandler_attachStart = Nothing
                  , _autoLineEndPointHandler_attachEnd = Nothing
                }
            }
      MouseDragState_Dragging -> case _autoLineHandler_mDownPos of
        Nothing -> error "unexpected state"
        Just p -> r where
          -- TODO setup properly
          handler = AutoLineMidPointHandler {
              _autoLineMidPointHandler_dummy = ()
            }
          r = pHandleMouse handler phi rmd
      MouseDragState_Up -> case _autoLineHandler_mDownPos of
        Nothing -> Just def
        Just p -> r where
          -- TODO setup properly
          handler = AutoLineTextLabelHandler {
              _autoLineTextLabelHandler_dummy = ()
            }
          r = pHandleMouse handler phi rmd
      MouseDragState_Cancelled -> Just def
  pHandleKeyboard _ PotatoHandlerInput {..} kbd = case kbd of
    -- TODO keyboard movement
    _                              -> Nothing
  pRenderHandler AutoLineHandler {..} phi@PotatoHandlerInput {..} = r where
    boxes = renderEndPoints (False, False) _autoLineHandler_offsetAttach phi
    -- TODO set attach endpoints from currently selected line
    attachmentBoxes = renderAttachments phi (Nothing, Nothing)
    r = HandlerRenderOutput (attachmentBoxes <> boxes)

  pIsHandlerActive _ = False


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
  pHandleMouse slh@AutoLineEndPointHandler {..} PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) = let
      mridssline = maybeGetSLine _potatoHandlerInput_canvasSelection
      attachments = getAvailableAttachments True _potatoHandlerInput_pFState _potatoHandlerInput_broadPhase _potatoHandlerInput_screenRegion
      mattachend = fmap fst . isOverAttachment _mouseDrag_to $ attachments
    in case _mouseDrag_state of
      MouseDragState_Down -> error "this should be handleed by AutoLineHandler"
      MouseDragState_Dragging -> Just r where
        rid = _superOwl_id $ selectionToSuperOwl _potatoHandlerInput_canvasSelection

        ssline = case mridssline of
          Just (_,x) -> x
          Nothing -> def

        sslinestart = _sAutoLine_attachStart ssline
        sslineend = _sAutoLine_attachEnd ssline

        -- only attach on non trivial changes so we don't attach to our starting point
        nontrivialline = if _autoLineEndPointHandler_isStart
          then Just _mouseDrag_to /= (getAttachmentPosition _autoLineEndPointHandler_offsetAttach _potatoHandlerInput_pFState <$> sslineend)
          else Just _mouseDrag_from /= (getAttachmentPosition _autoLineEndPointHandler_offsetAttach _potatoHandlerInput_pFState <$> sslinestart)
        mattachendnontrivial = if nontrivialline
          then mattachend
          else Nothing

        -- for modifying an existing elt
        modifiedline = if _autoLineEndPointHandler_isStart
          then ssline {
              _sAutoLine_start       = _mouseDrag_to
              , _sAutoLine_attachStart = mattachendnontrivial
            }
          else ssline {
              _sAutoLine_end       = _mouseDrag_to
              , _sAutoLine_attachEnd = mattachendnontrivial
            }
        llama = makeSetLlama $ traceShowId $ (rid, SEltLine modifiedline)

        -- for creating new elt
        newEltPos = lastPositionInSelection (_owlPFState_owlTree _potatoHandlerInput_pFState) _potatoHandlerInput_selection
        lineToAdd = def {
            _sAutoLine_start = _mouseDrag_from
            , _sAutoLine_end = _mouseDrag_to
            , _sAutoLine_superStyle = _potatoDefaultParameters_superStyle _potatoHandlerInput_potatoDefaultParameters
            , _sAutoLine_lineStyle = _potatoDefaultParameters_lineStyle _potatoHandlerInput_potatoDefaultParameters
            , _sAutoLine_attachStart = _autoLineEndPointHandler_attachStart
            , _sAutoLine_attachEnd = mattachendnontrivial
          }

        op = if _autoLineEndPointHandler_isCreation
          then WSEAddElt (_autoLineEndPointHandler_undoFirst, newEltPos, OwlItem (OwlInfo "<line>") $ OwlSubItemLine lineToAdd Nothing)
          else WSEApplyLlama (_autoLineEndPointHandler_undoFirst, llama)

        r = def {
            _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler slh {
                _autoLineEndPointHandler_undoFirst = True
                , _autoLineEndPointHandler_attachStart = if _autoLineEndPointHandler_isStart then mattachendnontrivial else _autoLineEndPointHandler_attachStart
                , _autoLineEndPointHandler_attachEnd = if not _autoLineEndPointHandler_isStart then mattachendnontrivial else _autoLineEndPointHandler_attachEnd
              }
            , _potatoHandlerOutput_pFEvent = Just op
          }
      -- no need to return AutoLineHandler, it will be recreated from selection by goat
      MouseDragState_Up -> Just def
      MouseDragState_Cancelled -> Just def

  pRenderHandler AutoLineEndPointHandler {..} phi@PotatoHandlerInput {..} = r where
    boxes = renderEndPoints (_autoLineEndPointHandler_isStart, not _autoLineEndPointHandler_isStart) _autoLineEndPointHandler_offsetAttach phi
    attachmentBoxes = renderAttachments phi (_autoLineEndPointHandler_attachStart, _autoLineEndPointHandler_attachEnd)
    r = HandlerRenderOutput (attachmentBoxes <> boxes)
  pIsHandlerActive _ = True


--- WORK IN PROGRESS BELOW HERE

-- handles dragging and creating new midpoints
data AutoLineMidPointHandler = AutoLineMidPointHandler{
  _autoLineMidPointHandler_dummy :: ()
}

instance PotatoHandler AutoLineMidPointHandler where
  pHandlerName _ = handlerName_simpleLine_midPoint
  pHandleMouse slh@AutoLineMidPointHandler {..} PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) = Just def
  pRenderHandler AutoLineMidPointHandler {..} phi@PotatoHandlerInput {..} = undefined
  pIsHandlerActive _ = undefined

-- handles creating and modifying text labels
data AutoLineTextLabelHandler = AutoLineTextLabelHandler {
  _autoLineTextLabelHandler_dummy :: ()
}

instance PotatoHandler AutoLineTextLabelHandler where
  pHandlerName _ = handlerName_simpleLine_textLabel
  pHandleMouse slh@AutoLineTextLabelHandler {..} PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) = Just def
  pRenderHandler AutoLineTextLabelHandler {..} phi@PotatoHandlerInput {..} = undefined
  pIsHandlerActive _ = undefined
