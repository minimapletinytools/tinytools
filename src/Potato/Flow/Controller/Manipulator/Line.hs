{-# OPTIONS_GHC -fno-warn-unused-record-wildcards #-}

{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Manipulator.Line (
  AutoLineHandler(..)
) where

import           Relude

import qualified Potato.Data.Text.Zipper                           as TZ
import           Potato.Flow.Attachments
import           Potato.Flow.BroadPhase
import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.Manipulator.Common
import           Potato.Flow.Controller.Manipulator.TextInputState
import           Potato.Flow.Controller.Types
import           Potato.Flow.DebugHelpers
import           Potato.Flow.Llama
import           Potato.Flow.Math
import           Potato.Flow.Methods.LineDrawer
import           Potato.Flow.Owl
import           Potato.Flow.OwlItem
import           Potato.Flow.OwlState
import           Potato.Flow.Serialization.Snake
import Potato.Flow.Methods.LlamaWorks
import Potato.Flow.Preview

import Control.Monad (msum)
import           Control.Exception
import           Data.Default
import qualified Data.List                                         as L
import qualified Data.List.Index                                   as L
import qualified Data.Sequence                                     as Seq
import qualified Data.Text                                         as T

import           Data.Maybe                                        (fromJust)


maybeGetSLine :: CanvasSelection -> Maybe (REltId, SAutoLine)
maybeGetSLine selection = if Seq.length (unCanvasSelection selection) /= 1
  then Nothing
  else case superOwl_toSElt_hack sowl of
    SEltLine sline -> Just (rid, sline)
    _              -> Nothing
    where
      sowl = selectionToSuperOwl selection
      rid = _superOwl_id sowl

mustGetSLine :: CanvasSelection -> (REltId, SAutoLine)
mustGetSLine = fromJust . maybeGetSLine

-- TODO change return type to AvailableAttachment
-- TODO move me elsewhere
getAvailableAttachments :: Bool -> Bool -> OwlPFState -> BroadPhaseState -> LBox -> [(Attachment, XY)]
getAvailableAttachments includeNoBorder offsetBorder pfs bps screenRegion = r where
  culled = broadPhase_cull screenRegion (_broadPhaseState_bPTree bps)
  -- you could silently fail here by ignoring maybes but that would definitely be an indication of a bug so we fail here instead (you could do a better job about dumping debug info though)
  sowls = fmap (hasOwlTree_mustFindSuperOwl pfs) culled
  -- TODO sort sowls
  fmapfn sowl = fmap (\(a,p) -> (attachment_create_default (_superOwl_id sowl) a, p)) $ owlItem_availableAttachmentsAtDefaultLocation includeNoBorder offsetBorder (_superOwl_elt sowl)
  r = join $ fmap fmapfn sowls

renderAttachments :: PotatoHandlerInput -> (Maybe Attachment, Maybe Attachment) -> [RenderHandle]
renderAttachments PotatoHandlerInput {..} (mstart, mend) = r where
  attachments = getAvailableAttachments False True _potatoHandlerInput_pFState _potatoHandlerInput_broadPhase _potatoHandlerInput_screenRegion
  fmapattachmentfn (a,p) = if matches mstart || matches mend then Nothing else Just $ RenderHandle {
      _renderHandle_box = (LBox p 1)
      , _renderHandle_char = Just (attachmentRenderChar a)
      , _renderHandle_color = RHC_Attachment
    } where
      rid = _attachment_target a
      al = _attachment_location a
      matches ma = fmap (\a' -> _attachment_target a' == rid && _attachment_location a' == al) ma == Just True
  r = catMaybes $ fmap fmapattachmentfn attachments

-- set midpointhighlightindex index to -1 for no highlight
maybeRenderPoints :: (Bool,Bool) -> Bool -> Int -> PotatoHandlerInput -> [RenderHandle]
maybeRenderPoints (highlightstart, highlightend) offsetAttach midpointhighlightindex PotatoHandlerInput {..} = r where
  -- in creation cases, _potatoHandlerInput_canvasSelection might not be a line
  -- however we only render points in non creation cases (I think) so this maybe should plainly not be necessary
  mselt = selectionToMaybeFirstSuperOwl _potatoHandlerInput_canvasSelection >>= return . superOwl_toSElt_hack
  r1 = case mselt of
    Just (SEltLine SAutoLine {..}) -> [makeRenderHandle (make_1area_lBox_from_XY startHandle) True, makeRenderHandle (make_1area_lBox_from_XY endHandle) False]
      where
        startHandle = fromMaybe _sAutoLine_start (maybeLookupAttachment offsetAttach _potatoHandlerInput_pFState _sAutoLine_attachStart)
        endHandle = fromMaybe _sAutoLine_end (maybeLookupAttachment offsetAttach _potatoHandlerInput_pFState _sAutoLine_attachEnd)
        makeRenderHandle b isstart = RenderHandle {
            _renderHandle_box     = b
            , _renderHandle_char  = if isstart then Just 'S' else Just 'E'
            , _renderHandle_color = if (isstart && highlightstart) || (not isstart && highlightend) then RHC_AttachmentHighlight else RHC_Default
          }
    _ -> []
  r2 = case mselt of
    Just (SEltLine SAutoLine {..}) -> L.imap imapfn _sAutoLine_midpoints
      where
        imapfn i mp = case mp of
          SAutoLineConstraintFixed pos -> RenderHandle {
              _renderHandle_box     = make_1area_lBox_from_XY pos
              , _renderHandle_char  = Just 'X'
              , _renderHandle_color = if midpointhighlightindex == i then RHC_AttachmentHighlight else RHC_Default
            }
    _ -> []
  r = r1 <> r2

renderLabels :: PotatoHandlerInput -> Bool -> [RenderHandle]
renderLabels PotatoHandlerInput {..} offsetByLabelHeight = r where
  (_, sal) = mustGetSLine _potatoHandlerInput_canvasSelection
  labels = getSortedSAutoLineLabelPositions _potatoHandlerInput_pFState sal
  fmapfn (pos,_,_) = RenderHandle {
      _renderHandle_box     = if offsetByLabelHeight
        then make_1area_lBox_from_XY (pos - (V2 0 1))
        else make_1area_lBox_from_XY pos
      , _renderHandle_char  = Just 'T'
      , _renderHandle_color = RHC_Default
    }
  r = fmap fmapfn labels


data AutoLineHandler = AutoLineHandler {
    _autoLineHandler_isCreation         :: Bool
    , _autoLineHandler_mDownManipulator :: Maybe Int
    -- TODO who sets this?
    , _autoLineHandler_offsetAttach     :: Bool
  } deriving (Show)

instance Default AutoLineHandler where
  def = AutoLineHandler {
      _autoLineHandler_isCreation = False
      , _autoLineHandler_mDownManipulator = Nothing
      , _autoLineHandler_offsetAttach = True
    }

-- TODO instead of `LMP_Midpoint Int` consider using zipper
data LineManipulatorProxy = LMP_Endpoint Bool | LMP_Midpoint Int | LMP_Nothing

sAutoLineConstraint_handlerPosition :: SAutoLineConstraint -> XY
sAutoLineConstraint_handlerPosition slc = case slc of
  SAutoLineConstraintFixed xy -> xy

findFirstLineManipulator_NEW :: SAutoLine -> Bool -> OwlPFState -> RelMouseDrag-> LineManipulatorProxy
findFirstLineManipulator_NEW SAutoLine {..} offsetBorder pfs (RelMouseDrag MouseDrag {..})= r where
  start = fromMaybe _sAutoLine_start $ maybeLookupAttachment offsetBorder pfs _sAutoLine_attachStart
  end = fromMaybe _sAutoLine_end $ maybeLookupAttachment offsetBorder pfs _sAutoLine_attachEnd
  mmid = L.findIndex (\slc -> sAutoLineConstraint_handlerPosition slc == _mouseDrag_to) _sAutoLine_midpoints
  r = if _mouseDrag_to == start then LMP_Endpoint True
    else if _mouseDrag_to == end then LMP_Endpoint False
      else maybe LMP_Nothing LMP_Midpoint mmid

-- TODO use cache
-- |
-- IMPORTANT MIDPOINT INDEXING DETAILS
-- midpoint indexing for N midpoints looks like
-- S ... 0 ... 1 ... N ... E
-- a midpoint index of (-1) is the segment between S and 0
--
-- e.g.
-- S ...(x)... 0 ... 1 ...
-- returns -1
-- favors right side
--
-- e.g.
-- S ... (x) ... 1
-- returns 0
--
-- to convert to _autoLineMidPointHandler_midPointIndex index you need to MINUS 1
whichSubSegmentDidClick :: OwlTree -> SAutoLine -> XY -> Maybe Int
whichSubSegmentDidClick ot sline@SAutoLine {..} pos = r where
  lars = sAutoLine_to_lineAnchorsForRenderList ot sline
  r = fmap fst $ L.ifind (\_ lar -> isJust $ lineAnchorsForRender_findIntersectingSubsegment lar pos) lars



getEndpointPosition ::  Bool -> OwlPFState -> SAutoLine -> Bool -> XY
getEndpointPosition offsetAttach pfs SAutoLine {..} isstart = if isstart
  then fromMaybe _sAutoLine_start $ maybeGetAttachmentPosition offsetAttach pfs =<< _sAutoLine_attachStart
  else fromMaybe _sAutoLine_end $ maybeGetAttachmentPosition offsetAttach pfs =<< _sAutoLine_attachEnd



-- |
-- see indexing information in 'whichSubSegmentDidClick'
getAnchorPosition :: (HasCallStack) => Bool -> OwlPFState -> SAutoLine -> Int -> XY
getAnchorPosition offsetAttach pfs sline@SAutoLine {..} anchorindex = r where
  mps = _sAutoLine_midpoints
  endindex = length mps + 1
  r = if anchorindex == 0
    then getEndpointPosition offsetAttach pfs sline True
    else if anchorindex == endindex
      then getEndpointPosition offsetAttach pfs sline False
      else if anchorindex > 0 && anchorindex < endindex
        then case mps `debugBangBang` (anchorindex-1) of
          SAutoLineConstraintFixed xy -> xy
        else error $ "out of bounds anchor index " <> show anchorindex



instance PotatoHandler AutoLineHandler where
  pHandlerName _ = handlerName_simpleLine
  pHandleMouse slh@AutoLineHandler {..} phi@PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) = let
    attachments = getAvailableAttachments False True _potatoHandlerInput_pFState _potatoHandlerInput_broadPhase _potatoHandlerInput_screenRegion
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
              , _autoLineEndPointHandler_lastAttachedBox = Nothing
            }
        }
      -- if shift is held down, ignore inputs, this allows us to shift + click to deselect
      -- TODO consider moving this into GoatWidget since it's needed by many manipulators
      MouseDragState_Down | elem KeyModifier_Shift _mouseDrag_modifiers -> Nothing
      MouseDragState_Down -> r where
        (_, sline) = fromJust $ maybeGetSLine _potatoHandlerInput_canvasSelection


        labels = getSortedSAutoLineLabelPositions _potatoHandlerInput_pFState sline

        findlabelfn (pos, _, llabel) = pos == _mouseDrag_to || does_lBox_contains_XY (getSAutoLineLabelBox pos llabel) _mouseDrag_to
        mfirstlabel = L.find findlabelfn labels
        firstlm = findFirstLineManipulator_NEW sline _autoLineHandler_offsetAttach _potatoHandlerInput_pFState rmd

        -- TODO update cache someday
        mclickonline = whichSubSegmentDidClick (_owlPFState_owlTree _potatoHandlerInput_pFState) sline _mouseDrag_to

        r = case (firstlm, mfirstlabel) of

          -- if clicked on endpoint
          (LMP_Endpoint isstart, _) -> Just $ def {
              _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler AutoLineEndPointHandler {
                  _autoLineEndPointHandler_isStart      = isstart
                  , _autoLineEndPointHandler_undoFirst  = False
                  , _autoLineEndPointHandler_isCreation = False
                  , _autoLineEndPointHandler_offsetAttach = _autoLineHandler_offsetAttach
                  , _autoLineEndPointHandler_attachStart = Nothing
                  , _autoLineEndPointHandler_attachEnd = Nothing
                  , _autoLineEndPointHandler_lastAttachedBox = Nothing
                }
            }

          -- click on line label or label anchor
          -- TODO right now clicking on line itself also allows you to move it (as oppose to just the anchor) is this what we want?
          (_, Just (_,index,_)) -> Just $
            def {
                _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler AutoLineLabelMoverHandler {
                  _autoLineLabelMoverHandler_anchorOffset  = 0
                  , _autoLineLabelMoverHandler_prevHandler = SomePotatoHandler slh
                  , _autoLineLabelMoverHandler_undoFirst   = False
                  , _autoLineLabelMoverHandler_labelIndex  = index
                }
              }

          -- if clicked on line but not on a handler, track the position
          (LMP_Nothing, _) | isJust mclickonline -> Just $ def {
              _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler slh {
                  _autoLineHandler_mDownManipulator = mclickonline
                }
            }

          -- did not click on manipulator, no capture
          (LMP_Nothing, _) -> Nothing

          (LMP_Midpoint i, _) -> rslt where
            handler = AutoLineMidPointHandler {
                _autoLineMidPointHandler_midPointIndex = i
                , _autoLineMidPointHandler_isMidpointCreation = False
                , _autoLineMidPointHandler_undoFirst  = False
                , _autoLineMidPointHandler_offsetAttach = _autoLineHandler_offsetAttach
              }
            rslt = pHandleMouse handler phi rmd

      MouseDragState_Dragging -> case _autoLineHandler_mDownManipulator of
        -- TODO BUG how does this happen? This shouldn't happen as we must capture all dragging operations (I'm pretty sure you already fixed this by implementing the undo on cancel)
        -- this can happen if we cancel in the middle of a drag operation (say), it will recreate an AutoLineHandler from the selection
        Nothing -> Nothing
        Just i -> r where
          handler = AutoLineMidPointHandler {
              _autoLineMidPointHandler_midPointIndex = i
              , _autoLineMidPointHandler_isMidpointCreation = True
              , _autoLineMidPointHandler_undoFirst  = False
              , _autoLineMidPointHandler_offsetAttach = _autoLineHandler_offsetAttach
            }
          r = pHandleMouse handler phi rmd

      -- TODO if down and up on line manipulator (text portion and not the anchor portion)


      -- if we click down and directly up in the same spot on the line, create a line label there and pass on input to AutoLineLabelHandler
      MouseDragState_Up -> case _autoLineHandler_mDownManipulator of
        Nothing -> Just def
        Just _ -> r where
          (rid, sal) = mustGetSLine _potatoHandlerInput_canvasSelection
          -- PERF cache someday...
          larlist = sAutoLine_to_lineAnchorsForRenderList _potatoHandlerInput_pFState sal
          (_, mpindex, reld) = getClosestPointOnLineFromLineAnchorsForRenderList larlist _mouseDrag_to
          newllabel = def {
              _sAutoLineLabel_index = mpindex
              , _sAutoLineLabel_position = SAutoLineLabelPositionRelative reld
            }
          r = Just def {
              _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler $ makeAutoLineLabelHandler_from_newLineLabel rid sal newllabel (SomePotatoHandler slh) phi rmd
            }
      -- TODO is this correct??
      MouseDragState_Cancelled -> Just def
  pHandleKeyboard _ PotatoHandlerInput {..} kbd = case kbd of
    -- TODO keyboard movement
    _                              -> Nothing
  pRenderHandler AutoLineHandler {..} phi@PotatoHandlerInput {..} = r where
    boxes = maybeRenderPoints (False, False) _autoLineHandler_offsetAttach (-1) phi
    attachmentBoxes = renderAttachments phi (Nothing, Nothing)
    labels = renderLabels phi False
    r = if _autoLineHandler_isCreation
      -- creation handlers are rendered by AutoLineEndPointHandler once dragging starts
      then HandlerRenderOutput attachmentBoxes
      else HandlerRenderOutput (boxes <> labels)

  pIsHandlerActive _ = HAS_Inactive
  pHandlerTool AutoLineHandler {..} = if _autoLineHandler_isCreation
    then Just Tool_Line
    else Nothing


-- handles dragging endpoints (which can be attached) and creating new lines
data AutoLineEndPointHandler = AutoLineEndPointHandler {
  _autoLineEndPointHandler_isStart        :: Bool -- either we are manipulating start, or we are manipulating end
  , _autoLineEndPointHandler_undoFirst    :: Bool
  , _autoLineEndPointHandler_isCreation   :: Bool
  , _autoLineEndPointHandler_offsetAttach :: Bool -- who sets this?
  -- where the current modified line is attached to (_autoLineEndPointHandler_attachStart will differ from actual line in the case when we start creating a line on mouse down)
  , _autoLineEndPointHandler_attachStart  :: Maybe Attachment
  , _autoLineEndPointHandler_attachEnd    :: Maybe Attachment
  , _autoLineEndPointHandler_lastAttachedBox :: Maybe Attachment
}




instance PotatoHandler AutoLineEndPointHandler where
  pHandlerName _ = handlerName_simpleLine_endPoint
  pHandleMouse slh@AutoLineEndPointHandler {..} PotatoHandlerInput {..} (RelMouseDrag MouseDrag {..}) = let
      mridssline = maybeGetSLine _potatoHandlerInput_canvasSelection
      attachments = getAvailableAttachments False True _potatoHandlerInput_pFState _potatoHandlerInput_broadPhase _potatoHandlerInput_screenRegion

      -- TODO change this so it tracks box we were attached to at the beggining for the duration of the AutoLineEndPointHandler drag  such that you can detach and reattach
      -- if we attached to some box we weren't already attached to
      mnewattachend = fmap fst . isOverAttachment _mouseDrag_to $ attachments

      -- if we attached to the box we were already attached to
      mprojectattachend = case mridssline of
        Nothing -> Nothing
        Just (_, ssline) -> r_2 where
          mattachedboxend = do
            aend <- if _autoLineEndPointHandler_isStart then _sAutoLine_attachStart ssline else _sAutoLine_attachEnd ssline
            box <- maybeGetAttachmentBox _autoLineEndPointHandler_offsetAttach _potatoHandlerInput_pFState aend
            return (box, aend)
          r_2 = do
            (box, aend) <- case mattachedboxend of
              -- if we didn't attach to the box we already attached to, see if we can attach to the last box we were attached to (w)
              Nothing -> case _autoLineEndPointHandler_lastAttachedBox of
                Just x -> do
                  box <- maybeGetAttachmentBox _autoLineEndPointHandler_offsetAttach _potatoHandlerInput_pFState x
                  return (box, x)
                Nothing -> Nothing
              Just x -> Just x
            fmap fst $ projectAttachment (_attachment_location aend) _mouseDrag_to (_attachment_target aend) box


      mattachend = msum [mprojectattachend, mnewattachend]

    in case _mouseDrag_state of
      MouseDragState_Down -> error "this should be handleed by AutoLineHandler"
      MouseDragState_Dragging -> Just r where
        rid = _superOwl_id $ selectionToSuperOwl _potatoHandlerInput_canvasSelection

        ssline = case mridssline of
          Just (_,x) -> x
          Nothing    -> def

        sslinestart = _sAutoLine_attachStart ssline
        sslineend = _sAutoLine_attachEnd ssline

        -- only attach on non trivial changes so we don't attach to our starting point
        nontrivialline = if _autoLineEndPointHandler_isStart
          then Just _mouseDrag_to /= (maybeGetAttachmentPosition _autoLineEndPointHandler_offsetAttach _potatoHandlerInput_pFState =<< sslineend)
          else Just _mouseDrag_to /= (maybeGetAttachmentPosition _autoLineEndPointHandler_offsetAttach _potatoHandlerInput_pFState =<< sslinestart)
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

        -- for creating new elt
        newEltPos = lastPositionInSelection (_owlPFState_owlTree _potatoHandlerInput_pFState) _potatoHandlerInput_selection
        lineToAdd = def {
            _sAutoLine_start = _mouseDrag_from
            , _sAutoLine_end = _mouseDrag_to
            , _sAutoLine_superStyle = _potatoDefaultParameters_superStyle _potatoHandlerInput_potatoDefaultParameters
            , _sAutoLine_lineStyle = _potatoDefaultParameters_lineStyle _potatoHandlerInput_potatoDefaultParameters
            , _sAutoLine_lineStyleEnd =
            _potatoDefaultParameters_lineStyleEnd _potatoHandlerInput_potatoDefaultParameters
            , _sAutoLine_attachStart = _autoLineEndPointHandler_attachStart
            , _sAutoLine_attachEnd = mattachendnontrivial
          }

        op = if _autoLineEndPointHandler_isCreation
          then makeAddEltLlama _potatoHandlerInput_pFState newEltPos (OwlItem (OwlInfo "<line>") $ OwlSubItemLine lineToAdd)
          else makeSetLlama $ (rid, SEltLine modifiedline)

        r = def {
            _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler slh {
                _autoLineEndPointHandler_undoFirst = True
                , _autoLineEndPointHandler_attachStart = if _autoLineEndPointHandler_isStart then mattachendnontrivial else _autoLineEndPointHandler_attachStart
                , _autoLineEndPointHandler_attachEnd = if not _autoLineEndPointHandler_isStart then mattachendnontrivial else _autoLineEndPointHandler_attachEnd
                , _autoLineEndPointHandler_lastAttachedBox = case mattachendnontrivial of
                  Nothing -> _autoLineEndPointHandler_lastAttachedBox
                  Just x -> Just x
              }
            , _potatoHandlerOutput_action = HOA_Preview $ Preview (previewOperation_fromUndoFirst _autoLineEndPointHandler_undoFirst) op
          }
      -- no need to return AutoLineHandler, it will be recreated from selection by goat
      MouseDragState_Up -> Just def {
          _potatoHandlerOutput_action = if _autoLineEndPointHandler_undoFirst then HOA_Preview Preview_Commit else HOA_Nothing
        }
      MouseDragState_Cancelled -> if _autoLineEndPointHandler_undoFirst then Just def { _potatoHandlerOutput_action = HOA_Preview Preview_Cancel } else Just def

  pHandleKeyboard _ PotatoHandlerInput {..} _ = Nothing
  pRenderHandler AutoLineEndPointHandler {..} phi@PotatoHandlerInput {..} = r where
    boxes = maybeRenderPoints (_autoLineEndPointHandler_isStart, not _autoLineEndPointHandler_isStart) _autoLineEndPointHandler_offsetAttach (-1) phi
    attachmentBoxes = renderAttachments phi (_autoLineEndPointHandler_attachStart, _autoLineEndPointHandler_attachEnd)
    r = HandlerRenderOutput (attachmentBoxes <> boxes)
  pIsHandlerActive _ = HAS_Active_Mouse
  pHandlerTool AutoLineEndPointHandler {..} = if _autoLineEndPointHandler_isCreation
    then Just Tool_Line
    else Nothing


-- TODO finish
{-
adjustLineLabelPositionsAfterModifyingOrAddingMidpoint ::
  (HasOwlTree a)
  => a
  -> SAutoLine --  ^ the previous line
  -> SAutoLine -- ^ the new line
  -> Maybe (Either Int Int) -- ^ Nothing is modify case, Just Left is creat, Just Right is delete
  -> SAutoLine
adjustLineLabelPositionsAfterModifyingOrAddingMidpoint ot old new mempindex = r where

  -- TODO need more than just this, need to copmute position too
  indexAdjust i = case mempindex of
    Nothing             -> i
    -- advance indices after addmpi since we are adding a midpoint
    Just (Left addmpi)  -> if i > addmpi then i+1 else i
    -- go bacak indices before delmpi since we are deleting a midpoint
    Just (Right delmpi) -> if i >= delmpi then i-1 else i


  oldlars = sAutoLine_to_lineAnchorsForRenderList ot old
  newlars = sAutoLine_to_lineAnchorsForRenderList ot new

  -- TODO
  -- compute previous LAR distances
  -- compute new LAR distances (after adjusting for midpoint index)
  -- adjust distance by the change in ratio

  r = undefined
-}



sAutoLine_addMidpoint :: Int -> XY -> SAutoLine -> SAutoLine
sAutoLine_addMidpoint mpindex pos sline = r where
  newmidpoints =  L.insertAt mpindex (SAutoLineConstraintFixed pos) (_sAutoLine_midpoints sline)


  -- TODO update line label position (whether you adjust index will depend on the position relative to where midpoint is added, right now we always assume label is to the left)
  fmapfn ll = if _sAutoLineLabel_index ll > mpindex
    then ll { _sAutoLineLabel_index = _sAutoLineLabel_index ll + 1}
    else ll
  newlabels = fmap fmapfn (_sAutoLine_labels sline)

  r = sline {
      _sAutoLine_midpoints = newmidpoints
      , _sAutoLine_labels = newlabels
    }

sAutoLine_modifyMidpoint :: Int -> XY -> SAutoLine -> SAutoLine
sAutoLine_modifyMidpoint mpindex pos sline = r where
  newmidpoints =  L.modifyAt mpindex (const $ SAutoLineConstraintFixed pos) (_sAutoLine_midpoints sline)
  -- TODO update line label position
  --fmapfn = undefined
  --newlabels = fmap fmapfn (_sAutoLine_labels sline)
  newlabels = _sAutoLine_labels sline

  r = sline {
      _sAutoLine_midpoints = newmidpoints
      , _sAutoLine_labels = newlabels
    }


sAutoLine_deleteMidpoint :: Int -> SAutoLine -> SAutoLine
sAutoLine_deleteMidpoint mpindex sline = r where
  newmidpoints =  L.deleteAt mpindex (_sAutoLine_midpoints sline)
  -- TODO update line label position (whether you adjust index will depend on the position relative to where midpoint is added, right now we always assume label is to the left)
  fmapfn ll = if _sAutoLineLabel_index ll > mpindex
    then ll { _sAutoLineLabel_index = _sAutoLineLabel_index ll - 1}
    else ll
  newlabels = fmap fmapfn (_sAutoLine_labels sline)

  r = sline {
      _sAutoLine_midpoints = newmidpoints
      , _sAutoLine_labels = newlabels
    }

-- handles dragging and creating new midpoints
data AutoLineMidPointHandler = AutoLineMidPointHandler{
  _autoLineMidPointHandler_midPointIndex        :: Int
  , _autoLineMidPointHandler_isMidpointCreation :: Bool
  , _autoLineMidPointHandler_undoFirst          :: Bool
  , _autoLineMidPointHandler_offsetAttach       :: Bool
}

instance PotatoHandler AutoLineMidPointHandler where
  pHandlerName _ = handlerName_simpleLine_midPoint
  pHandleMouse slh@AutoLineMidPointHandler {..} PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) = case _mouseDrag_state of
    -- this only happens in the click on existing midpoint case (creation case is handled by dragging)
    -- nothing to do here
    MouseDragState_Down -> assert (not _autoLineMidPointHandler_isMidpointCreation) $ Just $ captureWithNoChange slh
    MouseDragState_Dragging -> r where
      (rid, sline) = fromJust $ maybeGetSLine _potatoHandlerInput_canvasSelection

      -- TODO overlap adjacent issue, findFirstLineManipulator_NEW will midpoint instead of endpoint
      firstlm = findFirstLineManipulator_NEW sline _autoLineMidPointHandler_offsetAttach _potatoHandlerInput_pFState rmd

      -- index into _sAutoLine_midpoints
      -- in the '_autoLineMidPointHandler_isMidpointCreation' case, the midpoint index is AFTER the midpoint gets created
      -- `_autoLineMidPointHandler_midPointIndex == N` means we have `N-1 ... (x) ... N`
      -- so the new indexing is `N-1 ... N (x) ... N+1`
      mpindex = _autoLineMidPointHandler_midPointIndex

      -- TODO not working
      -- NOTE indexing of getAnchorPosition is offset from index into _autoLineMidPointHandler_midPointIndex
      ladjacentpos = getAnchorPosition _autoLineMidPointHandler_offsetAttach _potatoHandlerInput_pFState sline mpindex
      -- NOTE that this might be out of bounds in creation cases, but it won't get evaluated
      radjacentpos = getAnchorPosition _autoLineMidPointHandler_offsetAttach _potatoHandlerInput_pFState sline (mpindex+2)
      isoveradjacent = _mouseDrag_to == ladjacentpos || _mouseDrag_to == radjacentpos

      newsline = if _autoLineMidPointHandler_isMidpointCreation
        then sAutoLine_addMidpoint mpindex _mouseDrag_to sline
        else sAutoLine_modifyMidpoint mpindex _mouseDrag_to sline

      newslinedelete = sAutoLine_deleteMidpoint mpindex sline


      (diddelete, event) = case firstlm of
        -- create the new midpoint if none existed
        _ | _autoLineMidPointHandler_isMidpointCreation -> (False,) $ makeSetLlama $ (rid, SEltLine newsline)

        -- if overlapping existing ADJACENT endpoint do nothing (or undo if undo first)
        _ | isoveradjacent -> (True,) $ makeSetLlama (rid, SEltLine newslinedelete)

        -- normal case, update the midpoint position
        _ -> (False,) $ makeSetLlama $ (rid, SEltLine newsline)

      r = Just $ def {
          _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler slh {
              -- go back to creation case IF we deleted a midpoint AND we weren't already in creation case (this can happen if you have two mid/endpoints right next to each other and you drag from one to the other)
              _autoLineMidPointHandler_isMidpointCreation = diddelete && not _autoLineMidPointHandler_isMidpointCreation
              , _autoLineMidPointHandler_undoFirst  = True
            }
          , _potatoHandlerOutput_action = HOA_Preview $ Preview (previewOperation_fromUndoFirst _autoLineMidPointHandler_undoFirst) event
        }
    -- no need to return AutoLineHandler, it will be recreated from selection by goat
    MouseDragState_Up -> Just def {
        _potatoHandlerOutput_action = if _autoLineMidPointHandler_undoFirst then HOA_Preview Preview_Commit else HOA_Nothing
      }
    MouseDragState_Cancelled -> if _autoLineMidPointHandler_undoFirst then Just def { _potatoHandlerOutput_action = HOA_Preview Preview_Cancel } else Just def
  pRenderHandler AutoLineMidPointHandler {..} phi@PotatoHandlerInput {..} = r where
    boxes = maybeRenderPoints (False, False) _autoLineMidPointHandler_offsetAttach _autoLineMidPointHandler_midPointIndex phi
    -- TODO render mouse position as there may not actually be a midpoint there
    r = HandlerRenderOutput boxes
  pIsHandlerActive _ =  HAS_Active_Mouse

-- handles creating and moving text labels
data AutoLineLabelMoverHandler = AutoLineLabelMoverHandler {
    _autoLineLabelMoverHandler_anchorOffset  :: XY
    , _autoLineLabelMoverHandler_prevHandler :: SomePotatoHandler
    , _autoLineLabelMoverHandler_undoFirst   :: Bool
    , _autoLineLabelMoverHandler_labelIndex  :: Int
  }

-- TODO add support for moving line that does not exist yet
instance PotatoHandler AutoLineLabelMoverHandler where
  pHandlerName _ = handlerName_simpleLine_textLabelMover
  pHandleMouse slh@AutoLineLabelMoverHandler {..} phi@PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) = let

      -- TODO move to helper
      (rid, sal) = mustGetSLine _potatoHandlerInput_canvasSelection
      llabel = _sAutoLine_labels sal `debugBangBang` _autoLineLabelMoverHandler_labelIndex
      -- PERF cache someday...
      larlist = sAutoLine_to_lineAnchorsForRenderList _potatoHandlerInput_pFState sal
      (_, index, reld) = getClosestPointOnLineFromLineAnchorsForRenderList larlist _mouseDrag_to
      newl = llabel {
          _sAutoLineLabel_index = index
          , _sAutoLineLabel_position = SAutoLineLabelPositionRelative reld
        }

    in case _mouseDrag_state of

      MouseDragState_Down -> Just $ captureWithNoChange slh

      MouseDragState_Dragging -> r where
        newsal = sal {
            _sAutoLine_labels = L.setAt _autoLineLabelMoverHandler_labelIndex newl (_sAutoLine_labels sal)
          }
        op = makeSetLlama (rid, SEltLine newsal)
        r = Just def {
            _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler slh {
                _autoLineLabelMoverHandler_undoFirst = True
              }
            , _potatoHandlerOutput_action = HOA_Preview $ Preview (previewOperation_fromUndoFirst _autoLineLabelMoverHandler_undoFirst) op
          }

      MouseDragState_Up -> Just def {
          -- go back to AutoLineLabelHandler on completion
          _potatoHandlerOutput_nextHandler = if not _autoLineLabelMoverHandler_undoFirst
            -- if _autoLineLabelMoverHandler_undoFirst is false, this means we didn't drag at all, in which case go to label edit handler
            then Just $ SomePotatoHandler $
              makeAutoLineLabelHandler_from_labelIndex _autoLineLabelMoverHandler_labelIndex _autoLineLabelMoverHandler_prevHandler phi rmd
            -- TODO consider also going into edit handler after dragging an endpoint, but for now, just go back to the previous handler (which will be AutoLineHandler)
            else Just (_autoLineLabelMoverHandler_prevHandler)

          , _potatoHandlerOutput_action = if _autoLineLabelMoverHandler_undoFirst then HOA_Preview Preview_Commit else HOA_Nothing
        }

      MouseDragState_Cancelled -> Just def {
          -- go back to previous handler on cancel (could be AutoLineHandler or AutoLineLabelHandler)
          _potatoHandlerOutput_nextHandler = Just (_autoLineLabelMoverHandler_prevHandler)
          , _potatoHandlerOutput_action = if _autoLineLabelMoverHandler_undoFirst then HOA_Preview Preview_Cancel else HOA_Nothing
        }


  pRenderHandler AutoLineLabelMoverHandler {..} phi@PotatoHandlerInput {..} =  r where
    labels = renderLabels phi False
    r = HandlerRenderOutput labels

  pIsHandlerActive _ = HAS_Active_Mouse



sAutoLine_deleteLabel :: Int -> SAutoLine -> SAutoLine
sAutoLine_deleteLabel labelindex sline = r where
  newlabels =  L.deleteAt labelindex (_sAutoLine_labels sline)
  r = sline {
      _sAutoLine_labels = newlabels
    }

-- handles modifying text labels
data AutoLineLabelHandler = AutoLineLabelHandler {
    _autoLineLabelHandler_active        :: Bool
    , _autoLineLabelHandler_state       :: TextInputState
    , _autoLineLabelHandler_prevHandler :: SomePotatoHandler
    , _autoLineLabelHandler_undoFirst   :: Bool

    , _autoLineLabelHandler_labelIndex  :: Int
    , _autoLineLabelHandler_lineLabel   :: SAutoLineLabel

    -- this is needed to determine if erasing the last character in the label deletes the line label or undos the last operation
    , _autoLineLabelHandler_creation    :: Bool
  }


getSAutoLineLabelBox :: XY -> SAutoLineLabel -> LBox
getSAutoLineLabelBox (V2 x y) llabel = r where
  w = T.length (_sAutoLineLabel_text llabel)
  r = LBox (V2 (x - w `div` 2) y) (V2 w 1)

updateAutoLineLabelHandlerState :: (HasOwlTree a) => a -> Bool -> CanvasSelection -> AutoLineLabelHandler -> AutoLineLabelHandler
updateAutoLineLabelHandlerState ot reset selection slh@AutoLineLabelHandler {..} = r where

  -- TODO move to helper
  (_, sal) = mustGetSLine selection
  llabel = if T.null (TZ.value (_textInputState_zipper _autoLineLabelHandler_state))
    then _autoLineLabelHandler_lineLabel
    -- if we are not creating a new label pull the SAutoLineLabel again because it might have changed
    else _sAutoLine_labels sal `debugBangBang` _autoLineLabelHandler_labelIndex
  newtext = _sAutoLineLabel_text llabel
  pos = getSAutoLineLabelPosition ot sal llabel


  width = maxBound :: Int -- line label text always overflows
  box = getSAutoLineLabelBox pos llabel


  r = slh {
    _autoLineLabelHandler_state = _autoLineLabelHandler_state {
          _textInputState_original = if reset then Just newtext else _textInputState_original _autoLineLabelHandler_state
          , _textInputState_displayLines = TZ.displayLinesWithAlignment TZ.TextAlignment_Left width () () (_textInputState_zipper _autoLineLabelHandler_state)
          , _textInputState_box = box
      }
    , _autoLineLabelHandler_undoFirst = if reset
      then False
      else _autoLineLabelHandler_undoFirst

    -- the previously stored label may have been modified so update it with the new one
    , _autoLineLabelHandler_lineLabel = llabel
  }

-- | make a TextInputState from a SAutoLineLabel on the SAutoLine
-- the SAutoLineLabel does not need to exist in the SAutoLine
makeAutoLineLabelInputState_from_lineLabel :: REltId -> SAutoLine -> SAutoLineLabel -> PotatoHandlerInput -> RelMouseDrag -> TextInputState
makeAutoLineLabelInputState_from_lineLabel rid sal llabel PotatoHandlerInput {..} rmd = r where
  ogtext = _sAutoLineLabel_text llabel
  pos = getSAutoLineLabelPosition _potatoHandlerInput_pFState sal llabel
  box = getSAutoLineLabelBox pos llabel

  width = maxBound :: Int -- line label text always overflows
  ogtz = TZ.fromText ogtext
  tis = TextInputState {
      _textInputState_rid = rid
      , _textInputState_original   = Just ogtext
      , _textInputState_zipper   = ogtz
      , _textInputState_box = box
      , _textInputState_displayLines = TZ.displayLinesWithAlignment TZ.TextAlignment_Left width () () ogtz
    }
  r = mouseText tis rmd

makeAutoLineLabelInputState_from_labelIndex :: REltId -> SAutoLine -> Int -> PotatoHandlerInput -> RelMouseDrag -> TextInputState
makeAutoLineLabelInputState_from_labelIndex rid sal labelindex phi@PotatoHandlerInput {..} rmd = r where
  llabel = _sAutoLine_labels sal `debugBangBang` labelindex
  r = makeAutoLineLabelInputState_from_lineLabel rid sal llabel phi rmd

makeAutoLineLabelHandler_from_newLineLabel :: REltId -> SAutoLine -> SAutoLineLabel -> SomePotatoHandler -> PotatoHandlerInput -> RelMouseDrag -> AutoLineLabelHandler
makeAutoLineLabelHandler_from_newLineLabel rid sal llabel prev phi rmd = AutoLineLabelHandler {
    _autoLineLabelHandler_active = False
    , _autoLineLabelHandler_state = (makeAutoLineLabelInputState_from_lineLabel rid sal llabel phi rmd)
    , _autoLineLabelHandler_prevHandler = prev
    , _autoLineLabelHandler_undoFirst = False
    , _autoLineLabelHandler_labelIndex = 0
    , _autoLineLabelHandler_lineLabel = llabel
    , _autoLineLabelHandler_creation = True
  }


makeAutoLineLabelHandler_from_labelIndex :: Int -> SomePotatoHandler -> PotatoHandlerInput -> RelMouseDrag -> AutoLineLabelHandler
makeAutoLineLabelHandler_from_labelIndex labelindex prev phi@PotatoHandlerInput {..} rmd = r where
  (rid, sal) = mustGetSLine _potatoHandlerInput_canvasSelection
  llabel = _sAutoLine_labels sal `debugBangBang` labelindex
  r = AutoLineLabelHandler {
      _autoLineLabelHandler_active = False
      , _autoLineLabelHandler_state = moveToEol $ makeAutoLineLabelInputState_from_labelIndex rid sal labelindex phi rmd
      , _autoLineLabelHandler_prevHandler = prev
      , _autoLineLabelHandler_undoFirst = False
      , _autoLineLabelHandler_labelIndex = labelindex
      , _autoLineLabelHandler_lineLabel = llabel
      , _autoLineLabelHandler_creation = False
    }




-- TODO get rid of LBox arg, not used anymore
-- | just a helper for pHandleMouse
handleMouseDownOrFirstUpForAutoLineLabelHandler :: AutoLineLabelHandler -> PotatoHandlerInput -> RelMouseDrag -> Bool -> Maybe PotatoHandlerOutput
handleMouseDownOrFirstUpForAutoLineLabelHandler slh@AutoLineLabelHandler {..} phi@PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) isdown = r where
  clickInside = does_lBox_contains_XY (_textInputState_box _autoLineLabelHandler_state) _mouseDrag_to
  newState = mouseText _autoLineLabelHandler_state rmd
  r = if clickInside
    then Just $ def {
        _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler slh {
            _autoLineLabelHandler_active = isdown
            , _autoLineLabelHandler_state = newState
          }
      }
    -- pass the input on to the base handler (so that you can interact with BoxHandler mouse manipulators too)
    else pHandleMouse _autoLineLabelHandler_prevHandler phi rmd

instance PotatoHandler AutoLineLabelHandler where
  pHandlerName _ = handlerName_simpleLine_textLabel
  pHandleMouse slh' phi@PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) = let
      slh = updateAutoLineLabelHandlerState _potatoHandlerInput_pFState False _potatoHandlerInput_canvasSelection slh'
    in case _mouseDrag_state of
      -- TODO if click on drag anchor modifier thingy
      --    in this case, don't forget to reset creation and undofirst states
      MouseDragState_Down -> handleMouseDownOrFirstUpForAutoLineLabelHandler slh phi rmd True
            -- TODO if click on handler, go into mover handler
            {- Just $ SomePotatoHandler AutoLineLabelMoverHandler {
                _autoLineLabelMoverHandler_prevHandler = SomePotatoHandler slh
                , _autoLineLabelMoverHandler_anchorOffset = 0
                , _autoLineLabelMoverHandler_undoFirst = True
                , _autoLineLabelMoverHandler_labelIndex = 0
              } -}

      -- TODO drag select text someday
      MouseDragState_Dragging -> Just $ captureWithNoChange slh
      MouseDragState_Up -> if not (_autoLineLabelHandler_active slh)
        then handleMouseDownOrFirstUpForAutoLineLabelHandler slh phi rmd False
        else Just $ def {
            _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler slh {
                _autoLineLabelHandler_active = False
              }
          }
      MouseDragState_Cancelled -> Just $ captureWithNoChange slh

  pHandleKeyboard slh' PotatoHandlerInput {..} (KeyboardData k _) = let
      -- this regenerates displayLines unecessarily but who cares
      slh = updateAutoLineLabelHandlerState _potatoHandlerInput_pFState False _potatoHandlerInput_canvasSelection slh'
      -- TODO cache this in slh
      (rid, sal) = mustGetSLine _potatoHandlerInput_canvasSelection
    in case k of
      -- Escape or Return
      _ | k == KeyboardKey_Esc || k == KeyboardKey_Return -> Just $ def { _potatoHandlerOutput_nextHandler = Just (_autoLineLabelHandler_prevHandler slh) }

      -- TODO should only capture stuff caught by inputSingleLineZipper
      --  make sure pRefreshHandler clears the handler or sets it back to creation case in the event that an undo operation clears the handler
      _ -> Just r where

        -- TODO decide what to do with mods

        oldtais = _autoLineLabelHandler_state slh
        oldtextnull = T.null (TZ.value (_textInputState_zipper oldtais))

        -- if text was created, create the line label, you shouldn't need to but double check that there was no text before
        doescreate = oldtextnull
        (changed, newtais) = inputSingleLineZipper oldtais k
        newtext = TZ.value (_textInputState_zipper newtais)
        oldlabel = _autoLineLabelHandler_lineLabel slh
        newlabel = oldlabel {
            _sAutoLineLabel_text = newtext
          }


        newsal_creation = sal {
            _sAutoLine_labels = newlabel : _sAutoLine_labels sal
          }

        newsal_update = sal {
            _sAutoLine_labels = L.setAt (_autoLineLabelHandler_labelIndex slh) newlabel (_sAutoLine_labels sal)
          }

        -- if all text was removed, delete the line label, you shouldn't need to but double check that there was actually a label to delete
        doesdelete = T.null newtext && not oldtextnull
        newsal_delete = sAutoLine_deleteLabel (_autoLineLabelHandler_labelIndex slh) sal

        newsal = if doesdelete
          then newsal_delete
          else if doescreate
            then newsal_creation
            else newsal_update

        action = if not changed
          then HOA_Nothing
          else if doesdelete && _autoLineLabelHandler_creation slh
            -- if we deleted a newly created line just undo the last operation
            then HOA_Preview Preview_Cancel
            else HOA_Preview $ Preview (previewOperation_fromUndoFirst (_autoLineLabelHandler_undoFirst slh)) $ makeSetLlama (rid, SEltLine newsal)


        r = def {
            _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler slh {
                _autoLineLabelHandler_state  = newtais
                , _autoLineLabelHandler_undoFirst = case action of
                  HOA_Nothing      -> _autoLineLabelHandler_undoFirst slh
                  HOA_Preview Preview_Cancel -> False
                  _            -> True
              }
            , _potatoHandlerOutput_action = action
          }

  pRefreshHandler slh PotatoHandlerInput {..} =  if Seq.null (unCanvasSelection _potatoHandlerInput_canvasSelection)
    then Nothing -- selection was deleted or something
    else if rid /= (_textInputState_rid $ _autoLineLabelHandler_state slh)
      then Nothing -- selection was change to something else
      else case selt of
        -- TODO proper regeneration of AutoLineLabelHandler (this is only needed when you support remote events)
        SEltLine _ -> Nothing
        _          -> Nothing
      where
        sowl = selectionToSuperOwl _potatoHandlerInput_canvasSelection
        rid = _superOwl_id sowl
        selt = superOwl_toSElt_hack sowl

  pRenderHandler slh' PotatoHandlerInput {..} = r where
    slh = updateAutoLineLabelHandlerState _potatoHandlerInput_pFState False _potatoHandlerInput_canvasSelection slh'

    -- consider rendering endpoints?

    -- TODO render label mover anchor with offset 1

    -- render the text cursor
    btis = _autoLineLabelHandler_state slh
    r = makeTextHandlerRenderOutput btis

  -- TODO set properly
  pIsHandlerActive slh = if _autoLineLabelHandler_active slh then HAS_Active_Mouse else HAS_Active_Keyboard
