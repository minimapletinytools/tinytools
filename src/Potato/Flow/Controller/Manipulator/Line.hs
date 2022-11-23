{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Manipulator.Line (
  AutoLineHandler(..)
) where

import           Relude

import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.Manipulator.Common
import           Potato.Flow.Controller.Types
import           Potato.Flow.Methods.LineDrawer
import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.OwlItem
import Potato.Flow.OwlWorkspace
import Potato.Flow.BroadPhase
import Potato.Flow.OwlState
import Potato.Flow.Owl
import           Potato.Flow.Attachments
import Potato.Flow.Llama
import Potato.Flow.Controller.Manipulator.TextInputState
import qualified Potato.Data.Text.Zipper                          as TZ

import           Control.Exception
import Data.Maybe (catMaybes)
import           Data.Default
import qualified Data.Sequence                             as Seq
import qualified Data.List as L
import qualified Data.List.Index as L

import Data.Maybe (fromJust)

maybeGetSLine :: CanvasSelection -> Maybe (REltId, SAutoLine)
maybeGetSLine selection = if Seq.length (unCanvasSelection selection) /= 1
  then Nothing
  else case superOwl_toSElt_hack sowl of
    SEltLine sline  -> Just (rid, sline)
    selt -> Nothing
    where
      sowl = selectionToSuperOwl selection
      rid = _superOwl_id sowl

mustGetSLine :: CanvasSelection -> (REltId, SAutoLine)
mustGetSLine = fromJust . maybeGetSLine

-- TODO TEST
-- TODO move me elsewhere
getAvailableAttachments :: Bool -> Bool -> OwlPFState -> BroadPhaseState -> LBox -> [(Attachment, XY)]
getAvailableAttachments includeNoBorder offsetBorder pfs bps screenRegion = r where
  culled = broadPhase_cull screenRegion (_broadPhaseState_bPTree bps)
  -- you could silently fail here by ignoring maybes but that would definitely be an indication of a bug so we fail here instead (you could do a better job about dumping debug info though)
  sowls = fmap (hasOwlTree_mustFindSuperOwl pfs) culled
  -- TODO sort sowls
  fmapfn sowl = fmap (\(a,p) -> (Attachment (_superOwl_id sowl) a, p)) $ owlItem_availableAttachments includeNoBorder offsetBorder (_superOwl_elt sowl)
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
maybeRenderPoints :: (HasCallStack) => (Bool,Bool) -> Bool -> Int -> PotatoHandlerInput -> [RenderHandle]
maybeRenderPoints (highlightstart, highlightend) offsetAttach midpointhighlightindex PotatoHandlerInput {..} = r where
  -- in creation cases, _potatoHandlerInput_canvasSelection might not be a line so we need maybes throughout this function (you probably should have written using do notation)
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


data AutoLineHandler = AutoLineHandler {
    _autoLineHandler_isCreation :: Bool
    , _autoLineHandler_mDownManipulator :: Maybe Int
    -- TODO who sets this?
    , _autoLineHandler_offsetAttach :: Bool
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


-- returns index into midpoints if we clicked on the line
-- i.e. an index of 0 means we clicked somewhere between endpoints 0 and 1 (not including 1)
  -- TODO ☝🏽 is not true, need to fix
-- if `index == length midpoints` then point is between last midpoint and endpoint
whereOnLineDidClick :: OwlTree -> SAutoLine -> Maybe LineAnchorsForRender -> XY -> Maybe Int
whereOnLineDidClick ot sline@SAutoLine {..} manchors xy = r where
  anchors = case manchors of
    Nothing -> sSimpleLineNewRenderFnComputeCache ot sline
    Just x -> x
  r = lineAnchorsForRender_findIntersectingSubsegment anchors xy


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
  lars = sAutoLine_to_lineAnchorsForRenders ot sline
  r = fmap fst $ L.ifind (\_ lar -> isJust $ lineAnchorsForRender_findIntersectingSubsegment lar pos) lars



getEndpointPosition ::  Bool -> OwlPFState -> SAutoLine -> Bool -> XY
getEndpointPosition offsetAttach pfs SAutoLine {..} isstart = if isstart
  then fromMaybe _sAutoLine_start $ maybeGetAttachmentPosition offsetAttach pfs =<< _sAutoLine_attachStart
  else trace (show (maybeGetAttachmentPosition offsetAttach pfs =<< _sAutoLine_attachEnd) <> " " <> show _sAutoLine_end) $ fromMaybe _sAutoLine_end $ maybeGetAttachmentPosition offsetAttach pfs =<< _sAutoLine_attachEnd



-- |
-- see indexing information in 'whichSubSegmentDidClick'
getAnchorPosition :: Bool -> OwlPFState -> SAutoLine -> Int -> XY
getAnchorPosition offsetAttach pfs sline@SAutoLine {..} anchorindex = trace ("POS" <> show anchorindex <> show endindex) $ r where
  mps = _sAutoLine_midpoints
  endindex = length mps + 1
  r = if anchorindex == 0
    then getEndpointPosition offsetAttach pfs sline True
    else if anchorindex == endindex
      then getEndpointPosition offsetAttach pfs sline False
      else if anchorindex > 0 && anchorindex < endindex
        then case mps L.!! (anchorindex-1) of
          SAutoLineConstraintFixed xy -> xy
        else error $ "out of bounds anchor index " <> show anchorindex



instance PotatoHandler AutoLineHandler where
  pHandlerName _ = handlerName_simpleLine
  pHandleMouse slh@AutoLineHandler {..} phi@PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) = let
    mridssline = maybeGetSLine _potatoHandlerInput_canvasSelection
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
            }
        }
      -- if shift is held down, ignore inputs, this allows us to shift + click to deselect
      -- TODO consider moving this into GoatWidget since it's needed by many manipulators
      MouseDragState_Down | elem KeyModifier_Shift _mouseDrag_modifiers -> Nothing
      MouseDragState_Down -> r where
        (_, sline) = fromJust $ maybeGetSLine _potatoHandlerInput_canvasSelection
        firstlm = findFirstLineManipulator_NEW sline _autoLineHandler_offsetAttach _potatoHandlerInput_pFState rmd


        -- TODO update cache someday
        mclickonline = whichSubSegmentDidClick (_owlPFState_owlTree _potatoHandlerInput_pFState) sline _mouseDrag_to

        r = case firstlm of

          -- if clicked on line but not on a handler, track the position
          LMP_Nothing | isJust mclickonline -> Just $ def {
              _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler slh {
                  _autoLineHandler_mDownManipulator = mclickonline
                }
            }

          -- did not click on manipulator, no capture
          LMP_Nothing -> Nothing

          LMP_Midpoint i -> rslt where
            handler = AutoLineMidPointHandler {
                _autoLineMidPointHandler_midPointIndex = i
                , _autoLineMidPointHandler_isMidpointCreation = False
                , _autoLineMidPointHandler_undoFirst  = False
                , _autoLineMidPointHandler_offsetAttach = _autoLineHandler_offsetAttach
              }
            rslt = pHandleMouse handler phi rmd

          LMP_Endpoint isstart -> Just $ def {
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
      MouseDragState_Up -> case _autoLineHandler_mDownManipulator of
        Nothing -> Just def
        Just _ -> r where
          -- TODO setup properly
          handler = makeAutoLineLabelHandler (SomePotatoHandler slh) _potatoHandlerInput_canvasSelection rmd
          r = pHandleMouse handler phi rmd
      -- TODO is this correct??
      MouseDragState_Cancelled -> Just def
  pHandleKeyboard _ PotatoHandlerInput {..} kbd = case kbd of
    -- TODO keyboard movement
    _                              -> Nothing
  pRenderHandler AutoLineHandler {..} phi@PotatoHandlerInput {..} = r where
    boxes = maybeRenderPoints (False, False) _autoLineHandler_offsetAttach (-1) phi
    -- TODO P3 set attach endpoints from currently selected line (it's really not necessary though since the line handler attachments cover these)
    attachmentBoxes = renderAttachments phi (Nothing, Nothing)
    r = if _autoLineHandler_isCreation
      -- creation handlers are rendered by AutoLineEndPointHandler once dragging starts
      then emptyHandlerRenderOutput
      else HandlerRenderOutput (attachmentBoxes <> boxes)

  pIsHandlerActive _ = False
  pHandlerTool AutoLineHandler {..} = if _autoLineHandler_isCreation
    then Just Tool_Line
    else Nothing


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
      attachments = getAvailableAttachments False True _potatoHandlerInput_pFState _potatoHandlerInput_broadPhase _potatoHandlerInput_screenRegion
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
        llama = makeSetLlama $ (rid, SEltLine modifiedline)

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
      MouseDragState_Cancelled -> if _autoLineEndPointHandler_undoFirst then Just def { _potatoHandlerOutput_pFEvent = Just WSEUndo } else Just def

  pHandleKeyboard _ PotatoHandlerInput {..} kbd = Nothing
  pRenderHandler AutoLineEndPointHandler {..} phi@PotatoHandlerInput {..} = r where
    boxes = maybeRenderPoints (_autoLineEndPointHandler_isStart, not _autoLineEndPointHandler_isStart) _autoLineEndPointHandler_offsetAttach (-1) phi
    attachmentBoxes = renderAttachments phi (_autoLineEndPointHandler_attachStart, _autoLineEndPointHandler_attachEnd)
    r = HandlerRenderOutput (attachmentBoxes <> boxes)
  pIsHandlerActive _ = True
  pHandlerTool AutoLineEndPointHandler {..} = if _autoLineEndPointHandler_isCreation
    then Just Tool_Line
    else Nothing

-- handles dragging and creating new midpoints
data AutoLineMidPointHandler = AutoLineMidPointHandler{
  _autoLineMidPointHandler_midPointIndex :: Int
  , _autoLineMidPointHandler_isMidpointCreation :: Bool
  , _autoLineMidPointHandler_undoFirst :: Bool
  , _autoLineMidPointHandler_offsetAttach :: Bool
}

instance PotatoHandler AutoLineMidPointHandler where
  pHandlerName _ = handlerName_simpleLine_midPoint
  pHandleMouse slh@AutoLineMidPointHandler {..} PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) = let
      aoeu = undefined
    in case _mouseDrag_state of
      -- this only happens in the click on existing midpoint case (creation case is handled by dragging)
      -- nothing to do here
      MouseDragState_Down -> assert (not _autoLineMidPointHandler_isMidpointCreation) $ Just $ captureWithNoChange slh
      MouseDragState_Dragging -> r where
        (rid, sline) = fromJust $ maybeGetSLine _potatoHandlerInput_canvasSelection

        -- TODO overlap adjacent issue, findFirstLineManipulator_NEW will midpoint instead of endpoint
        firstlm = findFirstLineManipulator_NEW sline _autoLineMidPointHandler_offsetAttach _potatoHandlerInput_pFState rmd

        mps = _sAutoLine_midpoints sline
        nmps = length mps

        -- index into _sAutoLine_midpoints
        -- in the '_autoLineMidPointHandler_isMidpointCreation' case, the midpoint index is AFTER the midpoint gets created
        -- `_autoLineMidPointHandler_midPointIndex == N` means we have `N-1 ... (x) ... N`
        -- so the new indexing is `N-1 ... N (x) ... N+1`
        mpindex = _autoLineMidPointHandler_midPointIndex

        -- TODO not working
        -- NOTE indexing of getAnchorPosition is offset from index into _autoLineMidPointHandler_midPointIndex
        ladjacentpos = getAnchorPosition _autoLineMidPointHandler_offsetAttach _potatoHandlerInput_pFState sline mpindex
        -- NOTE that this is out of bounds in creation cases, but it won't get evaluated
        radjacentpos = getAnchorPosition _autoLineMidPointHandler_offsetAttach _potatoHandlerInput_pFState sline (mpindex+2)
        isoveradjacent = traceShowId $ trace (show ladjacentpos <> " : " <> show _mouseDrag_to <> " : " <> show radjacentpos <> " - " <> show mpindex <> " " <> show sline) $ _mouseDrag_to == ladjacentpos || _mouseDrag_to == radjacentpos

        newsline = sline {
            _sAutoLine_midpoints = if _autoLineMidPointHandler_isMidpointCreation
              then L.insertAt mpindex (SAutoLineConstraintFixed _mouseDrag_to) mps
              else L.modifyAt mpindex (const $ SAutoLineConstraintFixed _mouseDrag_to) mps
          }

        newslinedelete = sline {
            _sAutoLine_midpoints = L.deleteAt mpindex mps
          }

        mevent = case firstlm of
          -- create the new midpoint if none existed
          _ | _autoLineMidPointHandler_isMidpointCreation -> assert (not _autoLineMidPointHandler_undoFirst) $  Just $
            WSEApplyLlama (_autoLineMidPointHandler_undoFirst, makeSetLlama $ (rid, SEltLine newsline))

          -- if overlapping existing ADJACENT endpoint do nothing (or undo if undo first)
          _ | isoveradjacent -> Just $ WSEApplyLlama (_autoLineMidPointHandler_undoFirst, makeSetLlama (rid, SEltLine newslinedelete))

          -- normal case, update the midpoint position
          _ -> Just $
            WSEApplyLlama (_autoLineMidPointHandler_undoFirst, makeSetLlama $ (rid, SEltLine newsline))

        newundostate = case mevent of
          Nothing -> False
          Just WSEUndo -> False
          _ -> True

        r = Just $ def {
            _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler slh {
                -- NOTE that upon creation of AutoLineMidPointHandler `_autoLineMidPointHandler_isMidpointCreation` may not be equal to `not _autoLineMidPointHandler_undoFirst`
                _autoLineMidPointHandler_isMidpointCreation = not newundostate
                , _autoLineMidPointHandler_undoFirst  = newundostate
              }
            , _potatoHandlerOutput_pFEvent = mevent
          }
      -- no need to return AutoLineHandler, it will be recreated from selection by goat
      MouseDragState_Up -> Just def
      MouseDragState_Cancelled -> if _autoLineMidPointHandler_undoFirst then Just def { _potatoHandlerOutput_pFEvent = Just WSEUndo } else Just def

  pRenderHandler AutoLineMidPointHandler {..} phi@PotatoHandlerInput {..} = r where
    boxes = maybeRenderPoints (False, False) _autoLineMidPointHandler_offsetAttach _autoLineMidPointHandler_midPointIndex phi
    -- TODO render mouse position as there may not actually be a midpoint there
    r = HandlerRenderOutput boxes
  pIsHandlerActive _ = True


-- WIP BELOW THIS LINE

-- handles creating and modifying text labels
data AutoLineLabelHandler = AutoLineLabelHandler {
    _autoLineLabelHandler_isActive :: Bool
    , _autoLineLabelHandler_state :: TextInputState
    -- probably not necessary?
    , _autoLineLabelHandler_prevHandler :: SomePotatoHandler
    , _autoLineLabelHandler_undoFirst :: Bool

    , _autoLineLabelHandler_isCreation :: Bool
    , _autoLineLabelHandler_labelIndex :: Int
  }

-- TODO pass in existing line label or location of new line label
makeAutoLineLabelInputState :: REltId -> SAutoLine -> RelMouseDrag -> TextInputState
makeAutoLineLabelInputState rid sline rmd = r where

  -- TODO figure out which line label we are editing or create a new one
  mogtext = undefined

  -- TODO figure out box of line label we are editing
  box = undefined

  ogtz = TZ.fromText (fromMaybe "" mogtext)
  tis = TextInputState {
      _textInputState_rid = rid
      , _textInputState_original   = mogtext
      , _textInputState_zipper   = ogtz

      -- these fields get updated in next pass
      , _textInputState_box = error "expected to be filled"
      , _textInputState_displayLines = error "expected to be filled"
    }
  r = mouseText tis box rmd (V2 1 0)

makeAutoLineLabelHandler :: SomePotatoHandler -> CanvasSelection -> RelMouseDrag -> AutoLineLabelHandler
makeAutoLineLabelHandler prev selection rmd = AutoLineLabelHandler {
    _autoLineLabelHandler_isActive = False
    , _autoLineLabelHandler_state = uncurry makeAutoLineLabelInputState (mustGetSLine selection) rmd
    , _autoLineLabelHandler_prevHandler = prev
    , _autoLineLabelHandler_undoFirst = False
  }

data BoxTextHandler = BoxTextHandler {
    -- TODO rename to active
    _boxTextHandler_isActive      :: Bool
    , _boxTextHandler_state       :: TextInputState
    -- TODO you can prob delete this now, we don't persist state between sub handlers in this case
    , _boxTextHandler_prevHandler :: SomePotatoHandler
    , _boxTextHandler_undoFirst   :: Bool
  }

instance PotatoHandler AutoLineLabelHandler where
  pHandlerName _ = handlerName_simpleLine_textLabel
  pHandleMouse slh@AutoLineLabelHandler {..} PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) = Just def
  pHandleKeyboard slh@AutoLineLabelHandler {..} PotatoHandlerInput {..} (KeyboardData k _) = undefined
  pRenderHandler AutoLineLabelHandler {..} phi@PotatoHandlerInput {..} = undefined
  pIsHandlerActive _ = undefined
