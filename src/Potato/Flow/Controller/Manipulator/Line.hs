{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Manipulator.Line (
  SimpleLineHandler(..)
) where

import           Relude

import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.Manipulator.Common
import           Potato.Flow.Controller.Types
import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.Types
import           Potato.Flow.OwlWorkspace
import Potato.Flow.BroadPhase
import           Potato.Flow.OwlState
import           Potato.Flow.Owl
import           Potato.Flow.Attachments
import Potato.Flow.Llama

import           Control.Exception
import           Data.Default
import           Data.Dependent.Sum                        (DSum ((:=>)))
import qualified Data.IntMap                               as IM
import qualified Data.Sequence                             as Seq


getSLine :: CanvasSelection -> (REltId, SSimpleLine)
getSLine selection = case superOwl_toSElt_hack sowl of
  SEltLine sline  -> (rid, sline)
  selt -> error $ "expected SSimpleLine, got " <> show selt
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


-- TODO rename to AutoLine
data SimpleLineHandler = SimpleLineHandler {
    _simpleLineHandler_isStart      :: Bool -- either we are manipulating start, or we are manipulating end
    , _simpleLineHandler_undoFirst  :: Bool
    , _simpleLineHandler_isCreation :: Bool
    , _simpleLineHandler_active     :: Bool

    , _simpleLineHandler_original :: SSimpleLine -- track original so we can set proper "undo" point with undoFirst operations

    , _simpleLineHandler_offsetAttach :: Bool -- who sets this?

    -- where the current modified line is attached to (_simpleLineHandler_attachStart will differ from actual line in the case when we start creating a line on mouse down)
    , _simpleLineHandler_attachStart :: Maybe Attachment
    , _simpleLineHandler_attachEnd :: Maybe Attachment
  } deriving (Show)

instance Default SimpleLineHandler where
  def = SimpleLineHandler {
      _simpleLineHandler_isStart = False
      , _simpleLineHandler_undoFirst = False
      , _simpleLineHandler_isCreation = False
      , _simpleLineHandler_active = False
      , _simpleLineHandler_original = def
      , _simpleLineHandler_offsetAttach = True
      , _simpleLineHandler_attachStart = Nothing
      , _simpleLineHandler_attachEnd = Nothing
    }


findFirstLineManipulator :: Bool -> OwlPFState -> RelMouseDrag -> CanvasSelection -> Maybe Bool
findFirstLineManipulator offsetBorder pfs (RelMouseDrag MouseDrag {..}) (CanvasSelection selection) = assert (Seq.length selection == 1) $ r where
  msowl = Seq.lookup 0 selection
  selt = case msowl of
    Nothing -> error "expected selection"
    Just sowl -> superOwl_toSElt_hack sowl
  r = case selt of
    SEltLine SSimpleLine {..} ->
      let
        start = fromMaybe _sSimpleLine_start $ maybeLookupAttachment _sSimpleLine_attachStart offsetBorder pfs
        end = fromMaybe _sSimpleLine_end $ maybeLookupAttachment _sSimpleLine_attachEnd offsetBorder pfs
      in
        if _mouseDrag_to == start then Just True
          else if _mouseDrag_to == end then Just False
            else Nothing
    x -> error $ "expected SSimpleLine in selection but got " <> show x <> " instead"



dontChangeUnlessNecessary :: Eq a => (Maybe a, Maybe a) -> Maybe (Maybe a, Maybe a)
dontChangeUnlessNecessary (ma, mb) = if ma == mb
  then Nothing
  else Just (ma, mb)



instance PotatoHandler SimpleLineHandler where
  pHandlerName _ = handlerName_simpleLine
  pHandleMouse slh@SimpleLineHandler {..} PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) = let

    attachments = getAvailableAttachments True _potatoHandlerInput_pFState _potatoHandlerInput_broadPhase _potatoHandlerInput_screenRegion
    mattachend = fmap fst . isOverAttachment _mouseDrag_to $ attachments

    in case _mouseDrag_state of

      MouseDragState_Down | _simpleLineHandler_isCreation -> Just $ def {
          _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler slh {
              _simpleLineHandler_active = True
              , _simpleLineHandler_isStart = False
              , _simpleLineHandler_attachStart = mattachend
            }
        }
      -- if shift is held down, ignore inputs, this allows us to shift + click to deselect
      -- TODO consider moving this into GoatWidget since it's needed by many manipulators
      MouseDragState_Down | elem KeyModifier_Shift _mouseDrag_modifiers -> Nothing
      MouseDragState_Down -> r where
        (_, ssline) = getSLine _potatoHandlerInput_canvasSelection
        mistart = findFirstLineManipulator _simpleLineHandler_offsetAttach _potatoHandlerInput_pFState rmd _potatoHandlerInput_canvasSelection
        r = case mistart of
          Nothing -> Nothing -- did not click on manipulator, no capture
          Just isstart -> Just $ def {
              _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler slh {
                  _simpleLineHandler_isStart = isstart
                  , _simpleLineHandler_active = True
                  , _simpleLineHandler_original = ssline
                }
            }
      MouseDragState_Dragging -> Just r where
        rid = _superOwl_id $ selectionToSuperOwl _potatoHandlerInput_canvasSelection

        -- line should always have been set in MouseDragState_Down
        ogslinestart = _sSimpleLine_attachStart _simpleLineHandler_original
        ogslineend = _sSimpleLine_attachEnd _simpleLineHandler_original

        -- only attach on non trivial changes so we don't attach to our starting point
        nontrivialline = if _simpleLineHandler_isStart
          then Just _mouseDrag_to /= (getAttachmentPosition _simpleLineHandler_offsetAttach _potatoHandlerInput_pFState <$> ogslineend)
          else Just _mouseDrag_from /= (getAttachmentPosition _simpleLineHandler_offsetAttach _potatoHandlerInput_pFState <$> ogslinestart)
        mattachendnontrivial = if nontrivialline
          then mattachend
          else Nothing

        -- for modifying an existing elt
        modifiedline = if _simpleLineHandler_isStart
          then _simpleLineHandler_original {
              _sSimpleLine_start       = _mouseDrag_to
              , _sSimpleLine_attachStart = mattachendnontrivial
            }
          else _simpleLineHandler_original {
              _sSimpleLine_end       = _mouseDrag_to
              , _sSimpleLine_attachEnd = mattachendnontrivial
            }
        llama = makeSetLlama (rid, SEltLine modifiedline)

        -- for creating new elt
        newEltPos = lastPositionInSelection (_owlPFState_owlTree _potatoHandlerInput_pFState) _potatoHandlerInput_selection
        lineToAdd = SEltLine $ def {
            _sSimpleLine_start = _mouseDrag_from
            , _sSimpleLine_end = _mouseDrag_to
            , _sSimpleLine_superStyle = _potatoDefaultParameters_superStyle _potatoHandlerInput_potatoDefaultParameters
            , _sSimpleLine_lineStyle = _potatoDefaultParameters_lineStyle _potatoHandlerInput_potatoDefaultParameters
            , _sSimpleLine_attachStart = _simpleLineHandler_attachStart
            , _sSimpleLine_attachEnd = mattachendnontrivial
          }

        op = if _simpleLineHandler_isCreation
          then WSEAddElt (_simpleLineHandler_undoFirst, newEltPos, OwlEltSElt (OwlInfo "<line>") $ lineToAdd)
          else WSEApplyLlama (_simpleLineHandler_undoFirst, llama)

        r = def {
            _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler slh {
                _simpleLineHandler_undoFirst = True
                , _simpleLineHandler_attachStart = if _simpleLineHandler_isStart then mattachendnontrivial else _simpleLineHandler_attachStart
                , _simpleLineHandler_attachEnd = if not _simpleLineHandler_isStart then mattachendnontrivial else _simpleLineHandler_attachEnd
              }
            , _potatoHandlerOutput_pFEvent = Just op
          }
      MouseDragState_Up -> Just def
      MouseDragState_Cancelled -> Just def
  pHandleKeyboard _ PotatoHandlerInput {..} kbd = case kbd of
    -- TODO keyboard movement
    _                              -> Nothing
  pRenderHandler SimpleLineHandler {..} PotatoHandlerInput {..} = r where
    mselt = selectionToMaybeSuperOwl _potatoHandlerInput_canvasSelection >>= return . superOwl_toSElt_hack

    boxes = case mselt of
      Just (SEltLine SSimpleLine {..}) -> if _simpleLineHandler_active
        -- TODO if active, color selected handler
        then [make_1area_lBox_from_XY startHandle, make_1area_lBox_from_XY endHandle]
        else [make_1area_lBox_from_XY startHandle, make_1area_lBox_from_XY endHandle]
        where
          startHandle = fromMaybe _sSimpleLine_start (maybeLookupAttachment _sSimpleLine_attachStart _simpleLineHandler_offsetAttach _potatoHandlerInput_pFState)
          endHandle = fromMaybe _sSimpleLine_end (maybeLookupAttachment _sSimpleLine_attachEnd _simpleLineHandler_offsetAttach _potatoHandlerInput_pFState)
      _ -> []

    attachments = getAvailableAttachments True _potatoHandlerInput_pFState _potatoHandlerInput_broadPhase _potatoHandlerInput_screenRegion

    fmapattachmentfn (a,p) = RenderHandle {
        _renderHandle_box = (LBox p 1)
        , _renderHandle_char = Just (attachmentRenderChar a)
        , _renderHandle_color = if matches _simpleLineHandler_attachStart || matches _simpleLineHandler_attachEnd
          then RHC_AttachmentHighlight
          else RHC_Attachment
      } where
        rid = _attachment_target a
        matches ma = fmap (\a' -> _attachment_target a' == rid) ma == Just True
    attachmentBoxes = fmap fmapattachmentfn attachments

    r = HandlerRenderOutput (attachmentBoxes <> fmap defaultRenderHandle boxes)

  pIsHandlerActive = _simpleLineHandler_active
