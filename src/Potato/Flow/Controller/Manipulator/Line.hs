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
import           Potato.Flow.OwlState
import           Potato.Flow.Owl
import           Potato.Flow.Attachments

import           Control.Exception
import Data.Maybe (fromJust)
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


-- TODO rename to AutoLine
data SimpleLineHandler = SimpleLineHandler {
    _simpleLineHandler_isStart      :: Bool -- either we are manipulating start, or we are manipulating end
    , _simpleLineHandler_undoFirst  :: Bool
    , _simpleLineHandler_isCreation :: Bool
    , _simpleLineHandler_active     :: Bool

    , _simpleLineHandler_original :: SSimpleLine -- track original so we can set proper "undo" point with undoFirst operations

    , _simpleLineHandler_offsetAttach :: Bool -- who sets this?

    , _simpleLineHandler_attachStart :: Maybe Attachment
    , _simpleLineHandler_attachEnd :: Maybe Attachment
  } deriving (Show)

-- dummy value to make coding simpler
dummySSimpleLine :: SSimpleLine
dummySSimpleLine = SSimpleLine {
  _sSimpleLine_start       = 0
  , _sSimpleLine_end       = 0
  , _sSimpleLine_style     = def
  , _sSimpleLine_lineStyle = def

  , _sSimpleLine_attachStart = Nothing
  , _sSimpleLine_attachEnd = Nothing
}

instance Default SimpleLineHandler where
  def = SimpleLineHandler {
      _simpleLineHandler_isStart = False
      , _simpleLineHandler_undoFirst = False
      , _simpleLineHandler_isCreation = False
      , _simpleLineHandler_active = False
      , _simpleLineHandler_original = dummySSimpleLine
      , _simpleLineHandler_offsetAttach = True
      , _simpleLineHandler_attachStart = Nothing
      , _simpleLineHandler_attachEnd = Nothing
    }


findFirstLineManipulator :: RelMouseDrag -> CanvasSelection -> Maybe Bool
findFirstLineManipulator (RelMouseDrag MouseDrag {..}) (CanvasSelection selection) = assert (Seq.length selection == 1) $ r where
  msowl = Seq.lookup 0 selection
  selt = case msowl of
    Nothing -> error "expected selection"
    Just sowl -> superOwl_toSElt_hack sowl
  r = case selt of
    SEltLine SSimpleLine {..} ->
      if _mouseDrag_to == _sSimpleLine_start then Just True
        else if _mouseDrag_to == _sSimpleLine_end then Just False
          else Nothing
    x -> error $ "expected SSimpleLine in selection but got " <> show x <> " instead"



dontChangeUnlessNecessary :: Eq a => (Maybe a, Maybe a) -> Maybe (Maybe a, Maybe a)
dontChangeUnlessNecessary (ma, mb) = if ma == mb
  then Nothing
  else Just (ma, mb)


instance PotatoHandler SimpleLineHandler where
  pHandlerName _ = handlerName_simpleLine
  pHandleMouse slh@SimpleLineHandler {..} PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) = case _mouseDrag_state of

    MouseDragState_Down | _simpleLineHandler_isCreation -> Just $ def {
        _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler slh {
            _simpleLineHandler_active = True
            , _simpleLineHandler_attachStart = fmap fst . isOverAttachment _mouseDrag_from $ getAvailableAttachments True _potatoHandlerInput_pFState _potatoHandlerInput_broadPhase _potatoHandlerInput_screenRegion
          }
      }
    -- if shift is held down, ignore inputs, this allows us to shift + click to deselect
    -- TODO consider moving this into GoatWidget since it's needed by many manipulators
    MouseDragState_Down | elem KeyModifier_Shift _mouseDrag_modifiers -> Nothing
    MouseDragState_Down -> r where
      (_, sline) = getSLine _potatoHandlerInput_canvasSelection
      mistart = findFirstLineManipulator rmd _potatoHandlerInput_canvasSelection
      r = case mistart of
        Nothing -> Nothing -- did not click on manipulator, no capture
        Just isstart -> Just $ def {
            _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler slh {
                _simpleLineHandler_isStart = isstart
                , _simpleLineHandler_active = True
                , _simpleLineHandler_original = sline
              }
          }
    MouseDragState_Dragging -> Just r where
      rid = _superOwl_id $ selectionToSuperOwl _potatoHandlerInput_canvasSelection

      -- line should always have been set in MouseDragState_Down
      ogslinestart = _sSimpleLine_attachStart _simpleLineHandler_original
      ogslineend = _sSimpleLine_attachEnd _simpleLineHandler_original

      -- TODO handle shift click using restrict8
      dragDelta = _mouseDrag_to - _mouseDrag_from

      attachments = getAvailableAttachments True _potatoHandlerInput_pFState _potatoHandlerInput_broadPhase _potatoHandlerInput_screenRegion
      nontrivialline = if _simpleLineHandler_isStart
        then Just _mouseDrag_to /= (getAttachmentPosition _simpleLineHandler_offsetAttach _potatoHandlerInput_pFState <$> ogslineend)
        else Just _mouseDrag_from /= (getAttachmentPosition _simpleLineHandler_offsetAttach _potatoHandlerInput_pFState <$> ogslinestart)
      mattachend = if nontrivialline
        then fmap fst . isOverAttachment _mouseDrag_to $ attachments
        else Nothing



      -- for manipulation
      -- TODO connect to attachments here
      controller = CTagLine :=> (Identity $ if _simpleLineHandler_isStart
        then def {
            _cLine_deltaStart = Just $ DeltaXY dragDelta
            , _cLine_deltaAttachStart = if _simpleLineHandler_isStart then dontChangeUnlessNecessary (ogslinestart, mattachend) else Nothing
          }
        else def {
            _cLine_deltaEnd = Just $ DeltaXY dragDelta
            , _cLine_deltaAttachEnd = if _simpleLineHandler_isStart then Nothing else dontChangeUnlessNecessary (ogslineend, mattachend)
          })

      -- for creating new elt
      newEltPos = lastPositionInSelection (_owlPFState_owlTree _potatoHandlerInput_pFState) _potatoHandlerInput_selection
      lineToAdd = SEltLine $ SSimpleLine {
          _sSimpleLine_start = _mouseDrag_from
          , _sSimpleLine_end = _mouseDrag_to
          , _sSimpleLine_style = _potatoDefaultParameters_superStyle _potatoHandlerInput_potatoDefaultParameters
          , _sSimpleLine_lineStyle = _potatoDefaultParameters_lineStyle _potatoHandlerInput_potatoDefaultParameters
          , _sSimpleLine_attachStart = _simpleLineHandler_attachStart
          , _sSimpleLine_attachEnd = Nothing
        }

      op = if _simpleLineHandler_isCreation
        then WSEAddElt (_simpleLineHandler_undoFirst, newEltPos, OwlEltSElt (OwlInfo "<line>") $ lineToAdd)
        else WSEManipulate (_simpleLineHandler_undoFirst, IM.singleton rid controller)

      r = def {
          _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler slh {
              _simpleLineHandler_undoFirst = True
              , _simpleLineHandler_attachStart = if _simpleLineHandler_isStart then mattachend else Nothing
              , _simpleLineHandler_attachEnd = if _simpleLineHandler_isStart then Nothing else mattachend
            }
          , _potatoHandlerOutput_pFEvent = Just op
        }
    MouseDragState_Up -> Just def
    MouseDragState_Cancelled -> Just def
  pHandleKeyboard sh PotatoHandlerInput {..} kbd = case kbd of
    -- TODO keyboard movement
    _                              -> Nothing
  pRenderHandler slh@SimpleLineHandler {..} PotatoHandlerInput {..} = r where
    mselt = selectionToMaybeSuperOwl _potatoHandlerInput_canvasSelection >>= return . superOwl_toSElt_hack
    boxes = case mselt of
      Just (SEltLine SSimpleLine {..}) -> if _simpleLineHandler_active
        -- TODO if active, color selected handler
        then [make_lBox_from_XY _sSimpleLine_start, make_lBox_from_XY _sSimpleLine_end]
        else [make_lBox_from_XY _sSimpleLine_start, make_lBox_from_XY _sSimpleLine_end]
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

    r = HandlerRenderOutput (fmap defaultRenderHandle boxes <> attachmentBoxes)

  pIsHandlerActive = _simpleLineHandler_active
