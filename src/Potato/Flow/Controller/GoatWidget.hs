{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.GoatWidget (
  GoatWidgetConfig(..)
  , emptyGoatWidgetConfig
  , GoatWidget(..)
  , goatState_pFState
  , GoatState(..)
  , holdGoatWidget
) where

import           Relude

import           Reflex
import           Reflex.Potato.Helpers

import           Potato.Flow.BroadPhase
import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.Layers
import           Potato.Flow.Controller.Manipulator.Box
import           Potato.Flow.Controller.Manipulator.Common
import           Potato.Flow.Controller.Manipulator.Layers
import           Potato.Flow.Controller.Manipulator.Line
import           Potato.Flow.Controller.Manipulator.Pan
import           Potato.Flow.Controller.Manipulator.Select
import           Potato.Flow.Controller.Types
import           Potato.Flow.Math
import           Potato.Flow.Render
import           Potato.Flow.SElts
import           Potato.Flow.State
import           Potato.Flow.Types
import           Potato.Flow.Workspace

import           Control.Exception                         (assert)
import           Control.Monad.Fix
import           Data.Aeson
import           Data.Default                              (def)
import qualified Data.IntMap                               as IM
import           Data.Maybe
import qualified Data.Sequence                             as Seq
import           Data.Tuple.Extra


catMaybesSeq :: Seq (Maybe a) -> Seq a
catMaybesSeq = fmap fromJust . Seq.filter isJust


data GoatState = GoatState {

    -- TODO Refactor these out into GoatTab or something like that
    -- TODO consider including handlers (vs regenerating from selection each time you switch tabs)
    _goatState_pFWorkspace       :: PFWorkspace
    , _goatState_layerPosMap     :: LayerPosMap
    , _goatState_pan             :: XY -- panPos is position of upper left corner of canvas relative to screen
    , _goatState_selection       :: Selection
    , _goatState_broadPhaseState :: BroadPhaseState
    , _goatState_layersState     :: LayersState
    , _goatState_renderedCanvas  :: RenderedCanvas

    , _goatState_selectedTool    :: Tool
    , _goatState_mouseDrag       :: MouseDrag -- last mouse dragging state, this is a little questionable, arguably we should only store stuff needed, not the entire mouseDrag
    , _goatState_handler         :: SomePotatoHandler
    , _goatState_layersHandler   :: SomePotatoHandler
    , _goatState_clipboard       :: Maybe SEltTree
    , _goatState_debugLabel      :: Text

  } deriving (Show)

goatState_pFState :: GoatState -> PFState
goatState_pFState = _pFWorkspace_pFState . _goatState_pFWorkspace

data GoatCmd =
  GoatCmdTool Tool
  | GoatCmdLoad EverythingLoadState
  | GoatCmdWSEvent WSEvent

  -- canvas direct input
  | GoatCmdMouse LMouseData
  | GoatCmdKeyboard KeyboardData

  -- debug nonsense
  | GoatCmdSetDebugLabel Text
  deriving (Show)

-- | invariants
-- * TODO mouse input type can only change after a `_lMouseData_isRelease == True`
-- * TODO non-mouse inputs can only happen after a `_lMouseData_isRelease == True` except for cancel
data GoatWidgetConfig t = GoatWidgetConfig {

  -- TODO should really also include ControllerMeta
  _goatWidgetConfig_initialState    :: PFState

  -- canvas direct input
  , _goatWidgetConfig_mouse         :: Event t LMouseData
  , _goatWidgetConfig_keyboard      :: Event t KeyboardData

  -- command based
  , _goatWidgetConfig_selectTool    :: Event t Tool
  , _goatWidgetConfig_load          :: Event t EverythingLoadState

  -- TODO someday add this to support multi-user mode :O
  -- the only thing tricky about this is that this may invalidate active handlers and that needs to be accounted for (just check if active REltId shows up in changes)
  --, _goatWidgetConfig_externalWSEvent :: Event t WSEvent

  -- only intended for setting params
  , _goatWidgetConfig_paramsEvent   :: Event t ControllersWithId

  -- debugging
  , _goatWidgetConfig_setDebugLabel :: Event t Text
}

emptyGoatWidgetConfig :: (Reflex t) => GoatWidgetConfig t
emptyGoatWidgetConfig = GoatWidgetConfig {
    _goatWidgetConfig_initialState = emptyPFState
    , _goatWidgetConfig_selectTool  = never
    , _goatWidgetConfig_load = never
    , _goatWidgetConfig_mouse     = never
    , _goatWidgetConfig_keyboard = never
    , _goatWidgetConfig_paramsEvent = never
    , _goatWidgetConfig_setDebugLabel = never
  }

data GoatWidget t = GoatWidget {
  _goatWidget_tool                  :: Dynamic t Tool
  , _goatWidget_selection           :: Dynamic t Selection
  , _goatWidget_layers              :: Dynamic t LayerEntries
  , _goatWidget_pan                 :: Dynamic t XY
  , _goatWidget_broadPhase          :: Dynamic t BroadPhaseState
  -- TODO render here?
  , _goatWidget_handlerRenderOutput :: Dynamic t HandlerRenderOutput
  , _goatWidget_canvas              :: Dynamic t SCanvas
  , _goatWidget_renderedCanvas      :: Dynamic t RenderedCanvas

  -- debug stuff prob
  , _goatWidget_DEBUG_goatState     :: Dynamic t GoatState
}

-- TODO rename to makeHandlerFromCanvasSelection
makeHandlerFromSelection :: Selection -> SomePotatoHandler
makeHandlerFromSelection selection = case computeSelectionType selection of
  SMTBox         -> SomePotatoHandler $ (def :: BoxHandler)
  SMTBoxText     -> SomePotatoHandler $ (def :: BoxHandler)
  SMTLine        -> SomePotatoHandler $ (def :: SimpleLineHandler)
  SMTText        -> SomePotatoHandler EmptyHandler -- TODO
  SMTBoundingBox -> SomePotatoHandler $ (def :: BoxHandler)
  SMTNone        -> SomePotatoHandler EmptyHandler

maybeUpdateHandlerFromSelection :: SomePotatoHandler -> Selection -> SomePotatoHandler
maybeUpdateHandlerFromSelection sph selection = case sph of
  SomePotatoHandler h -> if pIsHandlerActive h
    then sph
    else makeHandlerFromSelection selection

-- TODO fix me
-- LayerMetaMap isn't enough to figure out if elt is hidden/locked
-- you need to check its parents as well... and we don't have an easy way to get to parents atm
makeCanvasSelectionFromSelection :: PFState -> LayerMetaMap -> Selection -> Selection
makeCanvasSelectionFromSelection PFState {..} lmm selection = flip Seq.filter selection $ \(rid,_,seltl) -> case seltl of
  SEltLabel _ SEltFolderStart -> False
  SEltLabel _ SEltFolderEnd -> False
  _ -> case IM.lookup rid lmm of
    Nothing             -> True
    Just LayerMeta {..} -> True


makeClipboard :: GoatState -> Maybe SEltTree
makeClipboard GoatState {..} = r where
  -- TODO if there are connections that aren't included in selection, convert SElt
  selectionToSEltTree :: Selection -> SEltTree
  selectionToSEltTree selection = assert (validateSelection selection)
    $ fmap (\(rid,_,seltl) -> (rid, seltl)) (toList selection)

  r = if Seq.null _goatState_selection
    then _goatState_clipboard
    else Just $ selectionToSEltTree _goatState_selection

potatoHandlerOutputNoHandlerChange :: GoatState -> PotatoHandlerOutput
potatoHandlerOutputNoHandlerChange GoatState {..} = def { _potatoHandlerOutput_nextHandler = Just _goatState_handler }

potatoHandlerOutputDeleteSelection :: GoatState -> PotatoHandlerOutput
potatoHandlerOutputDeleteSelection goatState@GoatState {..} = r where
  deleteEv = if Seq.null _goatState_selection
    then Nothing
    else Just $ WSERemoveElt (toList . fmap snd3 $ _goatState_selection)
  r = (potatoHandlerOutputNoHandlerChange goatState) {
      _potatoHandlerOutput_pFEvent = deleteEv
    }

potatoHandlerInputFromGoatState :: GoatState -> PotatoHandlerInput
potatoHandlerInputFromGoatState GoatState {..} = r where
  last_workspace = _goatState_pFWorkspace
  last_pFState = _pFWorkspace_pFState last_workspace
  r = PotatoHandlerInput {
    _potatoHandlerInput_pFState       = last_pFState
    , _potatoHandlerInput_broadPhase  = _goatState_broadPhaseState
    , _potatoHandlerInput_layerPosMap = _goatState_layerPosMap
    , _potatoHandlerInput_tool = _goatState_selectedTool

    , _potatoHandlerInput_layersState     = _goatState_layersState
    , _potatoHandlerInput_selection   = _goatState_selection
  }

processLayersHandlerOutput :: GoatState -> PotatoHandlerOutput -> (GoatState, PotatoHandlerOutput)
processLayersHandlerOutput goatState pho = (goatState', pho') where
  -- TODO this is wrong, you want to persist the current handler
  -- interacting with layers should not change canvas handler
  pho' = pho { _potatoHandlerOutput_nextHandler = Nothing }
  goatState' = goatState {
      _goatState_layersHandler = case _potatoHandlerOutput_nextHandler pho of
        Just h  -> h
        Nothing -> error "expected LayersHandler to return a new handler"
    }

-- TODO extract this method into another file
-- TODO make State monad for this
foldGoatFn :: GoatCmd -> GoatState -> GoatState
foldGoatFn cmd goatState@GoatState {..} = finalGoatState where
  last_workspace = _goatState_pFWorkspace
  last_pFState = _pFWorkspace_pFState last_workspace

  potatoHandlerInput = potatoHandlerInputFromGoatState goatState

  -- persist the handler if it's not a canvas input
  -- we need to this because of our incorrect use of PotatoHandlerOutput as the intermediary return type of the proceeding case statement
  -- i.e. the values of PotatoHandlerOutput are interpreted slightly differently here than intended
  -- intended interpretation: isNothing _potatoHandlerOutput_nextHandler -> handler does not capture input
  -- interpretation here when non-canvas input: isNothing _potatoHandlerOutput_nextHandler -> event not related to canvas handler
  -- this really sucks, maybe you should fix it D:
  -- NOTE for now, this type of input is not allowed when handler is active (may want to change this in the future?)
  persistHandlerNoCanvasInput = assert (not $ pIsHandlerActive _goatState_handler ) def { _potatoHandlerOutput_nextHandler = Just _goatState_handler }

  (goatStateAfterGoatCmd, phoAfterGoatCmd) = case _goatState_handler of
    SomePotatoHandler handler -> case cmd of
      --GoatCmdSetDebugLabel x -> traceShow x $ (goatState { _goatState_debugLabel = x }, persistHandlerNoCanvasInput)
      GoatCmdSetDebugLabel x -> (goatState { _goatState_debugLabel = x }, persistHandlerNoCanvasInput)
      GoatCmdTool x -> (goatState { _goatState_selectedTool = x }, persistHandlerNoCanvasInput)
      GoatCmdWSEvent x -> (goatState, persistHandlerNoCanvasInput { _potatoHandlerOutput_pFEvent = Just x })
      GoatCmdLoad (spf, cm) ->
        -- TODO load ControllerMeta stuff
        (goatState, def {
            _potatoHandlerOutput_pFEvent = Just $ WSELoad spf
          })
      --GoatCmdMouse mouseData -> traceShow _goatState_handler $ do
      GoatCmdMouse mouseData ->
        let
          sameSource = _mouseDrag_isLayerMouse _goatState_mouseDrag == _lMouseData_isLayerMouse mouseData
          mouseDrag = case _mouseDrag_state _goatState_mouseDrag of
            MouseDragState_Up        -> newDrag mouseData
            MouseDragState_Cancelled -> assert sameSource $ (continueDrag mouseData _goatState_mouseDrag) { _mouseDrag_state = MouseDragState_Cancelled }
            _                        -> assert sameSource $ continueDrag mouseData _goatState_mouseDrag

          canvasDrag = toRelMouseDrag last_pFState _goatState_pan mouseDrag
          goatState' = goatState { _goatState_mouseDrag = mouseDrag }

          isLayerMouse = _mouseDrag_isLayerMouse mouseDrag

        in case _mouseDrag_state mouseDrag of
          -- if mouse was cancelled, update _goatState_mouseDrag accordingly
          MouseDragState_Cancelled -> if _lMouseData_isRelease mouseData
            then (goatState' { _goatState_mouseDrag = def }, persistHandlerNoCanvasInput)
            else (goatState', persistHandlerNoCanvasInput) -- still cancelled

          -- if mouse is intended for layers
          _ | isLayerMouse -> case pHandleMouse _goatState_layersHandler potatoHandlerInput (RelMouseDrag mouseDrag) of
            Just pho -> processLayersHandlerOutput goatState' pho
            Nothing  -> (goatState', persistHandlerNoCanvasInput)

          -- special case, if mouse down and pan tool, we override with a new handler
          MouseDragState_Down | _goatState_selectedTool == Tool_Pan -> case pHandleMouse (def :: PanHandler) potatoHandlerInput canvasDrag of
            Just pho -> (goatState', pho)
            Nothing  -> error "this should never happen"
          -- special case, if mouse down and creation tool, we override with a new handler
          MouseDragState_Down | tool_isCreate _goatState_selectedTool -> assert (not $ pIsHandlerActive handler) r where
            someNewHandler = case _goatState_selectedTool of
              Tool_Box    -> SomePotatoHandler $ def { _boxHandler_creation = BoxCreationType_Box }
              Tool_Line   -> SomePotatoHandler $ def { _simpleLineHandler_isCreation = True }
              Tool_Select -> SomePotatoHandler $ (def :: SelectHandler)
              Tool_Text   -> SomePotatoHandler $ def { _boxHandler_creation = BoxCreationType_Text }
              _           -> error "not valid tool"

            -- pass input onto newly created handler
            r = case someNewHandler of
              SomePotatoHandler newHandler -> case pHandleMouse newHandler potatoHandlerInput canvasDrag of
                Just pho -> (goatState', pho)
                Nothing -> error "this should never happen, although if it did, we have many choices to gracefully recover (and I couldn't pick which one so I just did the error thing instead)"
          -- _ -> trace "handler mouse case:\nmouse: " $ traceShow mouseDrag $ trace "prev goatState:" $ traceShow goatState $ trace "handler" $ traceShow _goatState_handler $ case pHandleMouse handler potatoHandlerInput canvasDrag of
          _ -> case pHandleMouse handler potatoHandlerInput canvasDrag of
            Just pho -> (goatState', pho)
            -- input not captured by handler, do select or drag+select
            Nothing | _mouseDrag_state mouseDrag == MouseDragState_Down -> assert (not $ pIsHandlerActive handler) r where
              nextSelection = selectMagic last_pFState _goatState_layerPosMap _goatState_broadPhaseState canvasDrag
              shiftClick = isJust $ find (==KeyModifier_Shift) (_mouseDrag_modifiers mouseDrag)
              r = if Seq.null nextSelection || shiftClick
                -- clicked on nothing or shift click, start SelectHandler
                -- NOTE this is weird because:
                -- 1. doesn't select until mouse up, which is regression from when you do regular click on an elt (via BoxHandler which immediately selects)
                -- 2. if you let go of shift before mouse up, it just does a standard mouse selection
                -- I think this is still better than letting BoxHandler handle this case
                then case pHandleMouse (def :: SelectHandler) potatoHandlerInput canvasDrag of
                  Just pho -> (goatState', pho)
                  Nothing -> error "handler was expected to capture this mouse state"

                -- special drag + select case, override the selection
                -- alternative, we could let the BoxHandler do this but that would mean we query broadphase twice
                -- (once to determine that we should create the BoxHandler, and again to set the selection in BoxHandler)
                -- also NOTE BoxHandler here is used to move all SElt types, upon release, it will either return the correct handler type or not capture the input in which case GoatWidget will set the correct handler type
                else case pHandleMouse (def { _boxHandler_creation = BoxCreationType_DragSelect }) (potatoHandlerInput { _potatoHandlerInput_selection = nextSelection }) canvasDrag of
                  -- it's a little weird because we are forcing the selection from outside the handler and ignoring the new selection results returned by pho (which should always be Nothing)
                  Just pho -> assert (isNothing . _potatoHandlerOutput_select $ pho)
                    $ (goatState', pho { _potatoHandlerOutput_select = Just (False, nextSelection) })
                  Nothing -> error "handler was expected to capture this mouse state"

            Nothing -> error $ "handler " <> show (pHandlerName handler) <> "was expected to capture mouse state " <> show (_mouseDrag_state mouseDrag)

      GoatCmdKeyboard kbd -> case kbd of
        -- special case, treat escape cancel mouse drag as a mouse input
        KeyboardData KeyboardKey_Esc _ | mouseDrag_isActive _goatState_mouseDrag -> r where
          canceledMouse = cancelDrag _goatState_mouseDrag
          rGoatState = goatState {
              _goatState_mouseDrag = canceledMouse
            }
          r = if _mouseDrag_isLayerMouse _goatState_mouseDrag
            then case pHandleMouse _goatState_layersHandler potatoHandlerInput (RelMouseDrag canceledMouse) of
              Just pho -> processLayersHandlerOutput rGoatState pho
              Nothing  -> (rGoatState, def)
            else case pHandleMouse handler potatoHandlerInput (toRelMouseDrag last_pFState _goatState_pan canceledMouse) of
              Just pho -> (rGoatState, pho)
              Nothing  -> (rGoatState, def)

        -- we are in the middle of mouse drag, ignore all keyboard inputs
        -- perhaps a better way to do this is to have handlers capture all inputs when active
        _ | mouseDrag_isActive _goatState_mouseDrag -> (goatState, captureWithNoChange handler)

        -- TODO pass input to LayersHandler
        -- a reasonable way to do this is to always pass it to LayersHandler first and then to normal handler
        -- LayersHandler should only capture when donig renaming
        _ -> case pHandleKeyboard handler potatoHandlerInput kbd of
          Just pho -> (goatState, pho)
          -- input not captured by handler
          -- TODO consider wrapping this all up in KeyboardHandler or something? Unfortunately, copy needs to modify goatState in a what that PotatoHandlerOutput can't atm
          Nothing -> case kbd of
            KeyboardData KeyboardKey_Esc _ ->
              (goatState, def {
                -- TODO change tool back to select?
                  -- deselect goatState if we weren't using mouse
                  -- TODO actually probably don't bother checking for this condition, mouse should have been captured in this case
                  _potatoHandlerOutput_select = case _mouseDrag_state _goatState_mouseDrag of
                    MouseDragState_Up        -> Just (False, Seq.empty)
                    MouseDragState_Cancelled -> Just (False, Seq.empty)
                    _                        -> Nothing
                })

            KeyboardData (KeyboardKey_Delete) [] -> r where
              r = (goatState, potatoHandlerOutputDeleteSelection goatState)
            KeyboardData (KeyboardKey_Char 'c') [KeyModifier_Ctrl] -> r where
              copied = makeClipboard goatState
              r = (goatState { _goatState_clipboard = copied }, def)
            KeyboardData (KeyboardKey_Char 'x') [KeyModifier_Ctrl] -> r where
              copied = makeClipboard goatState
              r = (goatState { _goatState_clipboard = copied }, potatoHandlerOutputDeleteSelection goatState)
            KeyboardData (KeyboardKey_Char 'v') [KeyModifier_Ctrl] -> r where
              pastaEv = case _goatState_clipboard of
                Nothing    -> Nothing
                Just stree -> Just $ WSEAddRelative (lastPositionInSelection _goatState_selection, stree)
              r = (goatState, (potatoHandlerOutputNoHandlerChange goatState) { _potatoHandlerOutput_pFEvent = pastaEv })
            -- tool hotkeys
            KeyboardData (KeyboardKey_Char key) _ -> r where
              newTool = case key of
                'v'  -> Tool_Select
                -- 'p' -> Tool_Pan
                'b'  -> Tool_Box
                '\\' -> Tool_Line
                't'  -> Tool_Text
                _    -> _goatState_selectedTool
              r = (goatState { _goatState_selectedTool = newTool }, def)

            -- TODO copy pasta, or maybe copy pasta lives outside of GoatWidget?

            -- unhandled input
            _ -> (goatState, persistHandlerNoCanvasInput)
      _          -> undefined


  -- update PFWorkspace from pho
  (next_workspace, cslmapForBroadPhase) = case _potatoHandlerOutput_pFEvent phoAfterGoatCmd of
    -- if there was no update, then changes are not valid
    Nothing   -> (_goatState_pFWorkspace, IM.empty)
    Just wsev -> (r1,r2) where
      r1 = updatePFWorkspace wsev _goatState_pFWorkspace
      r2 = _pFWorkspace_lastChanges r1
  next_pFState = _pFWorkspace_pFState next_workspace
  next_layerPosMap = pFState_getLayerPosMap next_pFState
  cslmap = IM.mapWithKey (\rid v -> fmap (\seltl -> (next_layerPosMap IM.! rid, seltl)) v) cslmapForBroadPhase

  -- update pan from pho
  next_pan = case _potatoHandlerOutput_pan phoAfterGoatCmd of
    Nothing -> _goatState_pan
    Just (V2 dx dy) -> V2 (cx0+dx) (cy0 + dy) where
      V2 cx0 cy0 = _goatState_pan

  -- update layersState from pho
  next_layersState' = case _potatoHandlerOutput_layersState phoAfterGoatCmd of
    Nothing -> _goatState_layersState
    Just ls -> ls

  -- get selection from pho
  mSelectionFromPho = case _potatoHandlerOutput_select phoAfterGoatCmd of
    Nothing -> Nothing
    Just (add, sel) -> assert (pFState_selectionIsValid next_pFState (fmap snd3 (toList r))) $ Just r where
      r' = if add
        then disjointUnionSelection _goatState_selection sel
        else sel
      sortfn (_,lp1,_) (_,lp2,_) = compare lp1 lp2
      r = Seq.sortBy sortfn r'

  -- get selection from changes
  newEltFoldMapFn rid v = case v of
    Nothing     -> []
    Just (lp,seltl) -> if IM.member rid (_pFState_directory last_pFState) then [] else [(rid,lp,seltl)]

  -- TODO don't select changes on load
  wasLoad = False
  -- update selection based on changes from updating PFState
  (isNewSelection', selectionAfterChanges) = if IM.null cslmap
    then (False, _goatState_selection)
    else r where
      newlyCreatedSEltls = IM.foldMapWithKey newEltFoldMapFn cslmap
      r = if wasLoad || null newlyCreatedSEltls
        -- if there are no newly created elts, we still need to update the selection
        then (\x -> (False, x)) $ catMaybesSeq . flip fmap _goatState_selection $ \sseltl@(rid,_,_) ->
          case IM.lookup rid cslmap of
            -- no changes means not deleted
            Nothing                  -> Just sseltl
            -- if deleted, remove it
            Just Nothing             -> Nothing
            -- it was changed, update selection to newest version
            Just (Just (lp, seltl')) -> Just (rid, lp, seltl')
        else (True, Seq.fromList newlyCreatedSEltls)

  (isNewSelection, next_selection) = case mSelectionFromPho of
    Just x  -> assert (not isNewSelection') (True, x)
    -- better/more expensive check to ensure mSelectionFromPho stuff is mutually exclusive to selectionAfterChanges
    --Just x -> assert (selectionAfterChanges == _goatState_selection) (True, x)
    Nothing -> (isNewSelection', selectionAfterChanges)

  mHandlerFromPho = _potatoHandlerOutput_nextHandler phoAfterGoatCmd

  next_handler = if isNewSelection
    -- if there is a new selection, update the handler with new selection if handler wasn't active
    then maybeUpdateHandlerFromSelection (fromMaybe (SomePotatoHandler EmptyHandler) mHandlerFromPho) next_selection
    -- otherwise, use the returned handler or make a new one from selection
    else fromMaybe (makeHandlerFromSelection next_selection) mHandlerFromPho

  next_broadPhaseState = update_bPTree cslmapForBroadPhase (_broadPhaseState_bPTree _goatState_broadPhaseState)
  next_layersState = updateLayers next_pFState cslmapForBroadPhase next_layersState'

  -- TODO render the scene if the screen moved
  --moveRenderedCanvas :: BPTree -> REltIdMap SEltLabel -> LBox -> RenderedCanvas -> RenderedCanvas
  next_renderedCanvas' = _goatState_renderedCanvas


  -- render the scene if there were changes
  cslmapFromHide = IM.empty -- TODO
  cslmapForRendering = cslmapForBroadPhase `IM.union` cslmapFromHide
  next_renderedCanvas = if IM.null cslmapForRendering
    then next_renderedCanvas'
    else updateCanvas cslmapForRendering next_broadPhaseState next_pFState next_layerPosMap next_renderedCanvas'



  finalGoatState = goatStateAfterGoatCmd {
      _goatState_pFWorkspace      = next_workspace
      , _goatState_pan             = next_pan
      , _goatState_handler         = next_handler
      , _goatState_selection       = next_selection
      , _goatState_broadPhaseState = next_broadPhaseState
      , _goatState_renderedCanvas = next_renderedCanvas
      , _goatState_layersState     = next_layersState
      , _goatState_layerPosMap = next_layerPosMap
    }

holdGoatWidget :: forall t m. (Adjustable t m, MonadHold t m, MonadFix m)
  => GoatWidgetConfig t
  -> m (GoatWidget t)
holdGoatWidget GoatWidgetConfig {..} = mdo

  let
    --goatEvent = traceEvent "input: " $ leftmostWarn "GoatWidgetConfig_EverythingFrontend"
    goatEvent = leftmostWarn "GoatWidgetConfig_EverythingFrontend"
      [ GoatCmdTool <$> _goatWidgetConfig_selectTool
      , GoatCmdLoad <$> _goatWidgetConfig_load
      , GoatCmdMouse <$> _goatWidgetConfig_mouse
      , GoatCmdKeyboard <$> _goatWidgetConfig_keyboard
      , GoatCmdSetDebugLabel <$> _goatWidgetConfig_setDebugLabel
      , ffor _goatWidgetConfig_paramsEvent $ \cwid -> assert (controllerWithId_isParams cwid) (GoatCmdWSEvent (WSEManipulate (False, cwid)))
      ]

    --initialize broadphase with initial state
    initialbp = update_bPTree (fmap Just (_pFState_directory _goatWidgetConfig_initialState)) emptyBPTree
    initiallayersstate = makeLayersStateFromPFState _goatWidgetConfig_initialState

    -- TODO wrap this in a helper function in Render
    -- TODO we want to render the whole screen, not just the canvas
    initialCanvasBox = _sCanvas_box $ _pFState_canvas _goatWidgetConfig_initialState
    initialselts = fmap (\(SEltLabel _ selt) -> selt) $ toList $ _pFState_directory _goatWidgetConfig_initialState
    initialrc = render initialCanvasBox initialselts (emptyRenderedCanvas initialCanvasBox)

    initialgoat = GoatState {
        _goatState_pFWorkspace      = loadPFStateIntoWorkspace _goatWidgetConfig_initialState emptyWorkspace
        , _goatState_selectedTool    = Tool_Select
        , _goatState_pan             = 0
        , _goatState_mouseDrag       = def
        , _goatState_handler         = SomePotatoHandler EmptyHandler
        , _goatState_layersHandler   = SomePotatoHandler (def :: LayersHandler)
        , _goatState_debugLabel      = ""
        , _goatState_selection       = Seq.empty
        , _goatState_broadPhaseState = initialbp
        , _goatState_renderedCanvas = initialrc
        , _goatState_layersState     = initiallayersstate
        , _goatState_layerPosMap = pFState_getLayerPosMap _goatWidgetConfig_initialState
        , _goatState_clipboard = Nothing
      }

  goatDyn :: Dynamic t GoatState
    <- foldDyn foldGoatFn initialgoat goatEvent

  -- TODO make sure holdUniqDyn actually does what you think it does
  -- I think it does, but it will prob still do full equality check after changes in goatDyn :(
  -- TODO maybe you need to have special signals to control firing of each sub event instead
  -- I guess the good news is that you can still do this without changing the interface
  -- i.e. PFStateChangeFlag and have each PFState operation return a change flag as well
  r_tool <- holdUniqDyn $ fmap _goatState_selectedTool goatDyn
  r_selection <- holdUniqDyn $ fmap _goatState_selection goatDyn
  r_broadphase <- holdUniqDyn $ fmap _goatState_broadPhaseState goatDyn
  r_pan <- holdUniqDyn $ fmap _goatState_pan goatDyn
  r_layers <- holdUniqDyn $ fmap (snd . _goatState_layersState) goatDyn
  -- TODO flip order of render and holdUniqDyn
  r_handlerRenderOutput <- holdUniqDyn $ fmap (\gs -> pRenderHandler (_goatState_handler gs) (potatoHandlerInputFromGoatState gs)) goatDyn
  r_canvas <- holdUniqDyn $ fmap (_pFState_canvas . _pFWorkspace_pFState . _goatState_pFWorkspace) goatDyn
  let
    r_renderedCanvas = fmap _goatState_renderedCanvas goatDyn

  return GoatWidget
    {
      _goatWidget_tool           = r_tool
      , _goatWidget_selection    = r_selection
      , _goatWidget_layers       = r_layers
      , _goatWidget_pan          = r_pan
      , _goatWidget_broadPhase   = r_broadphase
      , _goatWidget_canvas = r_canvas
      , _goatWidget_renderedCanvas = r_renderedCanvas
      , _goatWidget_handlerRenderOutput =  r_handlerRenderOutput
      , _goatWidget_DEBUG_goatState = goatDyn
    }
