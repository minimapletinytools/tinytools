{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.GoatWidget (
  ControllerMeta(..)
  , emptyControllerMeta
  , GoatWidgetConfig(..)
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
import           Potato.Flow.Entry
import           Potato.Flow.Math
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



-- TODO move this somewhere else
-- TODO this is a problem because LayerMetaMap
-- I guess you can just reindex LayerMetaMap to index via LayerPos (which should be the same as REltId after loading I think)
-- Alternatively, you could just have SPotatoFlow include REltId, that might be slightly better solution...
data ControllerMeta = ControllerMeta {
  _controllerMeta_pan              :: XY
  , _controllerMeta_layerScrollPos :: Int
  , _controllerMeta_layers         :: LayerMetaMap
} deriving (Show, Eq, Generic)

instance FromJSON ControllerMeta
instance ToJSON ControllerMeta

emptyControllerMeta :: ControllerMeta
emptyControllerMeta = ControllerMeta 0 0 IM.empty

catMaybesSeq :: Seq (Maybe a) -> Seq a
catMaybesSeq = fmap fromJust . Seq.filter isJust

type EverythingLoadState = (SPotatoFlow, ControllerMeta)

data GoatState = GoatState {
    _goatState_pFWorkspace       :: PFWorkspace
    , _goatState_layerPosMap     :: REltIdMap LayerPos
    , _goatState_selectedTool    :: Tool
    , _goatState_pan             :: XY -- panPos is position of upper left corner of canvas relative to screen
    , _goatState_mouseDrag       :: MouseDrag -- last mouse dragging state, this is a little questionable, arguably we should only store stuff needed, not the entire mouseDrag
    , _goatState_handler         :: SomePotatoHandler
    , _goatState_layersHandler   :: SomePotatoHandler
    , _goatState_layerScrollPos  :: Int
    , _goatState_debugLabel      :: Text
    , _goatState_selection       :: Selection
    , _goatState_broadPhaseState :: BroadPhaseState
    , _goatState_layersState     :: LayersState
  } deriving (Show)

goatState_pFState :: GoatState -> PFState
goatState_pFState = _pFWorkspace_state . _goatState_pFWorkspace

data GoatCmd =
  GoatCmdTool Tool

  | GoatCmdLoad EverythingLoadState

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
    , _goatWidgetConfig_setDebugLabel = never
  }

data GoatWidget t = GoatWidget {
  _goatWidget_tool                  :: Dynamic t Tool
  , _goatWidget_selection           :: Dynamic t Selection
  , _goatWidget_layers              :: Dynamic t LayerEntries
  , _goatWidget_pan                 :: Dynamic t XY
  , _goatWidget_broadPhase          :: Dynamic t BroadPhaseState
  , _goatWidget_handlerRenderOutput :: Dynamic t HandlerRenderOutput

  -- debug stuff prob
  , _goatWidget_DEBUG_goatState     :: Dynamic t GoatState
}

makeHandlerFromSelection :: Selection -> SomePotatoHandler
makeHandlerFromSelection selection = case computeSelectionType selection of
  SMTBox         -> SomePotatoHandler $ (def :: BoxHandler)
  SMTLine        -> SomePotatoHandler $ (def :: SimpleLineHandler)
  SMTText        -> SomePotatoHandler $ (def { _boxHandler_isText = True }) -- pretty sure this is OK?
  SMTBoundingBox -> SomePotatoHandler $ (def :: BoxHandler)
  SMTNone        -> SomePotatoHandler EmptyHandler

maybeUpdateHandlerFromSelection :: SomePotatoHandler -> Selection -> SomePotatoHandler
maybeUpdateHandlerFromSelection sph selection = case sph of
  SomePotatoHandler h -> if pIsHandlerActive h
    then sph
    else makeHandlerFromSelection selection

potatoHandlerInputFromGoatState :: GoatState -> PotatoHandlerInput
potatoHandlerInputFromGoatState GoatState {..} = r where
  last_workspace = _goatState_pFWorkspace
  last_pFState = _pFWorkspace_state last_workspace
  r = PotatoHandlerInput {
    _potatoHandlerInput_pFState       = last_pFState
    , _potatoHandlerInput_broadPhase  = _goatState_broadPhaseState
    , _potatoHandlerInput_layerPosMap = _goatState_layerPosMap
    , _potatoHandlerInput_tool = _goatState_selectedTool

    , _potatoHandlerInput_layerScrollPos = _goatState_layerScrollPos
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

foldGoatFn :: (Reflex t) => GoatCmd -> GoatState -> PushM t GoatState
foldGoatFn cmd goatState@GoatState {..} = do
  let

    last_workspace = _goatState_pFWorkspace
    last_pFState = _pFWorkspace_state last_workspace

    potatoHandlerInput = potatoHandlerInputFromGoatState goatState

    (goatStateAfterGoatCmd, phoAfterGoatCmd) = case _goatState_handler of
      SomePotatoHandler handler -> case cmd of
        GoatCmdSetDebugLabel x -> (goatState { _goatState_debugLabel = x }, def)
        GoatCmdTool x -> (goatState { _goatState_selectedTool = x }, def)
        GoatCmdLoad (spf, cm) ->
          -- TODO load ControllerMeta stuff
          (goatState, def {
              _potatoHandlerOutput_pFEvent = Just $ PFELoad spf
            })
        --GoatCmdMouse mouseData -> traceShow _goatState_handler $ do
        GoatCmdMouse mouseData ->
          let
            sameSource = _mouseDrag_isLayerMouse _goatState_mouseDrag == _lMouseData_isLayerMouse mouseData
            mouseDrag = case _mouseDrag_state _goatState_mouseDrag of
              MouseDragState_Up        -> newDrag mouseData
              MouseDragState_Cancelled -> assert sameSource $ (continueDrag mouseData _goatState_mouseDrag) { _mouseDrag_state = MouseDragState_Cancelled }
              _                        -> assert sameSource $ continueDrag mouseData _goatState_mouseDrag

            canvasDrag = toRelMouseDrag last_pFState mouseDrag
            goatState' = goatState { _goatState_mouseDrag = mouseDrag }

            isLayerMouse = _mouseDrag_isLayerMouse mouseDrag

          in case _mouseDrag_state mouseDrag of
            -- if mouse was cancelled, update _goatState_mouseDrag accordingly
            MouseDragState_Cancelled -> if _lMouseData_isRelease mouseData
              then (goatState' { _goatState_mouseDrag = def }, def)
              else (goatState', def) -- still cancelled

            -- if mouse is intended for layers
            _ | isLayerMouse -> case pHandleMouse _goatState_layersHandler potatoHandlerInput (RelMouseDrag mouseDrag) of
              Just pho -> processLayersHandlerOutput goatState' pho
              Nothing  -> (goatState', def)

            -- special case, if mouse down and pan tool, we override with a new handler
            MouseDragState_Down | _goatState_selectedTool == Tool_Pan -> case pHandleMouse (def :: PanHandler) potatoHandlerInput canvasDrag of
              Just pho -> (goatState', pho)
              Nothing  -> error "this should never happen"
            -- special case, if mouse down and creation tool, we override with a new handler
            MouseDragState_Down | tool_isCreate _goatState_selectedTool -> assert (not $ pIsHandlerActive handler) r where
              someNewHandler = case _goatState_selectedTool of
                Tool_Box    -> SomePotatoHandler $ def { _boxHandler_isCreation = True }
                Tool_Line   -> SomePotatoHandler $ def { _simpleLineHandler_isCreation = True }
                Tool_Select -> SomePotatoHandler $ (def :: SelectHandler)
                Tool_Text   -> SomePotatoHandler $ def { _boxHandler_isCreation = True, _boxHandler_isText = True }
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
                  else case pHandleMouse (def :: BoxHandler) (potatoHandlerInput { _potatoHandlerInput_selection = nextSelection }) canvasDrag of
                    -- it's a little weird because we are forcing the selection from outside the handler and ignoring the new selection results returned by pho (which should always be Nothing)
                    Just pho -> assert (isNothing . _potatoHandlerOutput_select $ pho) $ (goatState', pho { _potatoHandlerOutput_select = Just (False, nextSelection) })
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
              else case pHandleMouse handler potatoHandlerInput (toRelMouseDrag last_pFState canceledMouse) of
                Just pho -> (rGoatState, pho)
                Nothing  -> (rGoatState, def)

          -- TODO pass input to LayersHandler
          -- a reasonable way to do this is to always pass it to LayersHandler first and then to normal handler
          -- LayersHandler should only capture when donig renaming
          _ -> case pHandleKeyboard handler potatoHandlerInput kbd of
            Just pho -> (goatState, pho)
            -- input not captured by handler
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

              KeyboardData (KeyboardKey_Char 'c') [KeyModifier_Ctrl] ->
                -- TODO copy
                undefined
              KeyboardData (KeyboardKey_Char 'v') [KeyModifier_Ctrl] ->
                -- TODO pasta
                undefined
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
              _ -> (goatState, def)
        _          -> undefined


    -- update PFTotalState from pho
    (next_workspace, cslmapForBroadPhase) = case _potatoHandlerOutput_pFEvent phoAfterGoatCmd of
      -- if there was no update, then changes are not valid
      Nothing   -> (_goatState_pFWorkspace, IM.empty)
      Just pfev -> assert (pasta == []) (r1,r2) where
        -- TODO extract updatePFTotalState and move into Workspace.hs since we don't need copy pasta functionality
        PFTotalState r1 pasta = updatePFTotalState pfev (PFTotalState _goatState_pFWorkspace [])
        r2 = _pFWorkspace_lastChanges r1
    next_pFState = _pFWorkspace_state next_workspace
    next_layerPosMap = Seq.foldrWithIndex (\lp rid acc -> IM.insert rid lp acc) IM.empty (_pFState_layers next_pFState)
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
      Just (add, sel) -> assert (pFState_selectionIsValid next_pFState (fmap snd3 (toList sel))) $ Just r where
        r = if add
          then disjointUnionSelection _goatState_selection sel
          else sel

    -- get selection from changes
    newEltFoldMapFn rid v = case v of
      Nothing     -> []
      Just (lp,seltl) -> if IM.member rid (_pFState_directory last_pFState) then [] else [(rid,lp,seltl)]
    mSelectionFromChanges = if IM.null cslmap
      then Nothing
      else Just r where
        newlyCreatedSEltls = IM.foldMapWithKey newEltFoldMapFn cslmap
        r = if null newlyCreatedSEltls
          -- if there are no newly created elts, we still need to update the selection
          then catMaybesSeq . flip fmap _goatState_selection $ \sseltl@(rid,_,_) ->
            case IM.lookup rid cslmap of
              -- no changes means not deleted
              Nothing                  -> Just sseltl
              -- if deleted, remove it
              Just Nothing             -> Nothing
              -- it was changed, update selection to newest version
              Just (Just (lp, seltl')) -> Just (rid, lp, seltl')
          else Seq.fromList newlyCreatedSEltls

    mnext_selection = assert (isNothing mSelectionFromPho || isNothing mSelectionFromChanges) $ asum [mSelectionFromPho, mSelectionFromChanges]
    mHandlerFromPho = _potatoHandlerOutput_nextHandler phoAfterGoatCmd
    tempHandlerForUpdateFromSelection = fromMaybe (SomePotatoHandler EmptyHandler) mHandlerFromPho
    tempHandlerForUpdateFromPho = fromMaybe (makeHandlerFromSelection _goatState_selection) mHandlerFromPho
    (next_handler, next_selection) = case mnext_selection of
      Just next_selection' -> (maybeUpdateHandlerFromSelection tempHandlerForUpdateFromSelection next_selection', next_selection')
      Nothing -> (tempHandlerForUpdateFromPho, _goatState_selection)
    next_broadPhaseState = update_bPTree cslmapForBroadPhase (_broadPhaseState_bPTree _goatState_broadPhaseState)
    next_layersState = updateLayers next_pFState cslmapForBroadPhase next_layersState'

  return $ goatStateAfterGoatCmd {
      _goatState_pFWorkspace      = next_workspace
      , _goatState_pan             = next_pan
      , _goatState_handler         = next_handler
      , _goatState_selection       = next_selection
      , _goatState_broadPhaseState = next_broadPhaseState
      , _goatState_layersState     = next_layersState
      , _goatState_layerPosMap = next_layerPosMap
    }

holdGoatWidget :: forall t m. (Adjustable t m, MonadHold t m, MonadFix m)
  => GoatWidgetConfig t
  -> m (GoatWidget t)
holdGoatWidget GoatWidgetConfig {..} = mdo

  let
    goatEvent = leftmostWarn "GoatWidgetConfig_EverythingFrontend"
      [ GoatCmdTool <$> _goatWidgetConfig_selectTool
      , GoatCmdLoad <$> _goatWidgetConfig_load
      , GoatCmdMouse <$> _goatWidgetConfig_mouse
      , GoatCmdKeyboard <$> _goatWidgetConfig_keyboard
      , GoatCmdSetDebugLabel <$> _goatWidgetConfig_setDebugLabel
      ]

    --initialize broadphase with initial state
    initialbp = update_bPTree (fmap Just (_pFState_directory _goatWidgetConfig_initialState)) emptyBPTree
    initiallayersstate = makeLayersStateFromPFState _goatWidgetConfig_initialState
    initialgoat = GoatState {
        _goatState_pFWorkspace      = (loadPFStateIntoWorkspace _goatWidgetConfig_initialState emptyWorkspace)
        , _goatState_selectedTool    = Tool_Select
        , _goatState_pan             = 0
        , _goatState_mouseDrag       = def
        , _goatState_handler         = SomePotatoHandler EmptyHandler
        , _goatState_layersHandler   = SomePotatoHandler (def :: LayersHandler)
        , _goatState_layerScrollPos  = 0
        , _goatState_debugLabel      = ""
        , _goatState_selection       = Seq.empty
        , _goatState_broadPhaseState = initialbp
        , _goatState_layersState     = initiallayersstate
      }

  goatDyn :: Dynamic t GoatState
    <- foldDynM foldGoatFn initialgoat goatEvent

  -- TODO make sure holdUniqDyn actually does what you think it does
  -- I think it does, but it will prob still do full equality check after changes in goatDyn :(
  -- TODO maybe you need to have special signals to control firing of each sub event instead
  -- I guess the good news is that you can still do this without changing the interface
  r_tool <- holdUniqDyn $ fmap _goatState_selectedTool goatDyn
  r_selection <- holdUniqDyn $ fmap _goatState_selection goatDyn
  r_broadphase <- holdUniqDyn $ fmap _goatState_broadPhaseState goatDyn
  r_pan <- holdUniqDyn $ fmap _goatState_pan goatDyn
  r_layers <- holdUniqDyn $ fmap (snd . _goatState_layersState) goatDyn
  r_handlerRenderOutput <- holdUniqDyn $ fmap (\gs -> pRenderHandler (_goatState_handler gs) (potatoHandlerInputFromGoatState gs)) goatDyn

  return GoatWidget
    {
      _goatWidget_tool           = r_tool
      , _goatWidget_selection    = r_selection
      , _goatWidget_layers       = r_layers
      , _goatWidget_pan          = r_pan
      , _goatWidget_broadPhase   = r_broadphase
      , _goatWidget_DEBUG_goatState = goatDyn
    }
