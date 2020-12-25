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
import           Potato.Flow.Controller.Everything
import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.Layers
import           Potato.Flow.Controller.Manipulator.Box
import           Potato.Flow.Controller.Manipulator.Common
import           Potato.Flow.Controller.Manipulator.Line
import           Potato.Flow.Controller.Manipulator.Pan
import           Potato.Flow.Controller.Manipulator.Select
import           Potato.Flow.Entry
import           Potato.Flow.Math
import           Potato.Flow.Render
import           Potato.Flow.SElts
import           Potato.Flow.State
import           Potato.Flow.Types
import           Potato.Flow.Workspace

import           Control.Exception                         (assert)
import           Control.Monad.Fix
import           Data.Default                              (def)
import qualified Data.IntMap                               as IM
import           Data.Maybe
import           Data.Semialign
import qualified Data.Sequence                             as Seq
import           Data.These
import           Data.Tuple.Extra


catMaybesSeq :: Seq (Maybe a) -> Seq a
catMaybesSeq = fmap fromJust . Seq.filter isJust

type EverythingLoadState = (SPotatoFlow, ControllerMeta)

data GoatState = GoatState {
    -- TODO replace with PFWorkspace and we'll manage clipboard in GoatState directly
    _goatState_pFTotalState      :: PFTotalState
    , _goatState_selectedTool    :: Tool
    , _goatState_pan             :: XY -- panPos is position of upper left corner of canvas relative to screen
    , _goatState_mouseDrag       :: MouseDrag -- last mouse dragging state, this is a little questionable, arguably we should only store stuff needed, not the entire mouseDrag
    , _goatState_handler         :: SomePotatoHandler
    , _goatState_layerScrollPos  :: Int
    , _goatState_debugLabel      :: Text
    , _goatState_selection       :: Selection
    , _goatState_broadPhaseState :: BroadPhaseState
    , _goatState_layersState     :: LayersState
  }

goatState_pFState :: GoatState -> PFState
goatState_pFState = _pFWorkspace_state . _pFTotalState_workspace . _goatState_pFTotalState

-- TODO rename to GoatCmd
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
  _goatWidget_tool              :: Dynamic t Tool
  , _goatWidget_selection       :: Dynamic t Selection
  , _goatWidget_layers          :: Dynamic t LayerEntries
  , _goatWidget_pan             :: Dynamic t XY
  , _goatWidget_broadPhase      :: Dynamic t BroadPhaseState

  -- debug stuff prob
  , _goatWidget_DEBUG_goatState :: Dynamic t GoatState
}

makeHandlerFromSelection :: Selection -> SomePotatoHandler
makeHandlerFromSelection selection = case computeSelectionType selection of
  SMTBox         -> SomePotatoHandler $ (def :: BoxHandler)
  SMTLine        -> SomePotatoHandler $ (def :: SimpleLineHandler)
  SMTText        -> SomePotatoHandler $ EmptyHandler -- TODO
  SMTBoundingBox -> SomePotatoHandler $ (def :: BoxHandler) -- pretty sure this is OK?
  SMTNone        -> SomePotatoHandler EmptyHandler

maybeUpdateHandlerFromSelection :: SomePotatoHandler -> Selection -> SomePotatoHandler
maybeUpdateHandlerFromSelection sph selection = case sph of
  SomePotatoHandler h -> if pIsHandlerActive h
    then sph
    else makeHandlerFromSelection selection

foldGoatFn :: (Reflex t) => GoatCmd -> GoatState -> PushM t GoatState
-- TODO rename everything to goat
foldGoatFn cmd everything@GoatState {..} = do
  let

    -- TODO set these
    last_workspace = _pFTotalState_workspace _goatState_pFTotalState
    last_pFState = _pFWorkspace_state last_workspace
    -- TODO cache this in GoatState
    last_layerPosMap = Seq.foldrWithIndex (\lp rid acc -> IM.insert rid lp acc) IM.empty (_pFState_layers last_pFState)



    -- TODO look into getting rid of this
    -- clear per frame statuses for next frame
    everything' = everything {
        -- cancel mouse drag if ESC is pressed
        -- TODO is this the right place to do this?
        _goatState_mouseDrag = case cmd of
          GoatCmdKeyboard (KeyboardData KeyboardKey_Esc _) -> cancelDrag _goatState_mouseDrag
          _ -> _goatState_mouseDrag
      }

    potatoHandlerInput = PotatoHandlerInput {
        _potatoHandlerInput_pFState       = last_pFState
        , _potatoHandlerInput_broadPhase  = _goatState_broadPhaseState
        , _potatoHandlerInput_layerPosMap = last_layerPosMap
        , _potatoHandlerInput_tool = _goatState_selectedTool

        , _potatoHandlerInput_layerScrollPos = _goatState_layerScrollPos
        , _potatoHandlerInput_layersState     = _goatState_layersState
        , _potatoHandlerInput_selection   = _goatState_selection
      }

    -- just get PotatoHandlerOutput from this I guess?
    -- or like... some other output class
    --
    (everythingAfterGoatCmd, phoAfterGoatCmd) = case _goatState_handler of
      SomePotatoHandler handler -> case cmd of
        GoatCmdSetDebugLabel x -> (everything' { _goatState_debugLabel = x }, def)
        GoatCmdTool x -> (everything' { _goatState_selectedTool = x }, def)
        GoatCmdLoad (spf, cm) ->
          -- TODO load ControllerMeta stuff
          (everything', def {
              _potatoHandlerOutput_pFEvent = Just $ PFELoad spf
            })
        --GoatCmdMouse mouseData -> traceShow _goatState_handler $ do
        GoatCmdMouse mouseData ->
          let
            mouseDrag = case _mouseDrag_state _goatState_mouseDrag of
              MouseDragState_Up        -> newDrag mouseData
              MouseDragState_Cancelled -> (continueDrag mouseData _goatState_mouseDrag) { _mouseDrag_state = MouseDragState_Cancelled }
              _                        -> continueDrag mouseData _goatState_mouseDrag

            canvasDrag = toRelMouseDrag last_pFState mouseDrag
            everything'' = everything' { _goatState_mouseDrag = mouseDrag }

          in case _mouseDrag_state mouseDrag of
            -- if mouse was cancelled, update _goatState_mouseDrag accordingly
            MouseDragState_Cancelled -> if _lMouseData_isRelease mouseData
              then (everything'' { _goatState_mouseDrag = emptyMouseDrag }, def)
              else (everything'', def) -- still cancelled
            -- special case, if mouse down and pan tool, we override with a new handler
            MouseDragState_Down | _goatState_selectedTool == Tool_Pan -> case pHandleMouse (def :: PanHandler) potatoHandlerInput canvasDrag of
              Just pho -> (everything'', pho)
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
                  Just pho -> (everything'', pho)
                  Nothing -> error "this should never happen, although if it did, we have many choices to gracefully recover (and I couldn't pick which one so I just did the error thing instead)"
            -- _ -> trace "handler mouse case:\nmouse: " $ traceShow mouseDrag $ trace "prev everything:" $ traceShow everything $ trace "handler" $ traceShow _goatState_handler $ case pHandleMouse handler potatoHandlerInput canvasDrag of
            _ -> case pHandleMouse handler potatoHandlerInput canvasDrag of
              Just pho -> (everything'', pho)
              -- input not captured by handler, do select or drag+select
              Nothing | _mouseDrag_state mouseDrag == MouseDragState_Down -> assert (not $ pIsHandlerActive handler) r where
                nextSelection = selectMagic last_pFState last_layerPosMap _goatState_broadPhaseState canvasDrag
                shiftClick = isJust $ find (==KeyModifier_Shift) (_mouseDrag_modifiers mouseDrag)
                r = if Seq.null nextSelection || shiftClick
                  -- clicked on nothing or shift click, start SelectHandler
                  -- NOTE this is weird because:
                  -- 1. doesn't select until mouse up, which is regression from when you do regular click on an elt (via BoxHandler which immediately selects)
                  -- 2. if you let go of shift before mouse up, it just does a standard mouse selection
                  -- I think this is still better than letting BoxHandler handle this case
                  then case pHandleMouse (def :: SelectHandler) potatoHandlerInput canvasDrag of
                    Just pho -> (everything'', pho)
                    Nothing -> error "handler was expected to capture this mouse state"
                  -- special drag + select case, override the selection
                  -- alternative, we could let the BoxHandler do this but that would mean we query broadphase twice
                  -- (once to determine that we should create the BoxHandler, and again to set the selection in BoxHandler)
                  else case pHandleMouse (def :: BoxHandler) (potatoHandlerInput { _potatoHandlerInput_selection = nextSelection }) canvasDrag of
                    -- it's a little weird because we are forcing the selection from outside the handler and ignoring the new selection results returned by pho (which should always be Nothing)
                    Just pho -> assert (isNothing . _potatoHandlerOutput_select $ pho) $ (everything'', pho { _potatoHandlerOutput_select = Just (False, nextSelection) })
                    Nothing -> error "handler was expected to capture this mouse state"
              Nothing -> error $ "handler " <> show (pHandlerName handler) <> "was expected to capture mouse state " <> show (_mouseDrag_state mouseDrag)
        GoatCmdKeyboard kbd -> case pHandleKeyboard handler potatoHandlerInput kbd of
          Just pho -> (everything', pho)
          -- input not captured by handler
          Nothing -> case kbd of
            KeyboardData KeyboardKey_Esc _ ->
              -- TODO change tool back to select?
              (everything', def {

                  -- deselect everything if we weren't using mouse
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
              r = (everything' { _goatState_selectedTool = newTool }, def)

            -- TODO copy pasta, or maybe copy pasta lives outside of GoatWidget?

            -- unhandled input
            _ -> (everything', def)
        _          -> undefined


    -- update PFTotalState from pho
    (next_pFTotalState, cslmapForBroadPhase) = case _potatoHandlerOutput_pFEvent phoAfterGoatCmd of
      -- if there was no update, then changes are not valid
      Nothing   -> (_goatState_pFTotalState, IM.empty)
      Just pfev -> (r1,r2) where
        r1 = updatePFTotalState pfev _goatState_pFTotalState
        r2 = _pFWorkspace_lastChanges $ _pFTotalState_workspace r1
    next_workspace = _pFTotalState_workspace $ next_pFTotalState
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

    -- update handler from pho
    next_handler' = case _potatoHandlerOutput_nextHandler phoAfterGoatCmd of
      Just sph -> sph
      Nothing  -> case _potatoHandlerOutput_select phoAfterGoatCmd of
        -- TODO I don't think you need to do this anymore, this should be covered later on
        -- there was no new handler, create a new handler from selection to replace the finished one
        -- alternatively, we could let backend do this??
        Nothing -> makeHandlerFromSelection _goatState_selection
        -- in this case, backend will override the handler from the new selection
        Just _  -> SomePotatoHandler EmptyHandler

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
          then catMaybesSeq . flip fmap _goatState_selection $ \sseltl@(rid,_,seltl) ->
            case IM.lookup rid cslmap of
              -- no changes means not deleted
              Nothing                  -> Just sseltl
              -- if deleted, remove it
              Just Nothing             -> Nothing
              -- it was changed, update selection to newest version
              Just (Just (lp, seltl')) -> Just (rid, lp, seltl')
          else Seq.fromList newlyCreatedSEltls

    mnext_selection = assert (isNothing mSelectionFromPho || isNothing mSelectionFromChanges) $ asum [mSelectionFromPho, mSelectionFromChanges]
    (next_handler, next_selection) = case mnext_selection of
      Just next_selection' -> (maybeUpdateHandlerFromSelection next_handler' next_selection', next_selection')
      Nothing -> (next_handler', _goatState_selection)
    next_broadPhaseState = update_bPTree cslmapForBroadPhase (_broadPhaseState_bPTree _goatState_broadPhaseState)
    next_layersState = updateLayers next_pFState cslmapForBroadPhase next_layersState'

  return $ everythingAfterGoatCmd {
      _goatState_pFTotalState      = next_pFTotalState
      , _goatState_pan             = next_pan
      , _goatState_handler         = next_handler
      , _goatState_selection       = next_selection
      , _goatState_broadPhaseState = next_broadPhaseState
      , _goatState_layersState     = next_layersState
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
        _goatState_pFTotalState      = PFTotalState (loadPFStateIntoWorkspace _goatWidgetConfig_initialState emptyWorkspace) []
        , _goatState_selectedTool    = Tool_Select
        , _goatState_pan             = 0
        , _goatState_mouseDrag       = emptyMouseDrag
        , _goatState_handler         = SomePotatoHandler EmptyHandler
        , _goatState_layerScrollPos  = 0
        , _goatState_debugLabel      = ""
        , _goatState_selection       = Seq.empty
        , _goatState_broadPhaseState = initialbp
        , _goatState_layersState     = initiallayersstate
      }

  goatDyn :: Dynamic t GoatState
    <- foldDynM foldGoatFn initialgoat goatEvent

  -- TODO make sure holdUniqDyn actually does what you think it does
  r_tool <- holdUniqDyn $ fmap _goatState_selectedTool goatDyn
  r_selection <- holdUniqDyn $ fmap _goatState_selection goatDyn
  r_broadphase <- holdUniqDyn $ fmap _goatState_broadPhaseState goatDyn
  r_pan <- holdUniqDyn $ fmap _goatState_pan goatDyn
  r_layers <- holdUniqDyn $ fmap (snd . _goatState_layersState) goatDyn

  return GoatWidget
    {
      _goatWidget_tool           = r_tool
      , _goatWidget_selection    = r_selection
      , _goatWidget_layers       = r_layers
      , _goatWidget_pan          = r_pan
      , _goatWidget_broadPhase   = r_broadphase
      , _goatWidget_DEBUG_goatState = goatDyn
    }
