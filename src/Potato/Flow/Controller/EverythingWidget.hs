{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.EverythingWidget (
  EverythingWidgetConfig(..)
  , emptyEverythingWidgetConfig
  , EverythingWidget(..)
  , holdEverythingWidget
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

import           Control.Exception                         (assert)
import           Control.Monad.Fix
import           Data.Default                              (def)
import qualified Data.IntMap                               as IM
import           Data.Maybe
import qualified Data.Sequence                             as Seq
import           Data.Tuple.Extra

-- TODO move to your reflex helper lib
-- DELETE you already have it :)
--simultaneous :: (Reflex t) => Event a -> Event b -> Event (a,b)
--simultaneous eva evb = fforMaybe (align eva evb) $ \case
--  These x -> Just x
--  _ -> Nothing

catMaybesSeq :: Seq (Maybe a) -> Seq a
catMaybesSeq = fmap fromJust . Seq.filter isJust

type EverythingLoadState = (SPotatoFlow, ControllerMeta)

data EverythingFrontendCmd =
  EFCmdTool Tool

  | EFCmdLoad EverythingLoadState

  -- canvas direct input
  | EFCmdMouse LMouseData
  | EFCmdKeyboard KeyboardData

  -- debug nonsense
  | EFCmdSetDebugLabel Text
  deriving (Show)

data EverythingBackendCmd =

  -- selection (first param is add to selection if true)
  -- it's a little weird that selection comes with all info about what's being selected
  -- but we have it already so may as well include it
  EBCmdSelect Bool Selection
  | EBCmdChanges SEltLabelChangesWithLayerPos
  | EBCmdNothing
  deriving (Show)



-- | invariants
-- * TODO mouse input type can only change after a `_lMouseData_isRelease == True`
-- * TODO non-mouse inputs can only happen after a `_lMouseData_isRelease == True` except for cancel

data EverythingWidgetConfig t = EverythingWidgetConfig {
  -- TODO should really also include ControllerMeta
  _everythingWidgetConfig_initialState    :: PFState

  -- canvas direct input
  , _everythingWidgetConfig_mouse         :: Event t LMouseData
  , _everythingWidgetConfig_keyboard      :: Event t KeyboardData

  -- command based
  , _everythingWidgetConfig_selectTool    :: Event t Tool
  , _everythingWidgetConfig_load          :: Event t EverythingLoadState

  -- TODO DELETE, selecting handled via SelectHandler and LayerHandler
  , _everythingWidgetConfig_selectNew     :: Event t Selection
  , _everythingWidgetConfig_selectAdd     :: Event t Selection

  -- debugging
  , _everythingWidgetConfig_setDebugLabel :: Event t Text
}

emptyEverythingWidgetConfig :: (Reflex t) => EverythingWidgetConfig t
emptyEverythingWidgetConfig = EverythingWidgetConfig {
    _everythingWidgetConfig_initialState = emptyPFState
    , _everythingWidgetConfig_selectTool  = never
    , _everythingWidgetConfig_load = never
    , _everythingWidgetConfig_mouse     = never
    , _everythingWidgetConfig_keyboard = never
    , _everythingWidgetConfig_selectNew = never
    , _everythingWidgetConfig_selectAdd = never
    , _everythingWidgetConfig_setDebugLabel = never
  }

data EverythingWidget t = EverythingWidget {
  _everythingWidget_tool                       :: Dynamic t Tool
  , _everythingWidget_selection                :: Dynamic t Selection
  , _everythingWidget_layers                   :: Dynamic t LayerEntries
  , _everythingWidget_pan                      :: Dynamic t XY
  , _everythingWidget_broadPhase               :: Dynamic t BroadPhaseState
  , _everythingWidget_pFOutput :: PFOutput t

  , _everythingWidget_everythingCombined_DEBUG :: Dynamic t EverythingCombined_DEBUG
}

fillEverythingFrontendWithHandlerOutput :: Selection -> PotatoHandlerOutput -> EverythingFrontend -> EverythingFrontend
fillEverythingFrontendWithHandlerOutput selection PotatoHandlerOutput {..} frontend = frontend {
    _everythingFrontend_handler = case _potatoHandlerOutput_nextHandler of
      Just sph -> sph
      Nothing  -> case _potatoHandlerOutput_select of
        -- there was no new handler, create a new handler from selection to replace the finished one
        -- alternatively, we could let backend do this??
        Nothing -> makeHandlerFromSelection selection
        -- in this case, backend will override the handler from the new selection
        Just _  -> SomePotatoHandler EmptyHandler
    , _everythingFrontend_select = _potatoHandlerOutput_select
    , _everythingFrontend_pFEvent = _potatoHandlerOutput_pFEvent
    , _everythingFrontend_pan = case _potatoHandlerOutput_pan of
      Nothing -> _everythingFrontend_pan frontend
      Just (V2 dx dy) -> V2 (cx0+dx) (cy0 + dy) where
        V2 cx0 cy0 = _everythingFrontend_pan frontend
    , _everythingFrontend_layersState = case _potatoHandlerOutput_layersState of
      Nothing -> _everythingFrontend_layersState frontend
      Just ls -> ls

  }

makeHandlerFromSelection :: Selection -> SomePotatoHandler
makeHandlerFromSelection selection = case computeSelectionType selection of
  SMTBox         -> SomePotatoHandler $ (def :: BoxHandler)
  SMTLine        -> SomePotatoHandler $ (def :: SimpleLineHandler)
  SMTText        -> SomePotatoHandler $ EmptyHandler -- TODO
  SMTBoundingBox -> SomePotatoHandler $ (def :: BoxHandler) -- pretty sure this is OK?
  SMTNone        -> SomePotatoHandler EmptyHandler

holdEverythingWidget :: forall t m. (Adjustable t m, MonadHold t m, MonadFix m)
  => EverythingWidgetConfig t
  -> m (EverythingWidget t)
holdEverythingWidget EverythingWidgetConfig {..} = mdo

  let
    -------------------------
    -- EVERYTHING FRONTEND --
    -------------------------

    --everythingFrontendEvent = traceEventWith ((<>"\n\n") . show) $ leftmostWarn "EverythingWidgetConfig_EverythingFrontend"
    everythingFrontendEvent = leftmostWarn "EverythingWidgetConfig_EverythingFrontend"
      [ EFCmdTool <$> _everythingWidgetConfig_selectTool
      , EFCmdLoad <$> _everythingWidgetConfig_load
      , EFCmdMouse <$> _everythingWidgetConfig_mouse
      , EFCmdKeyboard <$> _everythingWidgetConfig_keyboard
      , EFCmdSetDebugLabel <$> _everythingWidgetConfig_setDebugLabel
      ]

    foldEverythingFrontendFn :: EverythingFrontendCmd -> EverythingFrontend -> PushM t EverythingFrontend
    -- TODO rename everything to "frontend" or "lastFrontend"
    foldEverythingFrontendFn cmd everything@EverythingFrontend {..} = do

      -- these are always UP TO DATE as frontend only listens to UI events
      backend <- sample . current $ everythingBackendDyn
      pFState <- sample . current $ _pfo_pFState
      layerPosMap <- sample . current $ _pfo_layerPosMap

      let
        -- clear per frame statuses for next frame
        everything' = everything {
            -- clear one shot events
            _everythingFrontend_pFEvent = Nothing
            , _everythingFrontend_select = Nothing

            -- update handler from backend (maybe)
            , _everythingFrontend_handler = someHandler

            -- cancel mouse drag if ESC is pressed
            -- maybe change this? Now that handler's handle their own ESC, it's a little weird to do cancelled mouse state in EverythingWidget
            -- TODO move this up abov ewhen we create everytihng' I guess?
            , _everythingFrontend_mouseDrag = case cmd of
              EFCmdKeyboard (KeyboardData KeyboardKey_Esc _) -> cancelDrag _everythingFrontend_mouseDrag
              _ -> _everythingFrontend_mouseDrag
          }

        selection = _everythingBackend_selection backend

        someHandler = case _everythingBackend_handlerFromSelection backend of
          -- triple check that we're not trying to overwrite an active handler
          Just sh -> assert (not . everythingFrontend_isHandlerActive $ everything) sh
          Nothing -> _everythingFrontend_handler

        broadphase = _everythingBackend_broadPhaseState backend

        potatoHandlerInput = PotatoHandlerInput {
            _potatoHandlerInput_pFState       = pFState
            , _potatoHandlerInput_broadPhase  = broadphase
            , _potatoHandlerInput_layerPosMap = layerPosMap
            , _potatoHandlerInput_tool = _everythingFrontend_selectedTool

            , _potatoHandlerInput_layerScrollPos = _everythingFrontend_layerScrollPos
            , _potatoHandlerInput_layersState     = _everythingFrontend_layersState
            , _potatoHandlerInput_selection   = selection
          }

      case someHandler of
        SomePotatoHandler handler -> case cmd of
          EFCmdSetDebugLabel x -> return everything' { _everythingFrontend_debugLabel = x }
          EFCmdTool x -> return $ everything' { _everythingFrontend_selectedTool = x }
          EFCmdLoad (spf, cm) -> do
            -- TODO load cm stuff
            return $ everything' {
                _everythingFrontend_pFEvent = Just $ PFELoad spf
              }
          --EFCmdMouse mouseData -> traceShow _everythingFrontend_handler $ do
          EFCmdMouse mouseData -> do
            let
              mouseDrag = case _mouseDrag_state _everythingFrontend_mouseDrag of
                MouseDragState_Up        -> newDrag mouseData
                MouseDragState_Cancelled -> (continueDrag mouseData _everythingFrontend_mouseDrag) { _mouseDrag_state = MouseDragState_Cancelled }
                _                        -> continueDrag mouseData _everythingFrontend_mouseDrag

              canvasDrag = toRelMouseDrag pFState mouseDrag
              everything'' = everything' { _everythingFrontend_mouseDrag = mouseDrag }

            case _mouseDrag_state mouseDrag of
              -- if mouse was cancelled, update _everythingFrontend_mouseDrag accordingly
              MouseDragState_Cancelled -> return $ if _lMouseData_isRelease mouseData
                then everything'' { _everythingFrontend_mouseDrag = emptyMouseDrag }
                else everything'' -- still cancelled
              -- special case, if mouse down and pan tool, we override with a new handler
              MouseDragState_Down | _everythingFrontend_selectedTool == Tool_Pan -> case pHandleMouse (def :: PanHandler) potatoHandlerInput canvasDrag of
                Just pho -> return $ fillEverythingFrontendWithHandlerOutput selection pho everything''
                Nothing -> error "this should never happen"
              -- special case, if mouse down and creation tool, we override with a new handler
              MouseDragState_Down | tool_isCreate _everythingFrontend_selectedTool -> assert (not $ pIsHandlerActive handler) $ do
                let
                  someNewHandler = case _everythingFrontend_selectedTool of
                    Tool_Box    -> SomePotatoHandler $ def { _boxHandler_isCreation = True }
                    Tool_Line   -> SomePotatoHandler $ def { _simpleLineHandler_isCreation = True }
                    Tool_Select -> SomePotatoHandler $ (def :: SelectHandler)
                    Tool_Text   -> SomePotatoHandler $ def { _boxHandler_isCreation = True, _boxHandler_isText = True }
                    _           -> error "not valid tool"

                -- pass input onto newly created handler
                return $ case someNewHandler of
                  SomePotatoHandler newHandler -> case pHandleMouse newHandler potatoHandlerInput canvasDrag of
                    Just pho -> fillEverythingFrontendWithHandlerOutput selection pho everything''
                    Nothing -> error "this should never happen, although if it did, we have many choices to gracefully recover (and I couldn't pick which one so I just did the error thing instead)"
              -- _ -> trace "handler mouse case:\nmouse: " $ traceShow mouseDrag $ trace "prev everything:" $ traceShow everything $ trace "handler" $ traceShow someHandler $ case pHandleMouse handler potatoHandlerInput canvasDrag of
              _ -> case pHandleMouse handler potatoHandlerInput canvasDrag of
                Just pho -> return $ fillEverythingFrontendWithHandlerOutput selection pho everything''
                -- input not captured by handler, do select or drag+select
                Nothing | _mouseDrag_state mouseDrag == MouseDragState_Down -> assert (not $ pIsHandlerActive handler) $ do
                  let
                    nextSelection = selectMagic pFState layerPosMap broadphase canvasDrag
                    shiftClick = isJust $ find (==KeyModifier_Shift) (_mouseDrag_modifiers mouseDrag)
                  return $ if Seq.null nextSelection || shiftClick
                    -- clicked on nothing or shift click, start SelectHandler
                    -- NOTE this is weird because:
                    -- 1. doesn't select until mouse up, which is regression from when you do regular click on an elt (via BoxHandler which immediately selects)
                    -- 2. if you let go of shift before mouse up, it just does a standard mouse selection
                    -- I think this is still better than letting BoxHandler handle this case
                    then case pHandleMouse (def :: SelectHandler) potatoHandlerInput canvasDrag of
                      Just pho -> fillEverythingFrontendWithHandlerOutput selection pho everything''
                      Nothing -> error "handler was expected to capture this mouse state"
                    -- special drag + select case, override the selection
                    -- alternative, we could let the BoxHandler do this but that would mean we query broadphase twice
                    -- (once to determine that we should create the BoxHandler, and again to set the selection in BoxHandler)
                    else case pHandleMouse (def :: BoxHandler) (potatoHandlerInput { _potatoHandlerInput_selection = nextSelection }) canvasDrag of
                      -- it's a little weird because we are forcing the selection from outside the handler and ignoring the new selection results returned by pho (which should always be Nothing)
                      Just pho -> assert (isNothing . _potatoHandlerOutput_select $ pho) $ (fillEverythingFrontendWithHandlerOutput selection pho everything'') { _everythingFrontend_select = Just (False, nextSelection) }
                      Nothing -> error "handler was expected to capture this mouse state"
                Nothing -> error $ "handler " <> show (pHandlerName handler) <> "was expected to capture mouse state " <> show (_mouseDrag_state mouseDrag)
          EFCmdKeyboard kbd -> case pHandleKeyboard handler potatoHandlerInput kbd of
            Just pho -> return $ fillEverythingFrontendWithHandlerOutput selection pho everything'
            -- input not captured by handler
            Nothing -> case kbd of
              KeyboardData KeyboardKey_Esc _ -> do
                return everything' {
                    -- TODO change tool back to select?

                    -- deselect everything if we weren't using mouse
                    -- TODO actually probably don't bother checking for this condition, mouse should have been captured in this case
                    _everythingFrontend_select = case _mouseDrag_state _everythingFrontend_mouseDrag of
                      MouseDragState_Up        -> Just (False, Seq.empty)
                      MouseDragState_Cancelled -> Just (False, Seq.empty)
                      _                        -> Nothing
                  }
              KeyboardData (KeyboardKey_Char 'c') [KeyModifier_Ctrl] ->
                -- TODO copy
                undefined
              KeyboardData (KeyboardKey_Char 'v') [KeyModifier_Ctrl] ->
                -- TODO pasta
                undefined
              -- tool hotkeys
              KeyboardData (KeyboardKey_Char key) _ -> return r where
                newTool = case key of
                  'v'  -> Tool_Select
                  -- 'p' -> Tool_Pan
                  'b'  -> Tool_Box
                  '\\' -> Tool_Line
                  't'  -> Tool_Text
                  _    -> _everythingFrontend_selectedTool
                r = everything' { _everythingFrontend_selectedTool = newTool }

              -- TODO copy pasta, or maybe copy pasta lives outside of EverythingWidget?

              -- unhandled input
              _ -> return everything'


          _          -> undefined

  everythingFrontendDyn :: Dynamic t EverythingFrontend
    <- foldDynM foldEverythingFrontendFn emptyEverythingFrontend everythingFrontendEvent

  --------------
  -- PFOUTPUT --
  --------------
  let
    backendPFEvent = fmapMaybe _everythingFrontend_pFEvent (updated everythingFrontendDyn)

    -- connect events to PFConfig
    pFConfig = PFConfig {
      _pfc_addElt = fforMaybe backendPFEvent $ \case
        PFEAddElt x -> Just x
        _ -> Nothing
      , _pfc_addFolder    = fforMaybe backendPFEvent $ \case
        PFEAddFolder x -> Just x
        _ -> Nothing
      , _pfc_deleteElts   = fforMaybe backendPFEvent $ \case
        PFERemoveElt x -> Just x
        _ -> Nothing
      , _pfc_moveElt      = fforMaybe backendPFEvent $ \case
        PFEMoveElt x -> Just x
        _ -> Nothing
      , _pfc_copy         = fforMaybe backendPFEvent $ \case
        PFECopy x -> Just x
        _ -> Nothing
      , _pfc_paste        = fforMaybe backendPFEvent $ \case
        PFEPaste x -> Just x
        _ -> Nothing
      , _pfc_manipulate   = fforMaybe backendPFEvent $ \case
        --PFEManipulate x -> traceShow x $ Just x
        PFEManipulate x -> Just x
        _ -> Nothing
      , _pfc_resizeCanvas = fforMaybe backendPFEvent $ \case
        PFEResizeCanvas x -> Just x
        _ -> Nothing
      , _pfc_undo         = fforMaybe backendPFEvent $ \case
        PFEUndo -> Just ()
        _ -> Nothing
      , _pfc_redo         = fforMaybe backendPFEvent $ \case
        PFERedo -> Just ()
        _ -> Nothing
      , _pfc_load         = fforMaybe backendPFEvent $ \case
        PFELoad x -> Just x
        _ -> Nothing
      , _pfc_save         = never
    }
  pFOutput@PFOutput {..} <- holdPFWithInitialState _everythingWidgetConfig_initialState pFConfig


  ------------------------
  -- EVERYTHING BACKEND --
  ------------------------
  let
    -- TODO hook up to _everythingFrontend_select
    frontendOperation_select = fmapMaybe _everythingFrontend_select (updated everythingFrontendDyn)

    everythingBackendEvent' = leftmostWarn "EverythingWidgetConfig_EverythingBackend"
      [ EBCmdSelect False <$> _everythingWidgetConfig_selectNew
      , EBCmdSelect True <$> _everythingWidgetConfig_selectAdd
      , fmap (uncurry EBCmdSelect) frontendOperation_select
      , EBCmdChanges <$> _pfo_potato_changed
      ]



    -- we need the latest version of frontend. We could have done this using events as well, either
    -- 1. cherry picking just the events we need it each circumstance (currently few enough that this is very feasible)
    -- 2. or connecting backend to (updated everythingFrontendDyn)
    -- attachPromptlyDyn solves this problem in the simplest way, and we can easily change to 2. if we decide so
    everythingBackendEvent = attachPromptlyDyn everythingFrontendDyn $ leftmost $ [
      everythingBackendEvent'
      -- D:, we need to reset backend for every frontend event so we add a dummy empty event
      , EBCmdNothing <$ everythingFrontendEvent]

    foldEverythingBackendFn :: (EverythingFrontend, EverythingBackendCmd) -> EverythingBackend -> PushM t EverythingBackend
    -- TODO rename everything to backend
    --foldEverythingBackendFn (frontend, cmd) everything@EverythingBackend {..} = trace "BACKEND UPDATE" $ do
    foldEverythingBackendFn (frontend, cmd) everything@EverythingBackend {..} = do

      -- DOES NOT include latest changes!
      pFStateMaybeStale <- sample . current $ _pfo_pFState
      layerPosMapMaybeStale <- sample. current $ _pfo_layerPosMap
      let
        everything' = everything {
            _everythingBackend_handlerFromSelection = Nothing
          }

        maybeSetHandler selection = if (everythingFrontend_isHandlerActive frontend)
          then Nothing
          -- only set handler from selection if frontend handler isn't active
          else Just $ makeHandlerFromSelection selection

      case cmd of
        EBCmdSelect add sel -> do
          return $ assert (pFState_selectionIsValid pFStateMaybeStale (fmap snd3 (toList sel))) ()
          let
            newsel = if add
              then disjointUnionSelection _everythingBackend_selection sel
              else sel
          return $ everything' {
              _everythingBackend_selection = newsel
              , _everythingBackend_handlerFromSelection = maybeSetHandler newsel
            }

        EBCmdChanges cslmap -> do
          let

            -- broad phase stuff
            cslmapForBroadPhase = fmap (fmap snd) cslmap
            newBroadPhaseState = update_bPTree cslmapForBroadPhase (_broadPhaseState_bPTree _everythingBackend_broadPhaseState)
            bpt = _broadPhaseState_bPTree newBroadPhaseState
            boxes = _broadPhaseState_needsUpdate newBroadPhaseState
            rc = _everythingBackend_renderedCanvas
            newRenderedCanvas = case boxes of
              [] -> rc
              (b:bs) -> case intersect_lBox (renderedCanvas_box rc) (foldl' union_lBox b bs) of
                Nothing -> rc
                Just aabb -> newrc where
                  slmap = _pFState_directory pFStateMaybeStale
                  rids = broadPhase_cull aabb bpt
                  sseltls' = flip fmap rids $ \rid -> case IM.lookup rid cslmapForBroadPhase of
                    Nothing -> case IM.lookup rid slmap of
                      Nothing -> error "this should never happen, because broadPhase_cull should only give existing seltls"
                      Just seltl -> (rid, seltl)
                    Just mseltl -> case mseltl of
                      Nothing -> error "this should never happen, because deleted seltl would have been culled in broadPhase_cull"
                      Just seltl -> (rid, seltl)
                  sseltls = fmap (\(rid, s) -> (rid, layerPosMapMaybeStale IM.! rid, s)) sseltls'
                  seltls = fmap thd3 . sortOn snd3 $ sseltls
                  newrc = render aabb (map _sEltLabel_sElt seltls) rc

            -- new elt stuff
            lastDir = _pFState_directory pFStateMaybeStale
            newEltFoldMapFn rid v = case v of
              Nothing     -> []
              Just (lp,seltl) -> if IM.member rid lastDir then [] else [(rid,lp,seltl)]
            newlyCreatedSEltls = IM.foldMapWithKey newEltFoldMapFn cslmap
            newSelection = if null newlyCreatedSEltls
              -- if there are no newly created elts, we still need to update the selection
              then catMaybesSeq . flip fmap _everythingBackend_selection $ \sseltl@(rid,_,seltl) ->
                case IM.lookup rid cslmap of
                  -- no changes means not deleted
                  Nothing                  -> Just sseltl
                  -- if deleted, remove it
                  Just Nothing             -> Nothing
                  -- it was changed, update selection to newest version
                  Just (Just (lp, seltl')) -> Just (rid, lp, seltl')
              else Seq.fromList newlyCreatedSEltls

          return $ everything' {
              -- render
              _everythingBackend_broadPhaseState = newBroadPhaseState
              , _everythingBackend_renderedCanvas = newRenderedCanvas

              -- set new selection if there was a newly created elt
              , _everythingBackend_selection = newSelection


              , _everythingBackend_handlerFromSelection = maybeSetHandler newSelection
              -- TODO possibly also call pSelectionUpdated
              -- you'll need to compare the new selection with the previous one and only call if there were actually changes

            }
        _          -> return $ everything'



  let
    --initialize broadphase with initial state
    initialbp = update_bPTree (fmap Just (_pFState_directory _everythingWidgetConfig_initialState)) emptyBPTree
    initialbackend = emptyEverythingBackend { _everythingBackend_broadPhaseState = initialbp }
  everythingBackendDyn :: Dynamic t EverythingBackend
    <- foldDynM foldEverythingBackendFn initialbackend everythingBackendEvent


  r_tool <- holdUniqDyn $ fmap _everythingFrontend_selectedTool everythingFrontendDyn
  r_selection <- holdUniqDyn $ fmap _everythingBackend_selection everythingBackendDyn
  r_broadphase <- holdUniqDyn $ fmap _everythingBackend_broadPhaseState everythingBackendDyn
  r_pan <- holdUniqDyn $ fmap _everythingFrontend_pan everythingFrontendDyn
  --r_layers <- holdUniqDyn $ fmap (fst . _everythingFrontend_layersState) everythingFrontendDyn



  return EverythingWidget
    {
      _everythingWidget_tool           = r_tool
      , _everythingWidget_selection    = r_selection
      , _everythingWidget_layers       = undefined -- TODO
      , _everythingWidget_pan          = r_pan
      , _everythingWidget_broadPhase   = r_broadphase
      , _everythingWidget_pFOutput     = pFOutput
      , _everythingWidget_everythingCombined_DEBUG = ffor3 everythingFrontendDyn everythingBackendDyn _pfo_pFState combineEverything
    }
