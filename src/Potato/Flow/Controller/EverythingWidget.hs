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
import           Potato.Flow.Controller.Manipulator
import           Potato.Flow.Entry
import           Potato.Flow.Math
import           Potato.Flow.Render
import           Potato.Flow.SElts
import           Potato.Flow.State
import           Potato.Flow.Types

import           Control.Exception                  (assert)
import           Control.Monad.Fix
import           Data.Default                       (def)
import           Data.Foldable                      (minimum)
import qualified Data.IntMap                        as IM
import qualified Data.List                          as L
import           Data.Maybe
import qualified Data.Sequence                      as Seq
import           Data.Tuple.Extra


catMaybesSeq :: Seq (Maybe a) -> Seq a
catMaybesSeq = fmap fromJust . Seq.filter isJust

data EverythingFrontendCmd =
  EFCmdTool Tool

  -- canvas direct input
  | EFCmdMouse LMouseData
  | EFCmdKeyboard KeyboardData

  -- debug nonsense
  | EFCmdSetDebugLabel Text

data EverythingBackendCmd =
  -- selection (first param is add to selection if true)
  -- it's a little weird that selection comes with all info about what's being selected
  -- but we have it already so may as well include it
  EBCmdSelect Bool Selection
  | EBCmdChanges SEltLabelChangesWithLayerPos



data EverythingWidgetConfig t = EverythingWidgetConfig {
  _everythingWidgetConfig_initialState    :: PFState

  -- canvas direct input
  , _everythingWidgetConfig_mouse         :: Event t LMouseData
  , _everythingWidgetConfig_keyboard      :: Event t KeyboardData

  -- command based
  , _everythingWidgetConfig_selectTool    :: Event t Tool
  , _everythingWidgetConfig_selectNew     :: Event t Selection
  , _everythingWidgetConfig_selectAdd     :: Event t Selection

  -- debugging
  , _everythingWidgetConfig_setDebugLabel :: Event t Text
}

emptyEverythingWidgetConfig :: (Reflex t) => EverythingWidgetConfig t
emptyEverythingWidgetConfig = EverythingWidgetConfig {
    _everythingWidgetConfig_initialState = emptyPFState
    , _everythingWidgetConfig_selectTool  = never
    , _everythingWidgetConfig_mouse     = never
    , _everythingWidgetConfig_keyboard = never
    , _everythingWidgetConfig_selectNew = never
    , _everythingWidgetConfig_selectAdd = never
    , _everythingWidgetConfig_setDebugLabel = never
  }

data EverythingWidget t = EverythingWidget {
  _everythingWidget_tool                       :: Dynamic t Tool
  , _everythingWidget_selection                :: Dynamic t Selection
  , _everythingWidget_layers                   :: Dynamic t (Seq LayerDisplay)
  , _everythingWidget_manipulators             :: Dynamic t [MouseManipulator]
  , _everythingWidget_pan                      :: Dynamic t XY
  , _everythingWidget_broadPhase               :: Dynamic t ([AABB], BPTree, SEltLabelChanges)

  , _everythingWidget_everythingCombined_DEBUG :: Dynamic t EverythingCombined_DEBUG
}

selectMagic :: PFState -> REltIdMap LayerPos -> BroadPhaseState -> RelMouseDrag -> Selection
selectMagic pFState layerPosMap bps (RelMouseDrag MouseDrag {..}) = r where
  LBox pos' sz' = make_LBox_from_XYs _mouseDrag_to _mouseDrag_from
  -- always expand selection by 1
  selectBox = LBox pos' (sz' + V2 1 1)
  boxSize = lBox_area selectBox
  singleClick = boxSize == 1
  selectedRids = broadPhase_cull selectBox (_broadPhaseState_bPTree bps)
  mapToLp = map (\rid -> (fromJust . IM.lookup rid $ layerPosMap))
  lps' = mapToLp selectedRids
  lps = if singleClick
    -- single click, select top elt only
    then case lps' of
      [] -> []
      xs -> [L.maximumBy (\lp1 lp2 -> compare lp2 lp1) xs]
    -- otherwise select everything
    else lps'
  r = Seq.fromList $ map (pfState_layerPos_to_superSEltLabel pFState) lps

fillEverythingWithHandlerOutput :: PotatoHandlerOutput -> EverythingFrontend -> EverythingFrontend
fillEverythingWithHandlerOutput (msph, msel, mpfe) everything = everything {
    _everythingFrontend_handler = case msph of
      Just sph -> sph
      Nothing  -> SomePotatoHandler EmptyHandler
    , _everythingFrontend_select = msel
    , _everythingFrontend_pFEvent = mpfe
  }

holdEverythingWidget :: forall t m. (Adjustable t m, MonadHold t m, MonadFix m)
  => EverythingWidgetConfig t
  -> m (EverythingWidget t)
holdEverythingWidget EverythingWidgetConfig {..} = mdo

  let


    -------------------------
    -- EVERYTHING FRONTEND --
    -------------------------

    everythingFrontendEvent = leftmostWarn "EverythingWidgetConfig_EverythingFrontend"
      [ EFCmdTool <$> _everythingWidgetConfig_selectTool
      , EFCmdMouse <$> _everythingWidgetConfig_mouse
      , EFCmdKeyboard <$> _everythingWidgetConfig_keyboard
      , EFCmdSetDebugLabel <$> _everythingWidgetConfig_setDebugLabel
      ]

    foldEverythingFrontendFn :: EverythingFrontendCmd -> EverythingFrontend -> PushM t EverythingFrontend
    foldEverythingFrontendFn cmd everything@EverythingFrontend {..} = do

      backend <- sample . current $ everythingBackendDyn
      pFState <- sample . current $ _pfo_pFState

      let
        -- clear per frame statuses for next frame
        everything' = everything {
            _everythingFrontend_lastOperation = FrontendOperation_None

            -- clear one shot events
            , _everythingFrontend_pFEvent = Nothing
            , _everythingFrontend_select = Nothing
          }
        -- other useful stuff
        undoFirst = case _everythingFrontend_lastOperation of
          FrontendOperation_Manipulate mpfe _ -> isJust mpfe
          _                                   -> False
        selection = _everythingBackend_selection backend

        -- handler (eventually may be override from backend? If so, be sure to set in everything')
        -- TODO doesn't work, needs to be in a pattern match I think o_o
        someHandler = _everythingFrontend_handler

        broadphase = _everythingBackend_broadPhaseState backend

        potatoHandlerInput = PotatoHandlerInput pFState broadphase selection

      case someHandler of
        SomePotatoHandler handler -> case cmd of
          EFCmdSetDebugLabel x -> return everything' { _everythingFrontend_debugLabel = x }
          EFCmdTool x -> return $ everything' { _everythingFrontend_selectedTool = x }
          -- TODO DELETE
          EFCmdMouse mouseData -> case _mouseDrag_state _everythingFrontend_mouseDrag of
            -- if last mouse was cancelled, only "uncancel" when we release the mouse
            MouseDragState_Cancelled -> if _lMouseData_isRelease mouseData
              then return everything' { _everythingFrontend_mouseDrag = emptyMouseDrag }
              else return everything' -- still cancelled
            -- otherwise process the mouse as normal
            _ -> do
              let

                (mouseDrag, deltaDrag) = case _mouseDrag_state _everythingFrontend_mouseDrag of
                  MouseDragState_Up        -> (newDrag mouseData, 0)
                  MouseDragState_Cancelled -> (newDrag mouseData, 0)
                  _                        -> (continueDrag mouseData _everythingFrontend_mouseDrag, mouseDragDelta mouseDrag _everythingFrontend_mouseDrag)

                -- TODO DELETE use canvasDrag below instead
                canvasDragFrom = pFState_toCanvasCoordinates pFState (_mouseDrag_from mouseDrag)
                canvasDragTo = pFState_toCanvasCoordinates pFState (_mouseDrag_to mouseDrag)
                canvasDragDelta = canvasDragTo - canvasDragFrom

                canvasDrag = toRelMouseDrag pFState mouseDrag

              -- TODO clean up unecessary monad or move sampling above into use site
              everything'' <- case _everythingFrontend_selectedTool of
                Tool_Pan -> do
                  -- add delta to pan position
                  let
                    V2 cx0 cy0 = _everythingFrontend_pan
                    V2 dx dy = deltaDrag
                  return $ everything' {
                      _everythingFrontend_pan = V2 (cx0+dx) (cy0 + dy)
                      , _everythingFrontend_lastOperation = FrontendOperation_Pan
                    }
                Tool_Select -> do
                  case _mouseDrag_state mouseDrag of
                    MouseDragState_Down -> do
                      if Seq.null selection
                      then do
                        layerPosMap <- sample . current $ _pfo_layerPosMap
                        let
                          nextSelection = selectMagic pFState layerPosMap broadphase canvasDrag
                          shiftClick = isJust $ find (==KeyModifier_Shift) (_mouseDrag_modifiers mouseDrag)
                          mmi = findFirstMouseManipulator canvasDrag nextSelection
                        case mmi of
                          Nothing -> return everything'
                          Just mi -> return everything' {

                              _everythingFrontend_lastOperation = FrontendOperation_Select shiftClick nextSelection
                            }
                      else let
                          mmi = findFirstMouseManipulator canvasDrag selection
                        in case mmi of
                          Nothing -> return everything'
                          Just mi -> return everything' {
                              -- just indicate the manipulator selected, don't actually manipulate here
                              _everythingFrontend_lastOperation = FrontendOperation_Manipulate Nothing mi
                            }
                    MouseDragState_Dragging -> case _everythingFrontend_lastOperation of
                      FrontendOperation_Manipulate _ lmi  ->  return $ everything' {
                              _everythingFrontend_lastOperation = FrontendOperation_Manipulate (Just operation) mi
                            }
                          where
                            (mi, operation) = makeManipulationController canvasDrag selection lmi undoFirst
                      -- IMPORTANT: special case, if last operation was FrontendOperation_Select and mouse state is MouseDragState_Dragging
                      -- then this means that we are in the special select + manipulate case
                      -- I am not so happy about this, perhaps it's better just to introduce another state var rather than rely on special state of two state vars
                      FrontendOperation_Select _ _ -> return $ everything' {
                              _everythingFrontend_lastOperation = FrontendOperation_Manipulate (Just operation) mi
                            }
                          where
                            -- regenerate mouse info from last frame
                            lastCanvasDrag = toRelMouseDrag pFState _everythingFrontend_mouseDrag
                            lmi = case findFirstMouseManipulator lastCanvasDrag selection of
                              Nothing -> error "expected to find manipulator, this probably means assumptions outlined in comments above are false"
                              Just x -> x
                            (mi, operation) = makeManipulationController canvasDrag selection lmi undoFirst
                      _ -> do
                        return $ everything' {
                            _everythingFrontend_lastOperation = FrontendOperation_Selecting (LBox canvasDragFrom canvasDragDelta)
                          }

                    MouseDragState_Up -> case _everythingFrontend_lastOperation of
                      -- if we were manipulating, don't need to do anything
                      FrontendOperation_Manipulate _ _ -> return everything'
                      -- This happens in the select+manipulate case in which case we have already selected
                      FrontendOperation_Select _ _ -> return everything'
                      -- if we weren't manipulating, then we were selecting, then finalize selection
                      _ -> do
                        layerPosMap <- sample . current $ _pfo_layerPosMap
                        let
                          shiftClick = isJust $ find (==KeyModifier_Shift) (_mouseDrag_modifiers mouseDrag)
                        -- selection is stored in backend so pass it on to backend
                        return $ everything' {
                            _everythingFrontend_lastOperation = FrontendOperation_Select shiftClick $ selectMagic pFState layerPosMap broadphase canvasDrag
                          }
                    -- 'case _mouseDrag_state mouseDrag of'
                    _ -> undefined

                -- 'case _everythingFrontend_selectedTool of'
                -- create new elements
                _ -> do
                  backend <- sample . current $ everythingBackendDyn

                  let
                    lastSelectionLps = fmap snd3 $ _everythingBackend_selection backend
                    newEltPos = if Seq.null lastSelectionLps then 0 else minimum lastSelectionLps
                  case _mouseDrag_state mouseDrag of
                    -- TODO run finalize operation on each thing
                    -- if we were manipulating, don't need to do anything
                    MouseDragState_Up -> return everything'
                    -- otherwise, create a new elt
                    -- note this will break if you change tools in the middle of dragging TODO should I bother to fix this?
                    -- TODO consider doing some funny corner rejiggering depending on direction you drag so that click square is always included?
                    _ -> case _everythingFrontend_selectedTool of
                      Tool_Box ->
                        return everything' {
                            _everythingFrontend_lastOperation =
                              FrontendOperation_Manipulate
                                (Just (PFEAddElt (undoFirst, (newEltPos, SEltLabel "<box>" $ SEltBox $ SBox (LBox (canvasDragFrom) (canvasDragTo - canvasDragFrom)) def))))
                                0
                          }
                      -- TODO finish other types
                      _ -> undefined
              return $ everything'' { _everythingFrontend_mouseDrag = mouseDrag }
          EFCmdKeyboard x -> case x of
            KeyboardData KeyboardKey_Esc _ -> let
                -- cancel the mouse action
                everything'' = everything' { _everythingFrontend_mouseDrag = cancelDrag _everythingFrontend_mouseDrag }
              in
                case _everythingFrontend_lastOperation of
                  FrontendOperation_Pan -> do
                    -- substract entire drag from pan position
                    let
                      V2 cx0 cy0 = _everythingFrontend_pan
                      V2 dx dy = (_mouseDrag_to _everythingFrontend_mouseDrag) - (_mouseDrag_from _everythingFrontend_mouseDrag)
                    -- TODO cancel future pans until mouse is released
                    return everything'' { _everythingFrontend_pan = V2 (cx0-dx) (cy0-dy) }
                  FrontendOperation_Manipulate _ _->
                    -- undo the last operation
                    return everything'' { _everythingFrontend_lastOperation = FrontendOperation_Undo }
                  FrontendOperation_Selecting _ ->
                    -- we don't select until release so just reset to orign state
                    return everything''
                  FrontendOperation_LayerDrag _ ->
                    -- TODO??
                    undefined
                  FrontendOperation_None -> return everything'
                  -- change to just return everything''
                  -- leaving as undefined now to catch for accidents
                  op -> error (show op)
            _                              -> undefined


          -- TODO use this instead
          EFCmdMouse mouseData -> do
            let
              (mouseDrag, deltaDrag) = case _mouseDrag_state _everythingFrontend_mouseDrag of
                MouseDragState_Up        -> (newDrag mouseData, 0)
                MouseDragState_Cancelled -> (newDrag mouseData, 0)
                _                        -> (continueDrag mouseData _everythingFrontend_mouseDrag, mouseDragDelta mouseDrag _everythingFrontend_mouseDrag)

              canvasDrag = toRelMouseDrag pFState mouseDrag

            everything'' <- case _mouseDrag_state mouseDrag of
              -- if mouse was cancelled, update _everythingFrontend_mouseDrag accordingly
              MouseDragState_Cancelled -> return $ if _lMouseData_isRelease mouseData
                then everything' { _everythingFrontend_mouseDrag = emptyMouseDrag }
                else everything' -- still cancelled
              -- if mouse down and creation tool
              MouseDragState_Down | tool_isCreate _everythingFrontend_selectedTool -> return r where
                -- cancel previous handler
                pho = pHandleCancel handler potatoHandlerInput
                -- TODO
                -- create new handler and pass input onto handler
                r = undefined
              _ -> case pHandleMouse handler potatoHandlerInput canvasDrag of
                Just pho -> return $ fillEverythingWithHandlerOutput pho everything'
                -- input not caputerd by handler
                Nothing -> do
                  layerPosMap <- sample . current $ _pfo_layerPosMap
                  let
                    nextSelection = selectMagic pFState layerPosMap broadphase canvasDrag
                  if not (Seq.null nextSelection)
                    then undefined
                      -- TODO create BBox handler and start dragging
                    else undefined
                      -- TODO else create selection handler and pass on input
            return $ everything'' { _everythingFrontend_mouseDrag = mouseDrag }
          EFCmdKeyboard x -> case x of
            KeyboardData KeyboardKey_Esc _ -> do
              let
                -- cancel handler
                pho = pHandleCancel handler potatoHandlerInput
                r = fillEverythingWithHandlerOutput pho everything'
              case fst3 pho of
                -- TODO create handler from selection here
                Nothing -> return r
                Just _  -> return r
            kbd -> do
              let
                mpho = pHandleKeyboard handler potatoHandlerInput kbd
              case mpho of
                Just pho -> return $ fillEverythingWithHandlerOutput pho everything'
                -- input not captured by handler
                Nothing -> case x of
                  -- tool hotkeys
                  KeyboardData (KeyboardKey_Char key) _ -> return r where
                    newTool = case key of
                      'v'  -> Tool_Select
                      -- 'p' -> Tool_Pan
                      'b'  -> Tool_Box
                      '\\' -> Tool_Line
                      't'  -> Tool_Text
                    r = everything' { _everythingFrontend_selectedTool = newTool }

                  -- TODO copy pasta, or maybe copy pasta lives outside of EverythingWidget?

                  -- unhandled input
                  _ -> return everything'


          _          -> undefined

  everythingFrontendDyn :: Dynamic t EverythingFrontend
    <- foldDynM foldEverythingFrontendFn emptyEverythingFrontend everythingFrontendEvent

  let
    frontendOperationEv = updated (fmap _everythingFrontend_lastOperation everythingFrontendDyn)
  --------------
  -- PFOUTPUT --
  --------------
  let
    -- TODO DELETE
    --backendPFEvent = traceEvent "PF: " $ fforMaybe frontendOperationEv $ \case
    backendPFEvent = fforMaybe frontendOperationEv $ \case
      FrontendOperation_Manipulate cmd _ -> cmd
      FrontendOperation_Undo -> Just PFEUndo
      _ -> Nothing

    -- TODO use this instead of the above
    newBackendPFEvent = updated (fmap _everythingFrontend_pFEvent everythingFrontendDyn)

    -- connect events to PFConfig
    -- TODO maybe not all events will come from backendPFEvent
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
  PFOutput {..} <- holdPFWithInitialState _everythingWidgetConfig_initialState pFConfig


  ------------------------
  -- EVERYTHING BACKEND --
  ------------------------
  let
    frontendOperation_select = fforMaybe frontendOperationEv $ \case
      FrontendOperation_Select addToSelection selection -> Just $ EBCmdSelect addToSelection selection
      _ -> Nothing

    everythingBackendEvent = leftmostWarn "EverythingWidgetConfig_EverythingBackend"
      [ EBCmdSelect False <$> _everythingWidgetConfig_selectNew
      , EBCmdSelect True <$> _everythingWidgetConfig_selectAdd
      , frontendOperation_select
      , EBCmdChanges <$> _pfo_potato_changed
      ]

    foldEverythingBackendFn :: EverythingBackendCmd -> EverythingBackend -> PushM t EverythingBackend
    foldEverythingBackendFn cmd everything@EverythingBackend {..} = let
        everything' = everything {
            _everythingBackend_handlerFromSelection = Nothing
          }
      in case cmd of
        EBCmdSelect add sel -> do
          pFState <- sample . current $ _pfo_pFState
          return $ assert (pFState_selectionIsValid pFState (fmap snd3 (toList sel))) ()
          if add
            then return $ everything' { _everythingBackend_selection = disjointUnionSelection _everythingBackend_selection sel }
            else return $ everything' { _everythingBackend_selection = sel }

        EBCmdChanges cslmap -> do
          pFState <- sample . current $ _pfo_pFState
          frontend <- sample . current $ everythingFrontendDyn
          let

            -- broad phase stuff
            cslmapForBroadPhase = fmap (fmap snd) cslmap
            newBroadPhaseState = update_bPTree cslmapForBroadPhase (_broadPhaseState_bPTree _everythingBackend_broadPhaseState)
            bpt = _broadPhaseState_bPTree newBroadPhaseState
            boxes = _broadPhaseState_needsUpdate newBroadPhaseState
            rc = _everythingBackend_renderedCanvas
            newRenderedCanvas = case boxes of
              [] -> rc
              (b:bs) -> case intersect_LBox (renderedCanvas_box rc) (foldl' union_LBox b bs) of
                Nothing -> rc
                Just aabb -> newrc where
                  slmap = _pFState_directory pFState
                  rids = broadPhase_cull aabb bpt
                  seltls = flip fmap rids $ \rid -> case IM.lookup rid cslmapForBroadPhase of
                    Nothing -> case IM.lookup rid slmap of
                      Nothing -> error "this should never happen, because broadPhase_cull should only give existing seltls"
                      Just seltl -> seltl
                    Just mseltl -> case mseltl of
                      Nothing -> error "this should never happen, because deleted seltl would have been culled in broadPhase_cull"
                      Just seltl -> seltl
                  -- TODO need to order seltls by layer position oops
                  newrc = render aabb (map _sEltLabel_sElt seltls) rc

            -- new elt stuff
            lastDir = _pFState_directory pFState
            newEltFoldMapFn rid v = case v of
              Nothing     -> []
              Just (lp,v) -> if IM.member rid lastDir then [] else [(rid,lp,v)]
            newlyCreatedSEltls = IM.foldMapWithKey newEltFoldMapFn cslmap
            newSelection = if null newlyCreatedSEltls
              then catMaybesSeq . flip fmap _everythingBackend_selection $ \sseltl@(rid,_,seltl) ->
                case IM.lookup rid cslmap of
                  Nothing                  -> Just sseltl
                  Just Nothing             -> Nothing
                  Just (Just (lp, sseltl)) -> Just (rid, lp, sseltl)
              else Seq.fromList newlyCreatedSEltls



          return $ everything' {
              -- render
              _everythingBackend_broadPhaseState = newBroadPhaseState
              , _everythingBackend_renderedCanvas = newRenderedCanvas

              -- set new selection if there was a newly created elt
              , _everythingBackend_selection = newSelection

            }
        _          -> undefined



  let
    --initialize broadphase with initial state
    initialbp = update_bPTree (fmap Just (_pFState_directory _everythingWidgetConfig_initialState)) emptyBPTree
    initialbackend = emptyEverythingBackend { _everythingBackend_broadPhaseState = initialbp }
  everythingBackendDyn :: Dynamic t EverythingBackend
    <- foldDynM foldEverythingBackendFn initialbackend everythingBackendEvent


  r_tool <- holdUniqDyn $ fmap _everythingFrontend_selectedTool everythingFrontendDyn
  r_selection <- holdUniqDyn $ fmap _everythingBackend_selection everythingBackendDyn




  return EverythingWidget
    {
      _everythingWidget_tool           = r_tool
      , _everythingWidget_selection    = r_selection
      , _everythingWidget_layers       = undefined
      , _everythingWidget_manipulators = undefined
      , _everythingWidget_pan          = undefined
      , _everythingWidget_broadPhase   = undefined
      , _everythingWidget_everythingCombined_DEBUG = ffor3 everythingFrontendDyn everythingBackendDyn _pfo_pFState combineEverything
    }
