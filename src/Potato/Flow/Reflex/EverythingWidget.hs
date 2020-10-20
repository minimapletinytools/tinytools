{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.EverythingWidget (
  EverythingWidgetConfig(..)
  , emptyEverythingWidgetConfig
  , EverythingWidget(..)
  , holdEverythingWidget
) where

import           Relude

import           Reflex
import           Reflex.Potato.Helpers

import           Potato.Flow.BroadPhase
import           Potato.Flow.Math
import           Potato.Flow.Reflex.Entry
import           Potato.Flow.Reflex.Everything
import           Potato.Flow.Render
import           Potato.Flow.SElts
import           Potato.Flow.State
import           Potato.Flow.Types

import           Control.Exception             (assert)
import           Control.Lens
import           Control.Monad.Fix
import           Data.Default                  (def)
import           Data.Foldable                 (minimum)
import qualified Data.IntMap                   as IM
import qualified Data.List                     as L
import           Data.Maybe
import qualified Data.Sequence                 as Seq
import           Data.Tuple.Extra

data EverythingFrontendCmd =
  EFCmdTool Tool

  -- canvas direct input
  | EFCmdMouse LMouseData
  | EFCmdKeyboard KeyboardData

data EverythingBackendCmd =
  -- selection (first param is add to selection if true)
  -- it's a little weird that selection comes with all info about what's being selected
  -- but we have it already so may as well include it
  EBCmdSelect Bool Selection
  | EBCmdChanges SEltLabelChangesWithLayerPos



data EverythingWidgetConfig t = EverythingWidgetConfig {
  _everythingWidgetConfig_initialState :: PFState

  -- canvas direct input
  , _everythingWidgetConfig_mouse      :: Event t LMouseData
  , _everythingWidgetConfig_keyboard   :: Event t KeyboardData

  -- command based
  , _everythingWidgetConfig_selectTool :: Event t Tool
  , _everythingWidgetConfig_selectNew  :: Event t Selection
  , _everythingWidgetConfig_selectAdd  :: Event t Selection
}

emptyEverythingWidgetConfig :: (Reflex t) => EverythingWidgetConfig t
emptyEverythingWidgetConfig = EverythingWidgetConfig {
    _everythingWidgetConfig_initialState = emptyPFState
    , _everythingWidgetConfig_selectTool  = never
    , _everythingWidgetConfig_mouse     = never
    , _everythingWidgetConfig_keyboard = never
    , _everythingWidgetConfig_selectNew = never
    , _everythingWidgetConfig_selectAdd = never
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
      ]

    foldEverythingFrontendFn :: EverythingFrontendCmd -> EverythingFrontend -> PushM t EverythingFrontend
    foldEverythingFrontendFn cmd everything@EverythingFrontend {..} = do
      let
        -- clear per frame statuses for next frame
        everything' = everything {
            _everythingFrontend_lastOperation = FrontendOperation_None
          }
        -- other useful stuff
        undoFirst = case _everythingFrontend_lastOperation of
          FrontendOperation_Manipulate _ _ -> True
          _                                -> False

      backend <- sample . current $ everythingBackendDyn



      case cmd of
        EFCmdTool x -> return $ everything' { _everythingFrontend_selectedTool = x }
        EFCmdMouse mouseData -> do
          pFState <- sample . current $ _pfo_pFState
          let

            (mouseDrag, deltaDrag) = case _mouseDrag_state _everythingFrontend_mouseDrag of
              MouseDragState_Up        -> (newDrag mouseData, 0)
              MouseDragState_Cancelled -> (newDrag mouseData, 0)
              _                        -> (continueDrag mouseData _everythingFrontend_mouseDrag, mouseDragDelta mouseDrag _everythingFrontend_mouseDrag)

            canvasDragFrom = pFState_toCanvasCoordinates pFState (_mouseDrag_from mouseDrag)
            canvasDragTo = pFState_toCanvasCoordinates pFState (_mouseDrag_to mouseDrag)

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
              let
                -- we could/should cache this but it happens to work out if we don't
                -- TODO this isn't true, another manipulator could end up in the start space when manipulating fix
                -- TODO should we be fancy about multiple manipulators and choosing one depending on where we end up dragging?
                --manipulatorUnderMouseStart = checkMouseDownManipulators canvasDragFrom (_everythingBackend_manipulators backend)
                manipulatorUnderMouseStart = Nothing
              case _mouseDrag_state mouseDrag of
                MouseDragState_Down -> case manipulatorUnderMouseStart of
                  -- no manipulators, don't do anything, we will select upon releasing
                  Nothing -> return everything'
                  Just m  -> undefined -- TODO set manipulation index
                MouseDragState_Dragging -> case _everythingFrontend_lastOperation of
                  FrontendOperation_Manipulate _ i  -> undefined -- TODO manipulate
                  _ -> do
                    return $ everything' {
                        _everythingFrontend_lastOperation = FrontendOperation_Selecting (LBox canvasDragFrom (canvasDragTo - canvasDragFrom))
                      }

                MouseDragState_Up -> case _everythingFrontend_lastOperation of
                  -- if we were manipulating, don't need to do anything
                  FrontendOperation_Manipulate _ _ -> return everything'
                  -- if we weren't manipulating, then we were selecting, then finalize selection
                  _ -> do
                    layerPosMap <- sample . current $ _pfo_layerPosMap
                    let
                      bps = _everythingBackend_broadPhaseState backend
                      shiftClick = isJust $ find (==MouseModifier_Shift) (_mouseDrag_modifiers mouseDrag)
                      boxSize = canvasDragTo - canvasDragFrom
                      singleClick = boxSize == 0
                      selectBox = LBox canvasDragFrom boxSize
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
                    -- selection is stored in backend so pass it on to backend
                    return $  everything' {
                        _everythingFrontend_lastOperation = FrontendOperation_Select shiftClick (Seq.fromList (map (pfState_layerPos_to_superSEltLabel pFState) lps))
                      }

            -- create new elements
            -- note for click + drag on creating new elts, we repeatedly undo + create new elts
            _ -> do
              backend <- sample . current $ everythingBackendDyn

              let
                lastSelectionLps = fmap snd3 $ _everythingBackend_selection backend
                newEltPos = if Seq.null lastSelectionLps then 0 else minimum lastSelectionLps
              case _mouseDrag_state mouseDrag of
                -- if we were manipulating, don't need to do anything
                MouseDragState_Up -> return everything'
                -- otherwise, create a new elt, note this will break if you change tools in the middle of dragging TODO should I bother to fix this?
                _ -> case _everythingFrontend_selectedTool of
                  Tool_Box ->
                    return everything' {
                        _everythingFrontend_lastOperation =
                          FrontendOperation_Manipulate
                            (PFEAddElt (undoFirst, (newEltPos, SEltLabel "<box>" $ SEltBox $ SBox (LBox (canvasDragFrom) (canvasDragTo - canvasDragFrom)) def)))
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
                  return everything'' { _everythingFrontend_pan = V2 (cx0-dx) (cy0-dy) }
                FrontendOperation_LayerDrag -> undefined
                FrontendOperation_Manipulate _ _-> do
                  -- undo the last operation
                  -- TODO do I need to do anything else here?
                  return everything'' { _everythingFrontend_lastOperation = FrontendOperation_Undo }
                FrontendOperation_Selecting _ -> return everything''
                -- change to just return everything''
                -- leaving as undefined now to catch for accidents
                _ -> undefined
          _                              -> undefined
        _          -> undefined

  everythingFrontendDyn :: Dynamic t EverythingFrontend
    <- foldDynM foldEverythingFrontendFn emptyEverythingFrontend everythingFrontendEvent

  let
    frontendOperationEv = updated (fmap _everythingFrontend_lastOperation everythingFrontendDyn)
  --------------
  -- PFOUTPUT --
  --------------
  let
    --backendPFEvent = traceEvent "PF: " $ fforMaybe frontendOperationEv $ \case
    backendPFEvent = fforMaybe frontendOperationEv $ \case
      FrontendOperation_Manipulate cmd _ -> Just cmd
      FrontendOperation_Undo -> Just PFEUndo
      _ -> Nothing

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
    foldEverythingBackendFn cmd everything@EverythingBackend {..} = case cmd of

      EBCmdSelect add sel -> do
        pFState <- sample . current $ _pfo_pFState
        return $ assert (pFState_selectionIsValid pFState (fmap snd3 (toList sel))) ()
        if add
          then return $ everything { _everythingBackend_selection = disjointUnionSelection _everythingBackend_selection sel }
          else return $ everything { _everythingBackend_selection = sel }

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
            then _everythingBackend_selection
            else Seq.fromList newlyCreatedSEltls



        return $ everything {
            -- render
            _everythingBackend_broadPhaseState = newBroadPhaseState
            , _everythingBackend_renderedCanvas = newRenderedCanvas

            -- set new selection if there was a newly created elt
            , _everythingBackend_selection = newSelection

          }
      _          -> undefined



  everythingBackendDyn :: Dynamic t EverythingBackend
    <- foldDynM foldEverythingBackendFn emptyEverythingBackend everythingBackendEvent


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
