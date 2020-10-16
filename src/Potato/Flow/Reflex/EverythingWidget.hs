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
            _everythingFrontend_command = Nothing
            , _everythingFrontend_lastOperation = FrontendOperation_None
            , _everythingFrontend_manipulationIndex = Nothing
          }

      backend <- sample . current $ everythingBackendDyn

      case cmd of
        EFCmdTool x -> return $ everything { _everythingFrontend_selectedTool = x }
        EFCmdMouse mouseData -> do
          pFState <- sample . current $ _pfo_pFState
          let

            (mouseDrag, deltaDrag) = case _mouseDrag_state _everythingFrontend_mouseDrag of
              MouseDragState_Up        -> (newDrag mouseData, 0)
              MouseDragState_Cancelled -> (newDrag mouseData, 0)
              _                        -> (continueDrag mouseData _everythingFrontend_mouseDrag, mouseDragDelta mouseDrag _everythingFrontend_mouseDrag)

            canvasDragFrom = pFState_toCanvasCoordinates pFState (_mouseDrag_from mouseDrag)
            canvasDragTo = pFState_toCanvasCoordinates pFState (_mouseDrag_to mouseDrag)

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
                -- if we have manipulationIndex then that means we are in the middle of an operation
                undoFirst = isJust _everythingFrontend_manipulationIndex
                -- we could/should cache this but it happens to work out if we don't
                -- TODO this isn't true, another manipulator could end up in the start space when manipulating fix
                -- TODO should we be fancy about multiple manipulators and choosing one depending on where we end up dragging?
                --manipulatorUnderMouseStart = checkMouseDownManipulators canvasDragFrom (_everythingBackend_manipulators backend)
              case _mouseDrag_state mouseDrag of
                MouseDragState_Down -> case undefined of
                  -- no manipulators, don't do anything, we will select upon releasing
                  Nothing -> return everything'
                  Just m  -> undefined -- TODO set manipulation index
                MouseDragState_Dragging -> case _everythingFrontend_manipulationIndex of
                  Nothing -> do
                    -- TODO (store selection box in everything frontend for rendering?)
                    return everything'
                  Just i  -> undefined -- TODO manipulate

                MouseDragState_Up -> undefined -- TODO finalize selection if we didn't click on anything to start


              undefined
            -- create new elements
            -- note for click + drag on creating new elts, we repeatedly undo + create new elts
            _ -> do
              backend <- sample . current $ everythingBackendDyn

              let
                lastSelectionLps = fmap snd3 $ _everythingBackend_selection backend
                newEltPos = if Seq.null lastSelectionLps then 0 else minimum lastSelectionLps
                -- if we have manipulationIndex then that means we are in the middle of an operation
                undoFirst = isJust _everythingFrontend_manipulationIndex
              case _everythingFrontend_selectedTool of
                Tool_Box ->
                  return everything' {
                      _everythingFrontend_lastOperation = FrontendOperation_Manipulate
                      -- TODO add undofirst
                      , _everythingFrontend_command = Just (PFEAddElt (undoFirst, (newEltPos, SEltLabel "<box>" $ SEltBox $ SBox (LBox (canvasDragFrom) (canvasDragTo - canvasDragFrom)) def)))
                      , _everythingFrontend_manipulationIndex = Just 0
                    }
                -- TODO finish other types
                _ -> undefined


          -- TODO set the new manipulate command or whatever
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
                FrontendOperation_Manipulate -> assert (isJust _everythingFrontend_command) $
                  -- undo the last operation
                  -- TODO do I need to do anything else here?
                  return everything'' { _everythingFrontend_command = Just PFEUndo }
                _ -> undefined
          _                              -> undefined
        _          -> undefined

  everythingFrontendDyn :: Dynamic t EverythingFrontend
    <- foldDynM foldEverythingFrontendFn emptyEverythingFrontend everythingFrontendEvent

  --------------
  -- PFOUTPUT --
  --------------
  let
    backendPFEvent = fmapMaybe _everythingFrontend_command $ updated everythingFrontendDyn

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
    everythingBackendEvent = leftmostWarn "EverythingWidgetConfig_EverythingBackend"
      [ EBCmdSelect False <$> _everythingWidgetConfig_selectNew
      , EBCmdSelect True <$> _everythingWidgetConfig_selectAdd
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
