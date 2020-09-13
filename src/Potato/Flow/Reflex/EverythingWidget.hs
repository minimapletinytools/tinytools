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
import           Potato.Flow.Reflex.BroadPhase
import           Potato.Flow.Reflex.Entry
import           Potato.Flow.Reflex.Everything
import           Potato.Flow.State
import           Potato.Flow.Types


import           Control.Exception             (assert)
import           Control.Monad.Fix
import           Data.Tuple.Extra



data EverythingFrontendCmd =
  EFCmdTool Tool

  -- canvas direct input
  | EFCmdMouse LMouseData
  | EFCmdKeyboard KeyboardData

data EverythingBackendCmd =
  -- selection (first param is add to selection if true)
  EBCmdSelect Bool Selection

  | EBCmdChanges SEltLabelChanges



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
    -- TODO event for newly created element
    -- you could sample directory for "past" version when listening to _pfo_potato_changed for changes
    newEltsEvent :: Event t [LayerPos]
    newEltsEvent = undefined


    -------------------------
    -- EVERYTHING FRONTEND --
    -------------------------

    everythingFrontendEvent = leftmostWarn "EverythingWidgetConfig_EverythingFrontend"
      [ EFCmdTool <$> _everythingWidgetConfig_selectTool
      , EFCmdMouse <$> _everythingWidgetConfig_mouse
      , EFCmdKeyboard <$> _everythingWidgetConfig_keyboard
      ]

    foldEverythingFrontendFn :: EverythingFrontendCmd -> EverythingFrontend -> PushM t EverythingFrontend
    foldEverythingFrontendFn cmd everything@EverythingFrontend {..} = case cmd of
      EFCmdTool x -> return $ everything { _everythingFrontend_selectedTool = x }
      EFCmdMouse mouseData -> do
        pfState <- sample _pfo_pFState

        let
          mouseDrag@MouseDrag{..} = case _everythingFrontend_mouseStart of
            Just ms -> continueDrag mouseData ms
            Nothing -> newDrag mouseData
          newMouseStart = case _mouseDrag_state of
            MouseDragState_Up -> Nothing
            _                 -> Just _mouseDrag_start

        everything' <- if _everythingFrontend_selectedTool == Tool_Pan
          then do
            let
              V2 cx0 cy0 = _everythingFrontend_pan
              V2 dx dy = _mouseDrag_to - (_mouseStart_from _mouseDrag_start)
            -- TODO simplify formula once you confirm it's correct
            return $ everything { _everythingFrontend_pan = V2 (cx0+dx) (cy0 + dy) }
          else if _everythingFrontend_selectedTool == Tool_Select then
            -- TODO select or manipulate
            undefined
          else do
            manipulating <- sample . current $ (fmap _everythingBackend_manipulating everythingBackendDyn)
            case manipulating of
              Just (rid, lp, sseltl) -> undefined
                -- TODO manipulate
              Nothing                -> undefined
                -- TODO create new stuff

        -- TODO set the new command or whatever
        return $ everything' { _everythingFrontend_mouseStart = newMouseStart }
      EFCmdKeyboard x -> case x of
        KeyboardData KeyboardKey_Esc _ -> undefined -- TODO cancel functionality
        _                              -> undefined
      _          -> undefined

  everythingFrontendDyn :: Dynamic t EverythingFrontend
    <- foldDynM foldEverythingFrontendFn emptyEverythingFrontend everythingFrontendEvent

  --------------
  -- PFOUTPUT --
  --------------
  PFOutput {..} <- holdPFWithInitialState _everythingWidgetConfig_initialState neverPFConfig


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
        pfState <- sample _pfo_pFState
        return $ assert (pFState_selectionIsValid pfState (fmap snd3 (toList sel))) ()
        if add
          then return $ everything { _everythingBackend_selection = disjointUnionSelection _everythingBackend_selection sel }
          else return $ everything { _everythingBackend_selection = sel }
      EBCmdChanges changes -> do
        let
          newBroadPhase = update_bPTree changes (snd3 _everythingBackend_broadPhase)
        return $ everything { _everythingBackend_broadPhase = newBroadPhase }
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
      , _everythingWidget_everythingCombined_DEBUG = ffor2 everythingFrontendDyn everythingBackendDyn combineEverything
    }
