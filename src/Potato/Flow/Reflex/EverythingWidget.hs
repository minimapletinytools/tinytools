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



data EverythingCmd =
  ECmdTool Tool
  -- selection (first param is add to selection if true)
  | ECmdSelect Bool Selection

  -- canvas direct input
  | ECmdMouse LMouseData
  | ECmdKeyboard KeyboardData


data EverythingWidgetConfig t = EverythingWidgetConfig {

  -- TODO EverythingWidgetConfig constructs potatoFlow, we want to take an initial state
  -- and possibly some PFConfig as well
  _everythingWidgetConfig_potatoFlow   :: PFOutput t

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
    _everythingWidgetConfig_potatoFlow = undefined
    , _everythingWidgetConfig_selectTool  = never
    , _everythingWidgetConfig_mouse     = never
    , _everythingWidgetConfig_keyboard = never
    , _everythingWidgetConfig_selectNew = never
    , _everythingWidgetConfig_selectAdd = never
  }

data EverythingWidget t = EverythingWidget {
  _everythingWidget_tool           :: Dynamic t Tool
  , _everythingWidget_selection    :: Dynamic t Selection
  , _everythingWidget_layers       :: Dynamic t (Seq LayerDisplay)
  , _everythingWidget_manipulators :: Dynamic t [MouseManipulator]
  , _everythingWidget_pan          :: Dynamic t XY
  , _everythingWidget_broadPhase   :: Dynamic t BPTree
}

holdEverythingWidget :: forall t m. (Adjustable t m, MonadHold t m, MonadFix m)
  => EverythingWidgetConfig t
  -> m (EverythingWidget t)
holdEverythingWidget EverythingWidgetConfig {..} = mdo

  let
    PFOutput {..} = _everythingWidgetConfig_potatoFlow

    -- TODO event for newly created element
    -- you could sample directory for "past" version when listening to _pfo_potato_changed for changes
    newEltsEvent :: Event t [LayerPos]
    newEltsEvent = undefined

    everythingEvent = leftmostWarn "EverythingWidgetConfig"
      [ ECmdTool <$> _everythingWidgetConfig_selectTool
      , ECmdSelect False <$> _everythingWidgetConfig_selectNew
      , ECmdSelect True <$> _everythingWidgetConfig_selectAdd
      , ECmdMouse <$> _everythingWidgetConfig_mouse
      , ECmdKeyboard <$> _everythingWidgetConfig_keyboard
      ]

    -- TODO you nee a DMAP because some of the cmd are allowed to happen at once
    -- order them so they run in the right order
    -- wait does this even work? Specific scenario
    -- create new object -> force a new selection...
    -- ok no it doesn't work, instead the state handler needs to do it without triggering a new event
    -- you could have state reducers and have one reducer call the other...
    -- actually manipulator events need to trigger PFO inputs and get RID for PFO output
    -- so what you actualyl need to do is intercept manipulator events and send them to PFO first
    -- then combine promptly PFO outputs with original inputs
    -- NO everything above is wrong!!!
    -- just have like EverythingFrontend and EverythingBackend!!!
    -- where everyhting frontend samples from EverythingBackend generates inputs for PFOutput
    -- and EverythingBackend reads events from PFOutput
    foldEverythingFn :: EverythingCmd -> EverythingBackend -> PushM t EverythingBackend
    foldEverythingFn cmd everything@EverythingBackend {..} = case cmd of
      ECmdTool x -> return $ everything { _everythingBackend_selectedTool = x }
      ECmdSelect add sel -> do
        pfState <- sample _pfo_pFState
        return $ assert (pFState_selectionIsValid pfState (fmap snd3 (toList sel))) ()
        if add
          then return $ everything { _everythingBackend_selection = disjointUnionSelection _everythingBackend_selection sel }
          else return $ everything { _everythingBackend_selection = sel }
      ECmdMouse mouseData -> do
        pfState <- sample _pfo_pFState

        let
          mouseDrag@MouseDrag{..} = case _everythingBackend_mouseStart of
            Just ms -> continueDrag mouseData ms
            Nothing -> newDrag mouseData
          newMouseStart = case _mouseDrag_state of
            MouseDragState_Up -> Nothing
            _                 -> Just _mouseDrag_start

        everything' <- if _everythingBackend_selectedTool == Tool_Pan then do
          let
            V2 cx0 cy0 = _everythingBackend_pan
            V2 dx dy = (_mouseStart_from _mouseDrag_start) - _mouseDrag_to
          -- TODO simplify formula once you confirm it's correct
          return $ everything { _everythingBackend_pan = V2 (cx0+dx) (cy0 + dy) }


        else if _everythingBackend_selectedTool == Tool_Select then
          -- TODO select or manipulate
          undefined
        else do
          manipulating <- sample . current $ manipulatingDyn
          case manipulating of
            Just (rid, lp, sseltl) -> undefined
              -- TODO manipulate
            Nothing                -> undefined
              -- TODO create new stuff

        -- TODO set the new command or whatever
        return $ everything' { _everythingBackend_mouseStart = newMouseStart }
      ECmdKeyboard x -> case x of
        KeyboardData KeyboardKey_Esc _ -> undefined -- TODO cancel functionality
        _                              -> undefined
      _          -> undefined



  everythingDyn :: Dynamic t EverythingBackend
    <- foldDynM foldEverythingFn emptyEverythingBackend everythingEvent

  -- TODO I think we need to spilt everythingDyn into 2 steps,
  -- 1 for stuff that feeds into PFoutput
  -- and 1 for stuff that comes out of PFOutput
  -- e.g. capture PFOutput manipulating stuff
  manipulatingDyn :: Dynamic t (Maybe SuperSEltLabel)
    <- holdDyn Nothing never
  -- e.g. update layers

  r_tool <- holdUniqDyn $ fmap _everythingBackend_selectedTool everythingDyn
  r_selection <- holdUniqDyn $ fmap _everythingBackend_selection everythingDyn




  return EverythingWidget
    {
      _everythingWidget_tool           = r_tool
      , _everythingWidget_selection    = r_selection
      , _everythingWidget_layers       = undefined
      , _everythingWidget_manipulators = undefined
      , _everythingWidget_pan          = undefined
      , _everythingWidget_broadPhase   = undefined

    }
