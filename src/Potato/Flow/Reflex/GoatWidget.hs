{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.GoatWidget (
  GoatWidgetConfig(..)
  , emptyGoatWidgetConfig
  , GoatWidget(..)
  , holdGoatWidget
) where

import           Relude

import           Reflex

import           Potato.Flow.BroadPhase
import           Potato.Flow.Controller.Goat
import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.OwlLayers
import           Potato.Flow.Controller.Types
import           Potato.Flow.Llama
import           Potato.Flow.Math
import           Potato.Flow.OwlState
import           Potato.Flow.OwlWorkspace
import           Potato.Flow.Render
import           Potato.Flow.Types

import           Control.Monad.Fix



-- | invariants
-- * TODO mouse input type can only change after a `_lMouseData_isRelease == True`
-- * TODO non-mouse inputs can only happen after a `_lMouseData_isRelease == True` except for cancel
data GoatWidgetConfig t = GoatWidgetConfig {

  -- initialization parameters
  -- the initial state of the scene, set this to `(owlpfstate_newProject, emptyControllerMeta)` for an empty scene
  _goatWidgetConfig_initialState     :: (OwlPFState, ControllerMeta)
  -- not used yet, set this to Nothing
  , _goatWidgetConfig_unicodeWidthFn :: Maybe UnicodeWidthFn

  -- canvas direct input
  , _goatWidgetConfig_mouse          :: Event t LMouseData
  , _goatWidgetConfig_keyboard       :: Event t KeyboardData

  -- use this to set the rendered region size (set this to the size of the area that renders the scene)
  , _goatWidgetConfig_canvasRegionDim     :: Event t XY

  -- command based
  -- and event to change the tool (NOTE keyboard input may also change the tool, but that's handled by _goatWidgetConfig_keyboard)
  , _goatWidgetConfig_selectTool     :: Event t Tool
  -- an event to load a new scene
  , _goatWidgetConfig_load           :: Event t EverythingLoadState
  -- an event to apply a Llama (operation). This is only intended for setting style parameters.
  , _goatWidgetConfig_paramsEvent    :: Event t Llama
  -- an event to apply a change that will set the canvas size of the scene
  , _goatWidgetConfig_canvasSize     :: Event t XY
  -- an event to create a new folder underneath the current selection
  , _goatWidgetConfig_newFolder :: Event t ()

  -- command based (via new endo style)
  -- and event to set the default styling parameters
  , _goatWidgetConfig_setPotatoDefaultParameters :: Event t SetPotatoDefaultParameters
  -- an event to mark the scene as saved, which is needed for the `goatState_hasUnsavedChanges` method to work
  , _goatWidgetConfig_markSaved :: Event t ()
  -- an event that should fire each time the focus area changes. This is needed such that preview changes are applied when you change focus.
  , _goatWidgetConfig_setFocusedArea :: Event t GoatFocusedArea

  -- debugging
  , _goatWidgetConfig_setDebugLabel  :: Event t Text
  , _goatWidgetConfig_bypassEvent :: Event t WSEvent
}

emptyGoatWidgetConfig :: (Reflex t) => GoatWidgetConfig t
emptyGoatWidgetConfig = GoatWidgetConfig {
    _goatWidgetConfig_initialState = (emptyOwlPFState, emptyControllerMeta)
    , _goatWidgetConfig_selectTool  = never
    , _goatWidgetConfig_load = never
    , _goatWidgetConfig_mouse     = never
    , _goatWidgetConfig_keyboard = never
    , _goatWidgetConfig_paramsEvent = never
    , _goatWidgetConfig_unicodeWidthFn = Nothing
    , _goatWidgetConfig_canvasRegionDim = never
    , _goatWidgetConfig_canvasSize = never
    , _goatWidgetConfig_newFolder = never
    , _goatWidgetConfig_setPotatoDefaultParameters = never
    , _goatWidgetConfig_markSaved = never
    , _goatWidgetConfig_setFocusedArea = never
    , _goatWidgetConfig_setDebugLabel = never
    , _goatWidgetConfig_bypassEvent = never
  }


data GoatWidget t = GoatWidget {

  _goatWidget_tool                  :: Dynamic t Tool

  , _goatWidget_selection           :: Dynamic t Selection
  , _goatWidget_potatoDefaultParameters :: Dynamic t PotatoDefaultParameters
  , _goatWidget_layers              :: Dynamic t LayersState -- do I even need this?
  , _goatWidget_pan                 :: Dynamic t XY 
  , _goatWidget_broadPhase          :: Dynamic t BroadPhaseState -- you can probably remove this
  , _goatWidget_handlerRenderOutput :: Dynamic t HandlerRenderOutput
  , _goatWidget_layersHandlerRenderOutput :: Dynamic t LayersViewHandlerRenderOutput
  , _goatWidget_canvas              :: Dynamic t SCanvas -- TODO DELETE just use OwlPFState
  , _goatWidget_renderedCanvas      :: Dynamic t RenderedCanvasRegion
  , _goatWidget_renderedSelection      :: Dynamic t RenderedCanvasRegion
  , _goatWidget_unsavedChanges     :: Dynamic t Bool

  -- TODO this is no longer debug (or maybe expose just OwlPFState part)
  -- debug stuff prob
  , _goatWidget_DEBUG_goatState     :: Dynamic t GoatState
}

holdGoatWidget :: forall t m. (Adjustable t m, MonadHold t m, MonadFix m)
  => GoatWidgetConfig t
  -> m (GoatWidget t)
holdGoatWidget GoatWidgetConfig {..} = mdo

  let
    initialscreensize = 0 -- we can't know this at initialization time without causing an infinite loop so it is expected that the app sends this information immediately after initializing (i.e. during postBuild)
    initialgoat = makeGoatState initialscreensize _goatWidgetConfig_initialState

    -- old command style
    goatEvent = [
        GoatCmdTool <$> _goatWidgetConfig_selectTool
        , GoatCmdLoad <$> _goatWidgetConfig_load
        , GoatCmdMouse <$> _goatWidgetConfig_mouse
        , GoatCmdKeyboard <$> _goatWidgetConfig_keyboard
        , GoatCmdSetDebugLabel <$> _goatWidgetConfig_setDebugLabel
        , GoatCmdNewFolder "folder" <$ _goatWidgetConfig_newFolder
        , ffor _goatWidgetConfig_bypassEvent GoatCmdWSEvent
        , ffor _goatWidgetConfig_canvasRegionDim GoatCmdSetCanvasRegionDim

        -- these two need to be run before _goatWidgetConfig_mouse because sometimes we want to set params/change focus and input a mouse at the same time (i.e. clicking away from params widget to canvas widget causing params to send an update)
        , ffor _goatWidgetConfig_paramsEvent $ \llama -> (GoatCmdWSEvent (WSEApplyLlama (False, llama)))
        , ffor _goatWidgetConfig_canvasSize $ \xy -> GoatCmdWSEvent (WSEApplyLlama (False, makePFCLlama $ OwlPFCResizeCanvas (DeltaLBox 0 xy)))
        , ffor _goatWidgetConfig_setFocusedArea $ \fa -> GoatCmdSetFocusedArea fa
      ]

    -- TODO split up foldGoatFn to be endo style
    goatEndoEvent = foldGoatFn <<$>> goatEvent

    -- new Endo folding
    setDefaultParamsEndoEvent = fmap endoGoatCmdSetDefaultParams _goatWidgetConfig_setPotatoDefaultParameters
    markSavedEvent = fmap endoGoatCmdMarkSaved _goatWidgetConfig_markSaved

  -- DELETE
  --goatDyn' :: Dynamic t GoatState <- foldDyn foldGoatFn initialgoat goatEvent

  goatDyn' :: Dynamic t GoatState
    <- foldDyn ($) initialgoat $ mergeWith (.) ([setDefaultParamsEndoEvent, markSavedEvent] <> goatEndoEvent)

  -- reduces # of calls to foldGoatFn to 2 :\
  let goatDyn = fmap id goatDyn'

  -- TODO make sure holdUniqDyn actually does what you think it does
  -- I think it does, but it will prob still do full equality check after changes in goatDyn :(
  -- TODO maybe you need to have special signals to control firing of each sub event instead
  -- I guess the good news is that you can still do this without changing the interface
  -- i.e. OwlPFStateChangeFlag and have each OwlPFState operation return a change flag as well
  r_tool <- holdUniqDyn $ fmap goatState_selectedTool goatDyn
  r_selection <- holdUniqDyn $ fmap _goatState_selection goatDyn
  r_potatoDefaultParams <- holdUniqDyn $ fmap _goatState_potatoDefaultParameters goatDyn
  r_broadphase <- holdUniqDyn $ fmap _goatState_broadPhaseState goatDyn
  r_pan <- holdUniqDyn $ fmap _goatState_pan goatDyn
  r_layers <- holdUniqDyn $ fmap _goatState_layersState goatDyn
  -- TODO flip order of render and holdUniqDyn
  r_handlerRenderOutput <- holdUniqDyn $ fmap (\gs -> pRenderHandler (_goatState_handler gs) (potatoHandlerInputFromGoatState gs)) goatDyn
  r_layersHandlerRenderOutput <- holdUniqDyn $ fmap (\gs -> pRenderLayersHandler (_goatState_layersHandler gs) (potatoHandlerInputFromGoatState gs)) goatDyn
  r_canvas <- holdUniqDyn $ fmap (_owlPFState_canvas . _owlPFWorkspace_owlPFState . _goatState_workspace) goatDyn
  r_unsavedChanges <- holdUniqDyn $ fmap (goatState_hasUnsavedChanges) goatDyn

  {- this causes 4 calls to foldGoatFn per tick :(
  let
    r_selection = fmap _goatState_selection goatDyn
    r_selection_converted = fmap (\gs -> superOwlParliament_convertToCanvasSelection (_owlPFState_owlTree . _owlPFWorkspace_owlPFState . _goatState_workspace $ gs) (const True) (_goatState_selection gs)) goatDyn
    r_broadphase = fmap _goatState_broadPhaseState goatDyn
    r_pan = fmap _goatState_pan goatDyn
    r_layers = fmap _goatState_layersState goatDyn
    -- TODO flip order of render and holdUniqDyn
    r_handlerRenderOutput = fmap (\gs -> pRenderHandler (_goatState_handler gs) (potatoHandlerInputFromGoatState gs)) goatDyn
    r_layersHandlerRenderOutput = fmap (\gs -> pRenderLayersHandler (_goatState_layersHandler gs) (potatoHandlerInputFromGoatState gs)) goatDyn
    r_canvas = fmap (_owlPFState_canvas . _owlPFWorkspace_owlPFState . _goatState_workspace) goatDyn
  -}

  let
    --why is this not holdUniqDyn? Is this why I'm getting extra ticks?
    r_renderedCanvas = fmap _goatState_renderedCanvas goatDyn
    r_renderedSelection = fmap _goatState_renderedSelection goatDyn

  return GoatWidget
    {
      _goatWidget_tool           = r_tool
      , _goatWidget_selection    = r_selection
      , _goatWidget_potatoDefaultParameters = r_potatoDefaultParams
      , _goatWidget_layers       = r_layers
      , _goatWidget_pan          = r_pan
      , _goatWidget_broadPhase   = r_broadphase
      , _goatWidget_canvas = r_canvas
      , _goatWidget_renderedCanvas = r_renderedCanvas
      , _goatWidget_renderedSelection = r_renderedSelection
      , _goatWidget_handlerRenderOutput =  r_handlerRenderOutput
      , _goatWidget_layersHandlerRenderOutput = r_layersHandlerRenderOutput
      , _goatWidget_unsavedChanges = r_unsavedChanges
      , _goatWidget_DEBUG_goatState = goatDyn
    }
