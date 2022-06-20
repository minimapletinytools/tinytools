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

import Potato.Flow.Controller.Goat
import           Potato.Flow.BroadPhase
import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.OwlLayers
import           Potato.Flow.Controller.Manipulator.Layers
import           Potato.Flow.Controller.Types
import           Potato.Flow.Math
import           Potato.Flow.Render
import           Potato.Flow.OwlItem
import Potato.Flow.OwlState
import Potato.Flow.Owl
import Potato.Flow.OwlWorkspace
import           Potato.Flow.Types
import Potato.Flow.Llama

import           Control.Exception                         (assert)
import           Control.Monad.Fix
import           Data.Default
import qualified Data.IntMap                               as IM
import qualified Data.Sequence                             as Seq



-- | invariants
-- * TODO mouse input type can only change after a `_lMouseData_isRelease == True`
-- * TODO non-mouse inputs can only happen after a `_lMouseData_isRelease == True` except for cancel
data GoatWidgetConfig t = GoatWidgetConfig {

  -- initialization parameters
  _goatWidgetConfig_initialState     :: (OwlPFState, ControllerMeta)
  , _goatWidgetConfig_unicodeWidthFn :: Maybe UnicodeWidthFn

  -- canvas direct input
  , _goatWidgetConfig_mouse          :: Event t LMouseData
  , _goatWidgetConfig_keyboard       :: Event t KeyboardData

  -- other canvas stuff
  , _goatWidgetConfig_canvasRegionDim     :: Event t XY

  -- command based
  , _goatWidgetConfig_selectTool     :: Event t Tool
  , _goatWidgetConfig_load           :: Event t EverythingLoadState
  -- only intended for setting params
  , _goatWidgetConfig_paramsEvent    :: Event t ControllersWithId
  , _goatWidgetConfig_canvasSize     :: Event t XY
  , _goatWidgetConfig_newFolder :: Event t ()

  -- command based (via new endo style)
  , _goatWidgetConfig_setPotatoDefaultParameters :: Event t SetPotatoDefaultParameters
  , _goatWidgetConfig_markSaved :: Event t ()


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
    , _goatWidgetConfig_setDebugLabel = never
    , _goatWidgetConfig_bypassEvent = never
  }


data GoatWidget t = GoatWidget {
  _goatWidget_tool                  :: Dynamic t Tool


  , _goatWidget_selection           :: Dynamic t Selection
  , _goatWidget_potatoDefaultParameters :: Dynamic t PotatoDefaultParameters

  , _goatWidget_layers              :: Dynamic t LayersState -- do I even need this?

  , _goatWidget_pan                 :: Dynamic t XY
  , _goatWidget_broadPhase          :: Dynamic t BroadPhaseState
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

foldGoatCmdSetDefaultParams :: SetPotatoDefaultParameters -> GoatState -> GoatState
foldGoatCmdSetDefaultParams spdp gs = gs {
    _goatState_potatoDefaultParameters = potatoDefaultParameters_set (_goatState_potatoDefaultParameters gs) spdp
  }

foldGoatCmdMarkSaved :: () -> GoatState -> GoatState
foldGoatCmdMarkSaved _ gs = gs {
    _goatState_workspace = markWorkspaceSaved (_goatState_workspace gs)
  }

holdGoatWidget :: forall t m. (Adjustable t m, MonadHold t m, MonadFix m)
  => GoatWidgetConfig t
  -> m (GoatWidget t)
holdGoatWidget GoatWidgetConfig {..} = mdo

  let
    initialowlpfstate = fst _goatWidgetConfig_initialState
    -- initialize broadphase with initial state
    initialAsSuperOwlChanges = IM.mapWithKey (\rid (oem, oe) -> Just $ SuperOwl rid oem oe) . _owlTree_mapping . _owlPFState_owlTree $ fst _goatWidgetConfig_initialState
    (_, initialbp) = update_bPTree initialowlpfstate initialAsSuperOwlChanges emptyBPTree
    initiallayersstate = makeLayersStateFromOwlPFState initialowlpfstate (_controllerMeta_layers $ snd _goatWidgetConfig_initialState)

    -- TODO DELETE
    -- TODO wrap this in a helper function in Render
    -- TODO we want to render the whole screen, not just the canvas
    initialCanvasBox = _sCanvas_box . _owlPFState_canvas $ initialowlpfstate
    initialselts = fmap (\(_, oelt) -> _owlItem_subItem oelt) . toList . _owlTree_mapping . _owlPFState_owlTree $ initialowlpfstate
    initialemptyrcr = emptyRenderedCanvasRegion initialCanvasBox
    initialrendercontext = RenderContext {
      _renderContext_owlTree = hasOwlTree_owlTree initialowlpfstate
      , _renderContext_layerMetaMap = _layersState_meta initiallayersstate
      , _renderContext_broadPhase = initialbp -- this is ignored but we may as well set in correctly
      , _renderContext_renderedCanvasRegion = initialemptyrcr
    }
    initialrc = _renderContext_renderedCanvasRegion $ render initialCanvasBox initialselts initialrendercontext

    initialgoat = GoatState {
        _goatState_workspace      = loadOwlPFStateIntoWorkspace (fst _goatWidgetConfig_initialState) emptyWorkspace
        , _goatState_pan             = _controllerMeta_pan (snd _goatWidgetConfig_initialState)
        , _goatState_mouseDrag       = def
        , _goatState_handler         = SomePotatoHandler EmptyHandler
        , _goatState_layersHandler   = SomePotatoHandler (def :: LayersHandler)
        , _goatState_potatoDefaultParameters = def
        , _goatState_attachmentMap = owlTree_makeAttachmentMap (_owlPFState_owlTree . fst $ _goatWidgetConfig_initialState)
        , _goatState_debugLabel      = ""
        , _goatState_selection       = isParliament_empty
        , _goatState_canvasSelection = CanvasSelection Seq.empty
        , _goatState_broadPhaseState = initialbp
        , _goatState_renderedCanvas = initialrc
        , _goatState_renderedSelection = initialemptyrcr
        , _goatState_layersState     = initiallayersstate
        , _goatState_clipboard = Nothing
        , _goatState_screenRegion = 0 -- we can't know this at initialization time without causing an infinite loop so it is expected that the app sends this information immediately after initializing (i.e. during postBuild)
        , _goatState_debugCommands = []
      }

    -- old command style
    goatEvent = [



        GoatCmdTool <$> _goatWidgetConfig_selectTool
        , GoatCmdLoad <$> _goatWidgetConfig_load
        , GoatCmdMouse <$> _goatWidgetConfig_mouse
        , GoatCmdKeyboard <$> _goatWidgetConfig_keyboard
        , GoatCmdSetDebugLabel <$> _goatWidgetConfig_setDebugLabel
        , GoatCmdNewFolder <$ _goatWidgetConfig_newFolder
        , ffor _goatWidgetConfig_bypassEvent GoatCmdWSEvent
        , ffor _goatWidgetConfig_canvasRegionDim GoatCmdSetCanvasRegionDim

        -- these two need to be run before _goatWidgetConfig_mouse because sometimes we want to set params and input a mouse at the same time (i.e. clicking away from params widget to canvas widget causing params to send an update)
        , ffor _goatWidgetConfig_paramsEvent $ \cwid -> assert (controllerWithId_isParams cwid) (GoatCmdWSEvent (WSEApplyLlama (False, makePFCLlama . OwlPFCManipulate $ cwid)))
        , ffor _goatWidgetConfig_canvasSize $ \xy -> GoatCmdWSEvent (WSEResizeCanvas (DeltaLBox 0 xy))
      ]

    -- TODO split up foldGoatFn to be endo style
    goatEndoEvent = foldGoatFn <<$>> goatEvent

    -- new Endo folding
    setDefaultParamsEndoEvent = fmap foldGoatCmdSetDefaultParams _goatWidgetConfig_setPotatoDefaultParameters
    markSavedEvent = fmap foldGoatCmdMarkSaved _goatWidgetConfig_markSaved

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
  r_canvas <- holdUniqDyn $ fmap (_owlPFState_canvas . _owlPFWorkspace_pFState . _goatState_workspace) goatDyn
  r_unsavedChanges <- holdUniqDyn $ fmap (goatState_hasUnsavedChanges) goatDyn

  {- this causes 4 calls to foldGoatFn per tick :(
  let
    r_selection = fmap _goatState_selection goatDyn
    r_selection_converted = fmap (\gs -> superOwlParliament_convertToCanvasSelection (_owlPFState_owlTree . _owlPFWorkspace_pFState . _goatState_workspace $ gs) (const True) (_goatState_selection gs)) goatDyn
    r_broadphase = fmap _goatState_broadPhaseState goatDyn
    r_pan = fmap _goatState_pan goatDyn
    r_layers = fmap _goatState_layersState goatDyn
    -- TODO flip order of render and holdUniqDyn
    r_handlerRenderOutput = fmap (\gs -> pRenderHandler (_goatState_handler gs) (potatoHandlerInputFromGoatState gs)) goatDyn
    r_layersHandlerRenderOutput = fmap (\gs -> pRenderLayersHandler (_goatState_layersHandler gs) (potatoHandlerInputFromGoatState gs)) goatDyn
    r_canvas = fmap (_owlPFState_canvas . _owlPFWorkspace_pFState . _goatState_workspace) goatDyn
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
