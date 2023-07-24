{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.Goat (
  GoatFocusedArea(..)
  , goatState_hasUnsavedChanges
  , makeGoatState
  , goatState_pFState
  , goatState_selectedTool
  , GoatState(..)
  , GoatCmd(..)
  , foldGoatFn

  -- endo style
  , endoGoatCmdSetDefaultParams
  , endoGoatCmdMarkSaved

  -- exposed for testing
  , potatoHandlerInputFromGoatState
) where

import           Relude

import           Potato.Data.Text.Unicode
import           Potato.Flow.BroadPhase
import           Potato.Flow.Configuration
import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.Manipulator.Box
import           Potato.Flow.Controller.Manipulator.CartLine
import           Potato.Flow.Controller.Manipulator.Common
import           Potato.Flow.Controller.Manipulator.Layers
import           Potato.Flow.Controller.Manipulator.Line
import           Potato.Flow.Controller.Manipulator.Pan
import           Potato.Flow.Controller.Manipulator.Select
import           Potato.Flow.Controller.OwlLayers
import           Potato.Flow.Controller.Types
import           Potato.Flow.Llama
import           Potato.Flow.Math
import           Potato.Flow.Owl
import           Potato.Flow.OwlItem
import           Potato.Flow.OwlState
import           Potato.Flow.OwlWorkspace
import           Potato.Flow.Render
import           Potato.Flow.SEltMethods
import           Potato.Flow.SElts
import           Potato.Flow.Types
import qualified Potato.Flow.Preview as Preview
import Potato.Flow.Methods.LlamaWorks

import           Control.Exception                           (assert)
import           Data.Default
import qualified Data.IntMap                                 as IM
import qualified Data.IntSet                                 as IS
import           Data.Maybe
import qualified Data.Sequence                               as Seq
import qualified Data.Text                                   as T


catMaybesSeq :: Seq (Maybe a) -> Seq a
catMaybesSeq = fmap fromJust . Seq.filter isJust

data GoatFocusedArea =
  GoatFocusedArea_Layers
  | GoatFocusedArea_Canvas
  | GoatFocusedArea_Other -- focus is some area that is not owned by tinytools (e.g. the params widgets)
  | GoatFocusedArea_None
  deriving (Eq, Show)

-- TODO move into its own file
data GoatState = GoatState {

    -- TODO make GoatTab
    -- unique to each document
    _goatState_workspace                 :: OwlPFWorkspace
    , _goatState_pan                     :: XY -- panPos is position of upper left corner of canvas relative to screen
    , _goatState_selection               :: Selection
    , _goatState_canvasSelection         :: CanvasSelection
    , _goatState_broadPhaseState         :: BroadPhaseState
    , _goatState_layersState             :: LayersState
    , _goatState_renderedCanvas          :: RenderedCanvasRegion
    , _goatState_renderedSelection       :: RenderedCanvasRegion -- TODO need sparse variant
    , _goatState_handler                 :: SomePotatoHandler
    , _goatState_layersHandler           :: SomePotatoHandler
    -- TODO consider moving into _goatState_workspace
    , _goatState_attachmentMap           :: AttachmentMap -- map of targets to things attached to it. This is a cache that gets updated over time and can be regenerated from the current OwlTree
    , _goatState_renderCache             :: RenderCache

    -- shared across documents
    , _goatState_configuration           :: PotatoConfiguration -- maybe also move PotatoDefaultParameters into this
    , _goatState_potatoDefaultParameters :: PotatoDefaultParameters
    , _goatState_mouseDrag               :: MouseDrag -- last mouse dragging state, this is a little questionable, arguably we should only store stuff needed, not the entire mouseDrag
    , _goatState_screenRegion            :: XY -- the screen dimensions
    , _goatState_clipboard               :: Maybe SEltTree
    , _goatState_focusedArea             :: GoatFocusedArea
    , _goatState_unbrokenInput       :: Text -- grapheme clusters are inputed as several keyboard character events so we track these inputs here

    -- debug stuff (shared across documents)
    , _goatState_debugLabel              :: Text
    , _goatState_debugCommands           :: [GoatCmd]

  } deriving (Show)

makeGoatState :: XY -> (OwlPFState, ControllerMeta) -> GoatState
makeGoatState (V2 screenx screeny) (initialstate, controllermeta) = goat where
    initialowlpfstate = initialstate
    -- initialize broadphase with initial state
    initialAsSuperOwlChanges = IM.mapWithKey (\rid (oem, oe) -> Just $ SuperOwl rid oem oe) . _owlTree_mapping . _owlPFState_owlTree $ initialstate
    (_, initialbp) = update_bPTree initialowlpfstate initialAsSuperOwlChanges emptyBPTree
    initiallayersstate = makeLayersStateFromOwlPFState initialowlpfstate (_controllerMeta_layers controllermeta)

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
      , _renderContext_cache = emptyRenderCache
    }
    initialrc = _renderContext_renderedCanvasRegion $ render initialCanvasBox initialselts initialrendercontext

    goat = GoatState {
        _goatState_workspace      = fst $ loadOwlPFStateIntoWorkspace (initialstate) emptyWorkspace
        , _goatState_pan             = _controllerMeta_pan controllermeta
        , _goatState_mouseDrag       = def
        , _goatState_handler         = SomePotatoHandler EmptyHandler
        , _goatState_layersHandler   = SomePotatoHandler (def :: LayersHandler)
        , _goatState_configuration = def
        , _goatState_potatoDefaultParameters = def
        , _goatState_attachmentMap = owlTree_makeAttachmentMap (_owlPFState_owlTree initialstate)
        , _goatState_debugLabel      = ""
        , _goatState_selection       = isParliament_empty
        , _goatState_canvasSelection = CanvasSelection Seq.empty
        , _goatState_broadPhaseState = initialbp
        , _goatState_renderedCanvas = initialrc
        , _goatState_renderedSelection = initialemptyrcr
        , _goatState_layersState     = initiallayersstate
        , _goatState_renderCache = emptyRenderCache
        , _goatState_clipboard = Nothing
        , _goatState_focusedArea = GoatFocusedArea_None
        , _goatState_unbrokenInput = ""
        , _goatState_screenRegion = V2 screenx screeny - (_controllerMeta_pan controllermeta)
        , _goatState_debugCommands = []
      }


goatState_pFState :: GoatState -> OwlPFState
goatState_pFState = _owlPFWorkspace_owlPFState . _goatState_workspace

-- TODO instance GoatState HasOwlTree
goatState_owlTree :: GoatState -> OwlTree
goatState_owlTree = _owlPFState_owlTree . goatState_pFState

goatState_hasUnsavedChanges :: GoatState -> Bool
goatState_hasUnsavedChanges = llamaStack_hasUnsavedChanges . _owlPFWorkspace_llamaStack . _goatState_workspace

goatState_selectedTool :: GoatState -> Tool
goatState_selectedTool = fromMaybe Tool_Select . pHandlerTool . _goatState_handler

-- TODO deprecate this in favor of Endo style
data GoatCmd =
  GoatCmdTool Tool
  | GoatCmdSetFocusedArea GoatFocusedArea
  | GoatCmdLoad EverythingLoadState

  -- command based input for widgets not owned by tiny tools
  | GoatCmdWSEvent WSEvent
  | GoatCmdSetCanvasRegionDim XY
  | GoatCmdNewFolder Text

  -- direct input for widgets owned by tiny tools
  | GoatCmdMouse LMouseData
  | GoatCmdKeyboard KeyboardData

  -- debug nonsense
  | GoatCmdSetDebugLabel Text
  deriving (Show)



-- Ok, don't think this needs to be a part of GoatCmdTempOutput but does need to be a part of GoatState
-- TODO do this later
{-
data DynGoatFlags = DynGoatFlags {
  _dynGoatFlags_tool           = r_tool
  , _dynGoatFlags_selection    = r_selection
  , _dynGoatFlags_layers       = r_layers
  , _dynGoatFlags_pan          = r_pan
  , _dynGoatFlags_broadPhase   = r_broadphase
  , _dynGoatFlags_canvas = r_canvas
  , _dynGoatFlags_renderedCanvas = r_renderedCanvas
  , _dynGoatFlags_handlerRenderOutput =  r_handlerRenderOutput
} deriving (Show)

data GoatStateFlag = GoatStateFlag_Tool | GoatStateFlag_Selection | GoatStateFlag_Layers | GoatStateFlag_Pan | GoatStateFlag_BroadPhase | GoatStateFlag_Canvas | GoatStateFlag_RenderedCanvasRegion | GoatStateFlag_HandlerRenderOutput deriving (Show, Eq)
-}


type DoesRefreshHandlers = Bool

-- TODO get rid of me
data GoatCmdTempOutput = GoatCmdTempOutput {
  _goatCmdTempOutput_goatState               :: GoatState

  , _goatCmdTempOutput_nextHandler           :: Maybe SomePotatoHandler

  , _goatCmdTempOutput_select                :: Maybe (Bool, Selection)

  -- TODO DELETE
  , _goatCmdTempOutput_pFEvent               :: Maybe (Bool, WSEvent) -- bool is true if it was a canvas handler event (TODO maybe just store was canvas handler as a separate bool?)

  -- TODO use me
  --, _goatCmdTempOutput_previewEvent          :: Maybe (DoesRefreshHandlers, Preview.Preview)

  , _goatCmdTempOutput_pan                   :: Maybe XY
  , _goatCmdTempOutput_layersState           :: Maybe LayersState
  , _goatCmdTempOutput_changesFromToggleHide :: SuperOwlChanges
} deriving (Show)

-- helpers to extract stuff out of goatState because we use record wildcards and can't access otherwise
goatCmdTempOutput_screenRegion :: GoatCmdTempOutput -> XY
goatCmdTempOutput_screenRegion = _goatState_screenRegion . _goatCmdTempOutput_goatState

goatCmdTempOutput_layersHandler :: GoatCmdTempOutput -> SomePotatoHandler
goatCmdTempOutput_layersHandler = _goatState_layersHandler . _goatCmdTempOutput_goatState


instance Default GoatCmdTempOutput where
  def = GoatCmdTempOutput {

      -- TODO just don't use Default if you're gonna do this...
      _goatCmdTempOutput_goatState = undefined --error "this is expected to be overwritten during initialization"

      , _goatCmdTempOutput_nextHandler  = Nothing
      , _goatCmdTempOutput_select      = Nothing
      , _goatCmdTempOutput_pFEvent     = Nothing
      , _goatCmdTempOutput_pan         = Nothing
      , _goatCmdTempOutput_layersState = Nothing
      , _goatCmdTempOutput_changesFromToggleHide = IM.empty
    }

makeGoatCmdTempOutputFromNothing :: GoatState -> GoatCmdTempOutput
makeGoatCmdTempOutputFromNothing goatState = def {
    _goatCmdTempOutput_goatState = goatState

    -- NOTE the value of _potatoHandlerOutput_nextHandler is not directly translated here
    -- PotatoHandlerOutput interpretation: isNothing _potatoHandlerOutput_nextHandler => handler does not capture input
    -- GoatCmdTempOutput interpretation (when non-canvas input):
    --    -isNothing _potatoHandlerOutput_nextHandler => the particular event we just processed is not related to the canvas handler
    --    -so in this case we default _goatCmdTempOutput_nextHandler = Just _goatState_handler
    , _goatCmdTempOutput_nextHandler = Just (_goatState_handler goatState)
  }

makeGoatCmdTempOutputFromNothingClearHandler :: GoatState -> GoatCmdTempOutput
makeGoatCmdTempOutputFromNothingClearHandler goatState = def {
    _goatCmdTempOutput_goatState = goatState
  }

makeGoatCmdTempOutputFromEvent :: GoatState -> WSEvent -> GoatCmdTempOutput
makeGoatCmdTempOutputFromEvent goatState wsev = (makeGoatCmdTempOutputFromNothing goatState) {
    _goatCmdTempOutput_pFEvent = Just (False, wsev)

    --_goatCmdTempOutput_previewEvent = Just (False, Preview.Commit wsev)
  }

makeGoatCmdTempOutputFromMaybeEvent :: GoatState -> Maybe WSEvent -> GoatCmdTempOutput
makeGoatCmdTempOutputFromMaybeEvent goatState mwsev = (makeGoatCmdTempOutputFromNothing goatState) {
    _goatCmdTempOutput_pFEvent = fmap (\x -> (False,x)) mwsev
  }

makeGoatCmdTempOutputFromPotatoHandlerOutput :: GoatState -> PotatoHandlerOutput -> GoatCmdTempOutput
makeGoatCmdTempOutputFromPotatoHandlerOutput goatState PotatoHandlerOutput {..} =  def {
    _goatCmdTempOutput_goatState = goatState
    , _goatCmdTempOutput_nextHandler = _potatoHandlerOutput_nextHandler
    , _goatCmdTempOutput_select      = _potatoHandlerOutput_select
    , _goatCmdTempOutput_pFEvent     = fmap (\x -> (True,x)) _potatoHandlerOutput_pFEvent
    , _goatCmdTempOutput_pan         = _potatoHandlerOutput_pan
    , _goatCmdTempOutput_layersState = _potatoHandlerOutput_layersState
    , _goatCmdTempOutput_changesFromToggleHide = _potatoHandlerOutput_changesFromToggleHide -- actually not needed, only used by layers
  }


makeGoatCmdTempOutputFromLayersPotatoHandlerOutput :: GoatState -> PotatoHandlerOutput -> GoatCmdTempOutput
makeGoatCmdTempOutputFromLayersPotatoHandlerOutput goatState PotatoHandlerOutput {..} =  def {
    _goatCmdTempOutput_goatState = goatState {
        _goatState_layersHandler = case _potatoHandlerOutput_nextHandler of
          Just h  -> h
          Nothing -> error "expected LayersHandler to return a new handler"
      }
    -- TODO flag that this was not canvas input
    , _goatCmdTempOutput_nextHandler = Nothing
    , _goatCmdTempOutput_select      = _potatoHandlerOutput_select
    , _goatCmdTempOutput_pFEvent     = fmap (\x -> (False,x)) _potatoHandlerOutput_pFEvent
    , _goatCmdTempOutput_pan         = _potatoHandlerOutput_pan
    , _goatCmdTempOutput_layersState = _potatoHandlerOutput_layersState
    , _goatCmdTempOutput_changesFromToggleHide = _potatoHandlerOutput_changesFromToggleHide
  }

makeGoatCmdTempOutputFromUpdateGoatStateFocusedArea :: GoatState -> GoatFocusedArea -> GoatCmdTempOutput
makeGoatCmdTempOutputFromUpdateGoatStateFocusedArea goatState gfa = r where
  didchange = gfa /= _goatState_focusedArea goatState
  goatstatewithnewfocus = goatState { _goatState_focusedArea = gfa }
  noactionneeded = makeGoatCmdTempOutputFromNothing goatstatewithnewfocus
  potatoHandlerInput = potatoHandlerInputFromGoatState goatState
  -- if we were renaming, finalize the rename operation by sending a fake return key event, I can't think of a less ad-hoc way to do this
  r = if didchange && pHandlerName (_goatState_layersHandler goatState) == handlerName_layersRename
    then assert (_goatState_focusedArea goatState == GoatFocusedArea_Layers) $ case pHandleKeyboard (_goatState_layersHandler goatState) potatoHandlerInput (KeyboardData KeyboardKey_Return []) of
      Nothing -> noactionneeded
      Just pho -> makeGoatCmdTempOutputFromLayersPotatoHandlerOutput goatstatewithnewfocus pho
    else noactionneeded

-- | hack function for resetting both handlers
-- It would be nice if we actually cancel/reset the handlers (such that in progress operations are undone), but I don't think it really matters
forceResetBothHandlersAndMakeGoatCmdTempOutput :: GoatState -> GoatCmdTempOutput
forceResetBothHandlersAndMakeGoatCmdTempOutput goatState = r where

  -- I think this is Ok
  msph_h = Nothing
  msph_lh = Just (SomePotatoHandler (def :: LayersHandler))

  r = def {
      _goatCmdTempOutput_goatState = goatState {
          _goatState_layersHandler = case msph_lh of
            Just x  -> x
            Nothing -> error "expected LayersHandler to return a new handler"
        }
      , _goatCmdTempOutput_nextHandler = msph_h
    }

makeHandlerFromNewTool :: GoatState -> Tool -> SomePotatoHandler
makeHandlerFromNewTool GoatState{..} = \case
  Tool_Box    -> SomePotatoHandler $ def { _boxHandler_creation = BoxCreationType_Box }
  Tool_Line   -> SomePotatoHandler $ def { _autoLineHandler_isCreation = True }
  Tool_CartLine -> SomePotatoHandler $ def { _cartLineHandler_isCreation = True }
  Tool_Select -> makeHandlerFromSelection _goatState_canvasSelection
  Tool_Text   -> SomePotatoHandler $ def { _boxHandler_creation = BoxCreationType_Text }
  Tool_TextArea -> SomePotatoHandler $ def { _boxHandler_creation = BoxCreationType_TextArea }
  Tool_Pan           -> SomePotatoHandler $ (def :: PanHandler)


-- TODO rename to makeHandlerFromCanvasSelection
makeHandlerFromSelection :: CanvasSelection -> SomePotatoHandler
makeHandlerFromSelection selection = case computeSelectionType selection of
  SMTBox         -> SomePotatoHandler $ (def :: BoxHandler)
  SMTBoxText     -> SomePotatoHandler $ (def :: BoxHandler)
  SMTLine        -> SomePotatoHandler $ (def :: AutoLineHandler)
  SMTTextArea    -> SomePotatoHandler $ (def :: BoxHandler)
  SMTBoundingBox -> SomePotatoHandler $ (def :: BoxHandler)
  SMTNone        -> SomePotatoHandler EmptyHandler

maybeUpdateHandlerFromSelection :: SomePotatoHandler -> CanvasSelection -> SomePotatoHandler
maybeUpdateHandlerFromSelection sph selection = case sph of
  SomePotatoHandler h -> if pIsHandlerActive h
    then sph
    else makeHandlerFromSelection selection

makeClipboard :: GoatState -> Maybe SEltTree
makeClipboard goatState@GoatState {..} = r where
  r = if isParliament_null _goatState_selection
    then _goatState_clipboard
    else Just $ superOwlParliament_toSEltTree (goatState_owlTree goatState) _goatState_selection

-- TODO move to Potato.Flow.Methods.LlamaWorks
deleteSelectionEvent :: GoatState -> Maybe WSEvent
deleteSelectionEvent gs@GoatState {..} = if isParliament_null _goatState_selection
  then Nothing
  else Just $ WSEApplyLlama (False, removeEltAndUpdateAttachments_to_llama (goatState_pFState gs) _goatState_attachmentMap (superOwlParliament_toOwlParliament _goatState_selection))

potatoHandlerInputFromGoatState :: GoatState -> PotatoHandlerInput
potatoHandlerInputFromGoatState GoatState {..} = r where
  last_workspace = _goatState_workspace
  last_pFState = _owlPFWorkspace_owlPFState last_workspace
  r = PotatoHandlerInput {
    _potatoHandlerInput_pFState       = last_pFState
    , _potatoHandlerInput_potatoDefaultParameters = _goatState_potatoDefaultParameters
    , _potatoHandlerInput_broadPhase  = _goatState_broadPhaseState
    , _potatoHandlerInput_renderCache = _goatState_renderCache

    -- the screen region in canvas space
    , _potatoHandlerInput_screenRegion = LBox (-_goatState_pan) _goatState_screenRegion

    , _potatoHandlerInput_layersState     = _goatState_layersState
    , _potatoHandlerInput_selection   = _goatState_selection
    , _potatoHandlerInput_canvasSelection = _goatState_canvasSelection
  }


-- | filters out keyboard input based on the configuration
-- must provide last character-unbroken sequence of text input in order to detect grapheme cluster
-- relies on assumption 🙈
-- let ...(n-1)(n) be a sequence of codepoints that is a grapheme cluster
-- then ...(n-1) is also a grapheme cluster
potatoModifyKeyboardKey :: PotatoConfiguration -> Text -> KeyboardData -> Maybe KeyboardData
potatoModifyKeyboardKey PotatoConfiguration {..} lastUnbrokenCharacters k = case k of
  KeyboardData (KeyboardKey_Char c) mods -> r where
    fulltext = T.snoc lastUnbrokenCharacters c
    r = if not _potatoConfiguration_allowGraphemeClusters && endsInGraphemeCluster fulltext
      then Nothing
      else case _potatoConfiguration_allowOrReplaceUnicodeWideChars of
        Nothing -> Just k
        Just x -> if getCharWidth c > 1
          then maybe Nothing (\nc -> Just (KeyboardData (KeyboardKey_Char nc) mods))  x
          else Just k
  _ -> Just k

-- TODO probably should have done "Endo GoatState" instead of "GoatCmd"
-- TODO extract this method into another file
-- TODO make State monad for this
foldGoatFn :: GoatCmd -> GoatState -> GoatState
--foldGoatFn cmd goatStateIgnore = trace ("FOLDING " <> show cmd) $ finalGoatState where
foldGoatFn cmd goatStateIgnore = finalGoatState where

  -- TODO do some sort of rolling buffer here prob
  -- NOTE even with a rolling buffer, I think this will leak if no one forces the thunk!
  --goatState = goatStateIgnore { _goatState_debugCommands = cmd:_goatState_debugCommands }
  goatState' = goatStateIgnore

  -- it's convenient/lazy to reset unbrokenInput here, this will get overriden in cases where it needs to be
  goatState = goatState' { _goatState_unbrokenInput = "" }
  last_unbrokenInput = _goatState_unbrokenInput goatState'
  last_workspace = _goatState_workspace goatState
  last_pFState = _owlPFWorkspace_owlPFState last_workspace

  potatoHandlerInput = potatoHandlerInputFromGoatState goatState

  -- TODO this step can update OwlState built-in cache (via select operation)
  -- | Process commands |
  goatCmdTempOutput = case (_goatState_handler goatState) of
    SomePotatoHandler handler -> case cmd of
      GoatCmdSetDebugLabel x -> makeGoatCmdTempOutputFromNothing $ goatState { _goatState_debugLabel = x }
      GoatCmdSetCanvasRegionDim x -> makeGoatCmdTempOutputFromNothing $ goatState { _goatState_screenRegion = x }
      GoatCmdWSEvent x ->  makeGoatCmdTempOutputFromEvent goatState x
      GoatCmdNewFolder x -> makeGoatCmdTempOutputFromEvent goatState newFolderEv where
        folderPos = lastPositionInSelection (_owlPFState_owlTree . _owlPFWorkspace_owlPFState $  (_goatState_workspace goatState)) (_goatState_selection goatState)
        newFolderEv = WSEApplyLlama (False, makeAddFolderLlama last_pFState (folderPos, x))
      GoatCmdLoad (spf, cm) -> r where
        -- HACK this won't get generated until later but we need this to generate layersState...
        -- someday we'll split up foldGoatFn by `GoatCmd` (or switch to Endo `GoatState`) and clean this up
        tempOwlPFStateHack = sPotatoFlow_to_owlPFState spf
        r = (makeGoatCmdTempOutputFromEvent goatState (WSELoad spf)) {
            _goatCmdTempOutput_pan = Just $ _controllerMeta_pan cm
            , _goatCmdTempOutput_layersState = Just $ makeLayersStateFromOwlPFState tempOwlPFStateHack (_controllerMeta_layers cm)
           }


      GoatCmdTool x -> r where
        -- TODO do we need to cancel the old handler?
        r = makeGoatCmdTempOutputFromNothing (goatState { _goatState_handler = makeHandlerFromNewTool goatState x })

      GoatCmdSetFocusedArea gfa -> makeGoatCmdTempOutputFromUpdateGoatStateFocusedArea goatState gfa

      GoatCmdMouse mouseData ->
        let
          sameSource = _mouseDrag_isLayerMouse (_goatState_mouseDrag goatState) == _lMouseData_isLayerMouse mouseData
          mouseSourceFailure = _mouseDrag_state (_goatState_mouseDrag goatState) /= MouseDragState_Up && not sameSource
          mouseDrag = case _mouseDrag_state (_goatState_mouseDrag goatState) of
            MouseDragState_Up        -> newDrag mouseData
            MouseDragState_Cancelled -> (continueDrag mouseData (_goatState_mouseDrag goatState)) { _mouseDrag_state = MouseDragState_Cancelled }

            _                        ->  continueDrag mouseData (_goatState_mouseDrag goatState)

          canvasDrag = toRelMouseDrag last_pFState (_goatState_pan goatState) mouseDrag

          goatState_withNewMouse = goatState {
              _goatState_mouseDrag = mouseDrag
              , _goatState_focusedArea = if isLayerMouse then GoatFocusedArea_Layers else GoatFocusedArea_Canvas
            }
          -- TODO call makeGoatCmdTempOutputFromUpdateGoatStateFocusedArea and merge outputs instead UG, or is there a trick for us to be renentrant into foldGoatFn?

          noChangeOutput = makeGoatCmdTempOutputFromNothing goatState_withNewMouse

          isLayerMouse = _mouseDrag_isLayerMouse mouseDrag

        in case _mouseDrag_state mouseDrag of

          -- hack to recover after sameSource issue
          -- TODO TEST
          _ | mouseSourceFailure -> assert False $
            forceResetBothHandlersAndMakeGoatCmdTempOutput goatState_withNewMouse

          -- if mouse was cancelled, update _goatState_mouseDrag accordingly
          MouseDragState_Cancelled -> if _lMouseData_isRelease mouseData
            then makeGoatCmdTempOutputFromNothing $ goatState_withNewMouse { _goatState_mouseDrag = def }
            else noChangeOutput -- still cancelled

          -- if mouse is intended for layers
          _ | isLayerMouse -> case pHandleMouse (_goatState_layersHandler goatState) potatoHandlerInput (RelMouseDrag mouseDrag) of
            Just pho -> makeGoatCmdTempOutputFromLayersPotatoHandlerOutput goatState_withNewMouse pho
            Nothing  -> noChangeOutput

          -- if middle mouse button, create a temporary PanHandler
          MouseDragState_Down | _lMouseData_button mouseData == MouseButton_Middle -> r where
            panhandler = def { _panHandler_maybePrevHandler = Just (SomePotatoHandler handler) }
            r = case pHandleMouse panhandler potatoHandlerInput canvasDrag of
              Just pho -> makeGoatCmdTempOutputFromPotatoHandlerOutput goatState_withNewMouse pho
              Nothing -> error "PanHandler expected to capture mouse input"

          -- pass onto canvas handler
          _ -> case pHandleMouse handler potatoHandlerInput canvasDrag of
            Just pho -> makeGoatCmdTempOutputFromPotatoHandlerOutput goatState_withNewMouse pho

            -- input not captured by handler, pass onto select or select+drag
            Nothing | _mouseDrag_state mouseDrag == MouseDragState_Down -> assert (not $ pIsHandlerActive handler) r where
              r = case pHandleMouse (def :: SelectHandler) potatoHandlerInput canvasDrag of
                Just pho -> makeGoatCmdTempOutputFromPotatoHandlerOutput goatState_withNewMouse pho
                Nothing -> error "handler was expected to capture this mouse state"

            Nothing -> error $ "handler " <> show (pHandlerName handler) <> "was expected to capture mouse state " <> show (_mouseDrag_state mouseDrag)

      GoatCmdKeyboard kbd' -> let
          next_unbrokenInput = case kbd' of
            KeyboardData (KeyboardKey_Char c) _ -> T.snoc last_unbrokenInput c
            _ -> ""
          mkbd =   potatoModifyKeyboardKey (_goatState_configuration goatState) last_unbrokenInput kbd'
          goatState_withKeyboard =  goatState { _goatState_unbrokenInput = next_unbrokenInput}
        in case mkbd of
          Nothing -> makeGoatCmdTempOutputFromNothing goatState_withKeyboard
          -- special case, treat escape cancel mouse drag as a mouse input
          Just (KeyboardData KeyboardKey_Esc _) | mouseDrag_isActive (_goatState_mouseDrag goatState_withKeyboard) -> r where
            canceledMouse = cancelDrag (_goatState_mouseDrag goatState_withKeyboard)
            goatState_withNewMouse = goatState_withKeyboard {
                _goatState_mouseDrag = canceledMouse

                -- escape will cancel mouse focus
                -- TODO this isn't correct, you have some handlers that cancel into each other, you should only reset to GoatFocusedArea_None if they canceled to Nothing
                , _goatState_focusedArea = GoatFocusedArea_None

              }

            -- TODO use _goatState_focusedArea instead
            r = if _mouseDrag_isLayerMouse (_goatState_mouseDrag goatState_withKeyboard)
              then case pHandleMouse (_goatState_layersHandler goatState_withKeyboard) potatoHandlerInput (RelMouseDrag canceledMouse) of
                Just pho -> makeGoatCmdTempOutputFromLayersPotatoHandlerOutput goatState_withNewMouse pho
                Nothing  -> makeGoatCmdTempOutputFromNothingClearHandler goatState_withNewMouse
              else case pHandleMouse handler potatoHandlerInput (toRelMouseDrag last_pFState (_goatState_pan goatState_withKeyboard) canceledMouse) of
                Just pho -> makeGoatCmdTempOutputFromPotatoHandlerOutput goatState_withNewMouse pho
                Nothing  -> makeGoatCmdTempOutputFromNothingClearHandler goatState_withNewMouse

          -- we are in the middle of mouse drag, ignore all keyboard inputs
          -- perhaps a better way to do this is to have handlers capture all inputs when active
          Just _ | mouseDrag_isActive (_goatState_mouseDrag goatState_withKeyboard) -> makeGoatCmdTempOutputFromNothing goatState_withKeyboard

          Just kbd ->
            let
              maybeHandleLayers = do
                guard $ _mouseDrag_isLayerMouse (_goatState_mouseDrag goatState_withKeyboard)
                pho <- pHandleKeyboard (_goatState_layersHandler goatState_withKeyboard) potatoHandlerInput kbd
                return $ makeGoatCmdTempOutputFromLayersPotatoHandlerOutput goatState_withKeyboard pho
            in case maybeHandleLayers of
              Just x -> x
              Nothing -> case pHandleKeyboard handler potatoHandlerInput kbd of
                Just pho -> makeGoatCmdTempOutputFromPotatoHandlerOutput goatState_withKeyboard pho
                -- input not captured by handler
                -- TODO consider wrapping this all up in KeyboardHandler or something? Unfortunately, copy needs to modify goatState_withKeyboard which PotatoHandlerOutput can't atm
                Nothing -> case kbd of
                  KeyboardData KeyboardKey_Esc _ ->
                    (makeGoatCmdTempOutputFromNothing goatState_withKeyboard) {
                        -- TODO change tool back to select?
                        -- cancel selection if we are in a neutral mouse state and there is no handler
                        _goatCmdTempOutput_select = case _mouseDrag_state (_goatState_mouseDrag goatState_withKeyboard) of
                          MouseDragState_Up        -> Just (False, isParliament_empty)
                          MouseDragState_Cancelled -> Just (False, isParliament_empty)
                          _                        -> Nothing
                      }

                  KeyboardData (KeyboardKey_Delete) [] -> r where
                    r = makeGoatCmdTempOutputFromMaybeEvent goatState_withKeyboard (deleteSelectionEvent goatState_withKeyboard)
                  KeyboardData (KeyboardKey_Backspace) [] -> r where
                    r = makeGoatCmdTempOutputFromMaybeEvent goatState_withKeyboard (deleteSelectionEvent goatState_withKeyboard)

                  KeyboardData (KeyboardKey_Char 'c') [KeyModifier_Ctrl] -> r where
                    copied = makeClipboard goatState_withKeyboard
                    r = makeGoatCmdTempOutputFromNothing $ goatState_withKeyboard { _goatState_clipboard = copied }
                  KeyboardData (KeyboardKey_Char 'x') [KeyModifier_Ctrl] -> r where
                    copied = makeClipboard goatState_withKeyboard
                    r = makeGoatCmdTempOutputFromMaybeEvent (goatState_withKeyboard { _goatState_clipboard = copied }) (deleteSelectionEvent goatState_withKeyboard)
                  KeyboardData (KeyboardKey_Char 'v') [KeyModifier_Ctrl] -> case _goatState_clipboard goatState_withKeyboard of
                    Nothing    -> makeGoatCmdTempOutputFromNothing goatState_withKeyboard
                    Just stree -> r where
                      offsetstree = offsetSEltTree (V2 1 1) stree
                      minitree' = owlTree_fromSEltTree offsetstree
                      -- reindex the tree so there are no collisions with the current state
                      maxid1 = owlTree_maxId minitree' + 1
                      maxid2 = owlPFState_nextId (_owlPFWorkspace_owlPFState (_goatState_workspace goatState_withKeyboard))
                      minitree = owlTree_reindex (max maxid1 maxid2) minitree'
                      spot = lastPositionInSelection (goatState_owlTree goatState_withKeyboard) (_goatState_selection goatState_withKeyboard)
                      treePastaEv = WSEApplyLlama (False, makePFCLlama $ OwlPFCNewTree (minitree, spot))
                      r = makeGoatCmdTempOutputFromEvent (goatState_withKeyboard { _goatState_clipboard = Just offsetstree }) treePastaEv
                  KeyboardData (KeyboardKey_Char 'z') [KeyModifier_Ctrl] -> r where
                    r = makeGoatCmdTempOutputFromEvent goatState_withKeyboard WSEUndo
                  KeyboardData (KeyboardKey_Char 'y') [KeyModifier_Ctrl] -> r where
                    r = makeGoatCmdTempOutputFromEvent goatState_withKeyboard WSERedo
                  -- tool hotkeys
                  KeyboardData (KeyboardKey_Char key) _ -> r where
                    mtool = case key of
                      'v' -> Just Tool_Select
                      'p' -> Just Tool_Pan
                      'b' -> Just Tool_Box
                      'l' -> Just Tool_Line
                      'n' -> Just Tool_TextArea
                      _   -> Nothing

                    newHandler = maybe (_goatState_handler goatState_withKeyboard) (makeHandlerFromNewTool goatState_withKeyboard) mtool
                    r = makeGoatCmdTempOutputFromNothing $ goatState_withKeyboard { _goatState_handler = newHandler }

                  -- unhandled input
                  _ -> makeGoatCmdTempOutputFromNothing goatState_withKeyboard

  -- | update OwlPFWorkspace from GoatCmdTempOutput |
  (workspace_afterEvent, cslmap_afterEvent) = case _goatCmdTempOutput_pFEvent goatCmdTempOutput of
    -- if there was no update, then changes are not valid
    Nothing   -> (_goatState_workspace goatState, IM.empty)
    Just (_, wsev) -> updateOwlPFWorkspace wsev (_goatState_workspace goatState)
  pFState_afterEvent = _owlPFWorkspace_owlPFState workspace_afterEvent

  -- | update pan from GoatCmdTempOutput |
  next_pan = case _goatCmdTempOutput_pan goatCmdTempOutput of
    Nothing -> _goatState_pan goatState
    Just (V2 dx dy) -> V2 (cx0+dx) (cy0 + dy) where
      V2 cx0 cy0 = _goatState_pan goatState

  -- | get layersState from GoatCmdTempOutput |
  next_layersState'' = case _goatCmdTempOutput_layersState goatCmdTempOutput of
    Nothing -> _goatState_layersState goatState
    Just ls -> ls

  -- | get selection from GoatCmdTempOutput |
  mSelectionFromPho = case _goatCmdTempOutput_select goatCmdTempOutput of
    Nothing -> Nothing
    --Just (add, sel) -> assert (superOwlParliament_isValid nextot r) $ Just r where
    Just (add, sel) -> assert (superOwlParliament_isValid nextot r) (Just r)where
      nextot = _owlPFState_owlTree pFState_afterEvent
      r' = if add
        then superOwlParliament_disjointUnionAndCorrect nextot (_goatState_selection goatState) sel
        else sel
      r = SuperOwlParliament . Seq.sortBy (owlTree_superOwl_comparePosition nextot) . unSuperOwlParliament $ r'

  -- | compute selection based on changes from updating OwlPFState (i.e. auto select newly created stuff if appropriate) |
  (isNewSelection', selectionAfterChanges) = if IM.null cslmap_afterEvent
    then (False, _goatState_selection goatState)
    else r where

      -- extract elements that got created
      newEltFoldMapFn rid v = case v of
        Nothing     -> []
        Just sowl -> if IM.member rid (_owlTree_mapping . _owlPFState_owlTree $ last_pFState) then [] else [sowl]

      -- NOTE, undoing a deleted element counts as a newly created element (and will be auto-selected)
      newlyCreatedSEltls = IM.foldMapWithKey newEltFoldMapFn cslmap_afterEvent

      sortedNewlyCreatedSEltls = SuperOwlParliament $ Seq.sortBy (owlTree_superOwl_comparePosition $ _owlPFState_owlTree $ pFState_afterEvent) (Seq.fromList newlyCreatedSEltls)
      -- pretty sure this does the same thing..
      --sortedNewlyCreatedSEltls = makeSortedSuperOwlParliament (_owlPFState_owlTree $ pFState_afterEvent) (Seq.fromList newlyCreatedSEltls)

      wasLoad = case cmd of
        GoatCmdLoad _ -> True
        _             -> False

      r = if wasLoad || null newlyCreatedSEltls
        -- if there are no newly created elts, we still need to update the selection
        then (\x -> (False, SuperOwlParliament x)) $ catMaybesSeq . flip fmap (unSuperOwlParliament (_goatState_selection goatState)) $ \sowl ->
          case IM.lookup (_superOwl_id sowl) cslmap_afterEvent of
            -- no changes means not deleted
            Nothing       -> Just sowl
            -- if deleted, remove it
            Just Nothing  -> Nothing
            -- it was changed, update selection to newest version
            Just (Just x) -> Just x
        else (True, sortedNewlyCreatedSEltls)

  -- for now, newly created stuff is the same as anything that got auto selected
  --newlyCreatedRids = IS.fromList . toList . fmap _superOwl_id . unSuperOwlParliament $ selectionAfterChanges

  -- | update the new selection based on previous computations|
  (isNewSelection, next_selection) = case mSelectionFromPho of
    Just x  -> assert (not isNewSelection') (True, x)
    -- better/more expensive check to ensure mSelectionFromPho stuff is mutually exclusive to selectionAfterChanges
    --Just x -> assert (selectionAfterChanges == _goatState_selection) (True, x)
    Nothing -> (isNewSelection', selectionAfterChanges)

  -- | update LayersState based from SuperOwlChanges after applying events |
  next_layersState' = updateLayers pFState_afterEvent cslmap_afterEvent next_layersState''

  -- | auto-expand folders and compute LayersState |
  -- auto expand folders for selected elements + (this will also auto expand when you drag or paste stuff into a folder)
  -- NOTE this will prevent you from ever collapsing a folder that has a selected child in it
  -- so maybe auto expand should only happen on newly created elements or add a way to detect for newly selected elements (e.g. diff between old selection)
  next_layersState = expandAllCollapsedParents next_selection pFState_afterEvent next_layersState'
  --next_layersState = next_layersState'

  -- | update the next handler |
  mHandlerFromPho = _goatCmdTempOutput_nextHandler goatCmdTempOutput
  filterHiddenOrLocked sowl = not $ layerMetaMap_isInheritHiddenOrLocked (_owlPFState_owlTree pFState_afterEvent) (_superOwl_id sowl) (_layersState_meta next_layersState)
  next_canvasSelection = superOwlParliament_convertToCanvasSelection (_owlPFState_owlTree pFState_afterEvent) filterHiddenOrLocked next_selection
  nextHandlerFromSelection = makeHandlerFromSelection next_canvasSelection
  next_handler' = if isNewSelection
    -- (a) if there is a new selection, update the handler with new selection if handler wasn't active
    then maybeUpdateHandlerFromSelection (fromMaybe (SomePotatoHandler EmptyHandler) mHandlerFromPho) next_canvasSelection
    -- (b) otherwise, use the returned handler or make a new one from selection if there was no returned handler
    else fromMaybe nextHandlerFromSelection mHandlerFromPho
  next_layersHandler' = goatCmdTempOutput_layersHandler goatCmdTempOutput

  -- | refresh the handler if there was a non-canvas event |
  -- TODO simplify this logic, it's confusing A.F.
  -- TODO you only need to do this if handler is one that came from mHandlerFromPho (b) above because handlers produced from selection should already be up to date
  (next_handler, next_layersHandler) = case _goatCmdTempOutput_pFEvent goatCmdTempOutput of
    -- since we don't have multi-user events, the handler should never be active when this happens
    Just (False, _) -> assert (not (pIsHandlerActive next_handler')) $ (refreshedHandler,refreshedLayersHandler) where
      -- TODO you need to create a second event processing stage (i.e. intermediateGoatState) because refresh handler might return a cancel operation 
      -- CAREFUL INFINITE LOOP DANGER WITH USE OF `finalGoatState`
      -- safe for now, since `potatoHandlerInputFromGoatState` does not use `_goatState_handler/_goatState_layersHandler finalGoatState` which is set to `next_handler/next_layersHandler`
      next_potatoHandlerInput = potatoHandlerInputFromGoatState finalGoatState
      refreshedHandler = fromMaybe nextHandlerFromSelection ( pRefreshHandler next_handler' next_potatoHandlerInput)
      -- a little weird that we refresh the layers handler in cases where the event was an event that came from layers, but it doesn't matter since the layers handler won't be active when it produces that event
      refreshedLayersHandler = fromMaybe (SomePotatoHandler (def :: LayersHandler)) (pRefreshHandler next_layersHandler' next_potatoHandlerInput)
    _ -> (next_handler', next_layersHandler')


  -- | TODO enter rename mode for newly created folders |
  -- TODO if cslmap_afterEvent has a newly created folder (i.e. we just createda folder) then we want to enter rename mode for that folder
    -- this is not correct, we want a condition for when we hit the "new folder" button. Perhaps there needs to be a separate command for enter rename and FE triggers 2 events in succession?
  --_goatState_layersHandler

  -- | update AttachmentMap based on new state and clear the cache on these changes |
  next_attachmentMap = updateAttachmentMapFromSuperOwlChanges cslmap_afterEvent (_goatState_attachmentMap goatState)
  -- we need to union with `_goatState_attachmentMap` as next_attachmentMap does not contain deleted targets and stuff we detached from
  attachmentMapForComputingChanges = IM.unionWith IS.union next_attachmentMap (_goatState_attachmentMap goatState)
  --attachmentChanges = trace "ATTACHMENTS" $ traceShow (IM.size cslmap_afterEvent) $ traceShowId $ getChangesFromAttachmentMap (_owlPFState_owlTree pFState_afterEvent) attachmentMapForComputingChanges cslmap_afterEvent
  attachmentChanges = getChangesFromAttachmentMap (_owlPFState_owlTree pFState_afterEvent) attachmentMapForComputingChanges cslmap_afterEvent

  -- | compute SuperOwlChanges for rendering |
  cslmap_withAttachments = IM.union cslmap_afterEvent attachmentChanges
  cslmap_fromLayersHide = _goatCmdTempOutput_changesFromToggleHide goatCmdTempOutput
  cslmap_forRendering = cslmap_fromLayersHide `IM.union` cslmap_withAttachments

  -- | clear the cache at places that have changed |
  renderCache_resetOnChangesAndAttachments = renderCache_clearAtKeys (_goatState_renderCache goatState) (IM.keys cslmap_withAttachments)

  -- | update the BroadPhase
  (needsupdateaabbs, next_broadPhaseState) = update_bPTree (_owlPFState_owlTree pFState_afterEvent) cslmap_forRendering (_broadPhaseState_bPTree (_goatState_broadPhaseState goatState))

  -- | update the rendered region if we moved the screen |
  canvasRegionBox = LBox (-next_pan) (goatCmdTempOutput_screenRegion goatCmdTempOutput)
  newBox = canvasRegionBox
  didScreenRegionMove = _renderedCanvasRegion_box (_goatState_renderedCanvas goatState) /= newBox
  rendercontext_forMove = RenderContext {
      _renderContext_cache = renderCache_resetOnChangesAndAttachments
      , _renderContext_owlTree = _owlPFState_owlTree pFState_afterEvent
      , _renderContext_layerMetaMap = _layersState_meta next_layersState
      , _renderContext_broadPhase = next_broadPhaseState
      , _renderContext_renderedCanvasRegion = _goatState_renderedCanvas goatState
    }
  rendercontext_forUpdate = if didScreenRegionMove
    then moveRenderedCanvasRegion newBox rendercontext_forMove
    else rendercontext_forMove

  -- | render the scene if there were changes, note that updates from actual changes are mutually exclusive from updates due to panning (although I think it would still work even if it weren't) |
  rendercontext_afterUpdate = if IM.null cslmap_forRendering
    then rendercontext_forUpdate
    else updateCanvas cslmap_forRendering needsupdateaabbs rendercontext_forUpdate

  next_renderedCanvas = _renderContext_renderedCanvasRegion rendercontext_afterUpdate

  -- | render the selection |
  rendercontext_forSelection = rendercontext_afterUpdate {
      -- NOTE this will render hidden stuff that's selected via layers!!
      _renderContext_layerMetaMap = IM.empty

      -- TODO DELETE THIS YOU SHOULDN'T HAVE TO DO THIS, this is breaking caching (you can fix by commenting it out)
      -- IDK WHY BUT IF YOU SELECT AUTOLINE WITH BOX AND MOVE BOTH THE CACHE STAYS WITH ORIGINAL PLACE AND SELECTED LINE DOESN'T MOVE
      -- so temp fix it by reseting the cache on attached lines whos target moved
      , _renderContext_cache = renderCache_resetOnChangesAndAttachments

      -- empty canvas to render our selection in
      -- we just re-render everything for now (in the future you can try and do partial rendering though)
      , _renderContext_renderedCanvasRegion = emptyRenderedCanvasRegion newBox
    }
  selectionselts = toList . fmap _superOwl_id $ unSuperOwlParliament next_selection

  (next_renderedSelection, next_renderCache) = if _goatState_selection goatState == next_selection && not didScreenRegionMove && IM.null cslmap_forRendering
    -- nothing changed, we can keep our selection rendering
    then (_goatState_renderedSelection goatState, _renderContext_cache rendercontext_afterUpdate)
    else (_renderContext_renderedCanvasRegion rctx, _renderContext_cache rctx) where
      rctx = render_new newBox selectionselts rendercontext_forSelection

  -- TODO just DELETE this...
  {- TODO render only parts of selection that have changed TODO broken
  next_renderedSelection' = if didScreenRegionMove
    then moveRenderedCanvasRegion next_broadPhaseState (owlTree_withCacheResetOnAttachments) newBox _goatState_renderedSelection
    else _goatState_renderedSelection
  prevSelChangeMap = IM.fromList . toList . fmap (\sowl -> (_superOwl_id sowl, Nothing)) $ unSuperOwlParliament _goatState_selection
  curSelChangeMap = IM.fromList . toList . fmap (\sowl -> (_superOwl_id sowl, Just sowl)) $ unSuperOwlParliament next_selection
  -- TODO you can be even smarter about this by combining cslmap_forRendering I think
  cslmapForSelectionRendering = curSelChangeMap `IM.union` prevSelChangeMap
  -- you need to do something like this but this is wrong....
  --(needsupdateaabbsforrenderselection, _) = update_bPTree cslmapForSelectionRendering (_broadPhaseState_bPTree next_broadPhaseState)
  needsupdateaabbsforrenderselection = needsupdateaabbs
  next_renderedSelection = if IM.null cslmapForSelectionRendering
    then next_renderedSelection'
    else updateCanvas cslmapForSelectionRendering needsupdateaabbsforrenderselection next_broadPhaseState pFState_withCacheResetOnAttachments next_renderedSelection'
  -}

  next_pFState = pFState_afterEvent { _owlPFState_owlTree = _renderContext_owlTree rendercontext_forSelection }
  next_workspace = workspace_afterEvent { _owlPFWorkspace_owlPFState = next_pFState}

  checkAttachmentMap = owlTree_makeAttachmentMap (_owlPFState_owlTree next_pFState) == next_attachmentMap

  -- TODO remove assert in production builds
  finalGoatState = if not checkAttachmentMap
    then error $ (show (owlTree_makeAttachmentMap (_owlPFState_owlTree next_pFState))) <> "\n\n\n" <> show next_attachmentMap
    else
      (_goatCmdTempOutput_goatState goatCmdTempOutput) {
        _goatState_workspace      = next_workspace
        , _goatState_pan             = next_pan
        , _goatState_layersHandler  = next_layersHandler
        , _goatState_handler         = next_handler
        , _goatState_selection       = next_selection
        , _goatState_canvasSelection = next_canvasSelection
        , _goatState_broadPhaseState = next_broadPhaseState
        , _goatState_renderedCanvas = next_renderedCanvas
        , _goatState_renderedSelection = next_renderedSelection
        , _goatState_layersState     = next_layersState
        , _goatState_attachmentMap = next_attachmentMap
        , _goatState_renderCache = next_renderCache
      }



-- Endo stuff

endoGoatCmdSetDefaultParams :: SetPotatoDefaultParameters -> GoatState -> GoatState
endoGoatCmdSetDefaultParams spdp gs = gs {
    _goatState_potatoDefaultParameters = potatoDefaultParameters_set (_goatState_potatoDefaultParameters gs) spdp
  }

endoGoatCmdMarkSaved :: () -> GoatState -> GoatState
endoGoatCmdMarkSaved _ gs = gs {
    _goatState_workspace = markWorkspaceSaved (_goatState_workspace gs)
  }
