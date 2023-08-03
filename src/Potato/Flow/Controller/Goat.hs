{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.Goat (
  GoatFocusedArea(..)
  , goatState_hasUnsavedChanges
  , makeGoatState
  , goatState_pFState
  , goatState_selectedTool
  , GoatState(..)

  -- endo style
  , endoGoatCmdSetDefaultParams
  , endoGoatCmdMarkSaved
  , endoGoatCmdSetTool 
  , endoGoatCmdSetDebugLabel
  , endoGoatCmdSetCanvasRegionDim
  , endoGoatCmdWSEvent
  , endoGoatCmdNewFolder 
  , endoGoatCmdLoad
  , endoGoatCmdSetFocusedArea
  , endoGoatCmdMouse
  , endoGoatCmdKeyboard

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
import  Potato.Flow.Preview 
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

    -- TODO you broke this with endo refactor
    -- TODO this isn't even used right now... DELETE ME
    , _goatState_unbrokenInput       :: Text -- grapheme clusters are inputed as several keyboard character events so we track these inputs here

    -- debug stuff (shared across documents)
    , _goatState_debugLabel              :: Text

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
  -- TODO instead, just check if there is a preview operation or not
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






endoGoatCmdSetDefaultParams :: SetPotatoDefaultParameters -> GoatState -> GoatState
endoGoatCmdSetDefaultParams spdp gs = gs {
    _goatState_potatoDefaultParameters = potatoDefaultParameters_set (_goatState_potatoDefaultParameters gs) spdp
  }

endoGoatCmdMarkSaved :: () -> GoatState -> GoatState
endoGoatCmdMarkSaved _ gs = gs {
    _goatState_workspace = markWorkspaceSaved (_goatState_workspace gs)
  }

-- TODO do I need to do anything else after setting handler??
endoGoatCmdSetTool :: Tool -> GoatState -> GoatState
endoGoatCmdSetTool tool gs = gs {
    _goatState_handler = makeHandlerFromNewTool gs tool
  }

endoGoatCmdSetDebugLabel :: Text -> GoatState -> GoatState
endoGoatCmdSetDebugLabel x gs = gs {
    _goatState_debugLabel = x
  }

endoGoatCmdSetCanvasRegionDim :: V2 Int -> GoatState -> GoatState
endoGoatCmdSetCanvasRegionDim x gs = r where
  -- set the screen region
  -- rerender
  gs_1 = gs {
      _goatState_screenRegion = x
    }
  r = goat_setPan (_goatState_pan gs_1) gs_1


endoGoatCmdWSEvent :: WSEvent -> GoatState -> GoatState
endoGoatCmdWSEvent wsev gs = goat_applyWSEvent WSEventType_Local_Refresh wsev gs

endoGoatCmdNewFolder :: Text -> GoatState -> GoatState
endoGoatCmdNewFolder x gs = goat_applyWSEvent WSEventType_Local_Refresh newFolderEv gs where
  pfs = goatState_pFState gs
  folderPos = lastPositionInSelection (_owlPFState_owlTree pfs) (_goatState_selection gs)
  newFolderEv = WSEApplyLlama (False, makeAddFolderLlama pfs (folderPos, x))

endoGoatCmdLoad :: (SPotatoFlow, ControllerMeta) -> GoatState -> GoatState
endoGoatCmdLoad (spf, cm) gs = r where
  gs' = goat_applyWSEvent WSEventType_Local_Refresh (WSELoad spf) gs
  r = gs' {
      _goatState_pan = _controllerMeta_pan cm
      , _goatState_layersState = makeLayersStateFromOwlPFState (goatState_pFState gs') (_controllerMeta_layers cm)
      -- NOTE _goatState_layersHandler gets set by goat_applyWSEvent during refresh
    }

endoGoatCmdSetFocusedArea :: GoatFocusedArea -> GoatState -> GoatState
endoGoatCmdSetFocusedArea gfa goatState = r where
  didchange = gfa /= _goatState_focusedArea goatState
  goatstatewithnewfocus = goatState { _goatState_focusedArea = gfa }
  noactionneeded = goatstatewithnewfocus
  potatoHandlerInput = potatoHandlerInputFromGoatState goatState
  r = if didchange && pHandlerName (_goatState_layersHandler goatState) == handlerName_layersRename
    then let
        goatState_afterAction = case pHandleKeyboard (_goatState_layersHandler goatState) potatoHandlerInput (KeyboardData KeyboardKey_Return []) of
          Nothing -> noactionneeded
          Just pho -> goat_processLayersHandlerOutput pho goatstatewithnewfocus
      in assert (_goatState_focusedArea goatState == GoatFocusedArea_Layers) $ goatState_afterAction  
    else noactionneeded


endoGoatCmdMouse :: LMouseData -> GoatState -> GoatState
endoGoatCmdMouse mouseData goatState = trace ("endoGoatCmdMouse " <> show (_goatState_handler goatState)) $ r where
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
  noChangeOutput = goatState_withNewMouse
  -- TODO maybe split this case out to endoGoatCmdLayerMouse
  isLayerMouse = _mouseDrag_isLayerMouse mouseDrag

  potatoHandlerInput = potatoHandlerInputFromGoatState goatState_withNewMouse
  last_pFState = goatState_pFState goatState_withNewMouse
  handler = _goatState_handler goatState_withNewMouse


  r = case _mouseDrag_state mouseDrag of

    -- TODO soft failure
    _ | mouseSourceFailure -> error "invalid mouse sequence due to source"

    -- TODO assert false here? I'm pretty sure this should never happen
    -- if mouse was cancelled, update _goatState_mouseDrag accordingly
    MouseDragState_Cancelled -> if _lMouseData_isRelease mouseData
      then goatState_withNewMouse { _goatState_mouseDrag = def }
      else noChangeOutput -- still cancelled

    -- if mouse is intended for layers
    _ | isLayerMouse -> case pHandleMouse (_goatState_layersHandler goatState) potatoHandlerInput (RelMouseDrag mouseDrag) of
      Just pho -> goat_processLayersHandlerOutput pho goatState_withNewMouse
      Nothing  -> noChangeOutput

    -- if middle mouse button, create a temporary PanHandler
    MouseDragState_Down | _lMouseData_button mouseData == MouseButton_Middle -> r where
      panhandler = def { _panHandler_maybePrevHandler = Just (SomePotatoHandler handler) }
      r = case pHandleMouse panhandler potatoHandlerInput canvasDrag of
        Just pho -> goat_processCanvasHandlerOutput pho goatState_withNewMouse
        Nothing -> error "PanHandler expected to capture mouse input"

    -- pass onto canvas handler
    _ -> case pHandleMouse handler potatoHandlerInput canvasDrag of
      Just pho -> goat_processCanvasHandlerOutput pho goatState_withNewMouse

      -- input not captured by handler, pass onto select or select+drag
      Nothing | _mouseDrag_state mouseDrag == MouseDragState_Down -> assert (not $ pIsHandlerActive handler) r where
        r = case pHandleMouse (def :: SelectHandler) potatoHandlerInput canvasDrag of
          Just pho -> goat_processCanvasHandlerOutput pho goatState_withNewMouse
          Nothing -> error "handler was expected to capture this mouse state"

      Nothing -> error $ "handler " <> show (pHandlerName handler) <> "was expected to capture mouse state " <> show (_mouseDrag_state mouseDrag)




endoGoatCmdKeyboard :: KeyboardData -> GoatState -> GoatState
endoGoatCmdKeyboard kbd' goatState = r where
  -- TODO you need to do reset logic for this (basically, reset it anytime there was a non-keyboard event)
  last_unbrokenInput = _goatState_unbrokenInput goatState
  next_unbrokenInput = case kbd' of
    KeyboardData (KeyboardKey_Char c) _ -> T.snoc last_unbrokenInput c
    _ -> ""
  mkbd =   potatoModifyKeyboardKey (_goatState_configuration goatState) last_unbrokenInput kbd'
  goatState_withKeyboard =  goatState { _goatState_unbrokenInput = next_unbrokenInput}
  potatoHandlerInput = potatoHandlerInputFromGoatState goatState_withKeyboard
  last_pFState = goatState_pFState goatState_withKeyboard
  -- TODO rename to canvasHandler
  handler = _goatState_handler goatState_withKeyboard

  r = case mkbd of
    Nothing -> goatState_withKeyboard
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
          Just pho -> goat_processLayersHandlerOutput pho goatState_withNewMouse
          -- TODO I think this is fine, but maybe you should you clear the handler instead?
          Nothing  -> goatState_withNewMouse
        else case pHandleMouse handler potatoHandlerInput (toRelMouseDrag last_pFState (_goatState_pan goatState_withKeyboard) canceledMouse) of
          Just pho -> goat_processCanvasHandlerOutput pho goatState_withNewMouse
          -- TODO I think this is fine, but maybe you should you clear the handler instead?
          Nothing  -> goatState_withNewMouse

    -- we are in the middle of mouse drag, ignore all keyboard inputs
    -- perhaps a better way to do this is to have handlers capture all inputs when active
    Just _ | mouseDrag_isActive (_goatState_mouseDrag goatState_withKeyboard) -> goatState_withKeyboard

    Just kbd ->
      let
        maybeHandleLayers = do
          guard $ _mouseDrag_isLayerMouse (_goatState_mouseDrag goatState_withKeyboard)
          pho <- pHandleKeyboard (_goatState_layersHandler goatState_withKeyboard) potatoHandlerInput kbd
          return $ goat_processLayersHandlerOutput pho goatState_withKeyboard
      in case maybeHandleLayers of
        Just x -> x
        Nothing -> case pHandleKeyboard handler potatoHandlerInput kbd of
          Just pho -> goat_processCanvasHandlerOutput pho goatState_withKeyboard
          -- input not captured by handler
          -- TODO consider wrapping this all up in KeyboardHandler or something? Unfortunately, copy needs to modify goatState_withKeyboard which PotatoHandlerOutput can't atm
          Nothing -> case kbd of
            KeyboardData KeyboardKey_Esc _ -> case _mouseDrag_state (_goatState_mouseDrag goatState_withKeyboard) of
                    x | x == MouseDragState_Up || x == MouseDragState_Cancelled -> goat_setSelection False isParliament_empty goatState_withKeyboard
                    _                        -> goatState_withKeyboard

            KeyboardData k [] | k == KeyboardKey_Delete || k == KeyboardKey_Backspace -> case deleteSelectionEvent goatState_withKeyboard of
              Nothing -> goatState_withKeyboard
              Just wsev -> goat_applyWSEvent WSEventType_Local_Refresh wsev goatState_withKeyboard
            KeyboardData (KeyboardKey_Char 'c') [KeyModifier_Ctrl] -> r where
              copied = makeClipboard goatState_withKeyboard
              r = goatState_withKeyboard { _goatState_clipboard = copied }
            KeyboardData (KeyboardKey_Char 'x') [KeyModifier_Ctrl] -> r where
              copied = makeClipboard goatState_withKeyboard
              goatState_withClipboard = goatState_withKeyboard { _goatState_clipboard = copied }
              r = case deleteSelectionEvent goatState_withKeyboard of
                Nothing -> goatState_withClipboard
                Just wsev -> goat_applyWSEvent WSEventType_Local_Refresh wsev goatState_withClipboard
            KeyboardData (KeyboardKey_Char 'v') [KeyModifier_Ctrl] -> case _goatState_clipboard goatState_withKeyboard of
              Nothing    -> goatState_withKeyboard
              Just stree -> r where
                offsetstree = offsetSEltTree (V2 1 1) stree
                minitree' = owlTree_fromSEltTree offsetstree
                -- reindex the tree so there are no collisions with the current state
                maxid1 = owlTree_maxId minitree' + 1
                maxid2 = owlPFState_nextId (_owlPFWorkspace_owlPFState (_goatState_workspace goatState_withKeyboard))
                minitree = owlTree_reindex (max maxid1 maxid2) minitree'
                spot = lastPositionInSelection (goatState_owlTree goatState_withKeyboard) (_goatState_selection goatState_withKeyboard)
                treePastaEv = WSEApplyLlama (False, makePFCLlama $ OwlPFCNewTree (minitree, spot))
                r = goat_applyWSEvent WSEventType_Local_Refresh treePastaEv (goatState_withKeyboard { _goatState_clipboard = Just offsetstree })
            KeyboardData (KeyboardKey_Char 'z') [KeyModifier_Ctrl] -> r where
              r = goat_applyWSEvent WSEventType_Local_Refresh WSEUndo goatState_withKeyboard
            KeyboardData (KeyboardKey_Char 'y') [KeyModifier_Ctrl] -> r where
              r = goat_applyWSEvent WSEventType_Local_Refresh WSERedo goatState_withKeyboard
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
              -- TODO should I call a goat_setHandler function instead?
              r = goatState_withKeyboard { _goatState_handler = newHandler }
            _ -> goatState_withKeyboard


goat_renderCanvas_move :: RenderContext -> XY -> XY -> (RenderContext, Bool)
goat_renderCanvas_move rc@RenderContext {..} pan sr = r where
  newBox = LBox (-pan) sr
  didScreenRegionMove = _renderedCanvasRegion_box _renderContext_renderedCanvasRegion /= newBox
  r = if didScreenRegionMove
    then (moveRenderedCanvasRegion newBox rc, True)
    else (rc, False)


goat_renderCanvas_update :: (HasCallStack) => RenderContext -> NeedsUpdateSet -> SuperOwlChanges -> RenderContext
goat_renderCanvas_update rc needsupdateaabbs cslmap = r where
  r = if IM.null cslmap
    then rc
    else updateCanvas cslmap needsupdateaabbs rc


-- USAGE this function is a little sneaky, we pass in the canvas RenderContext and alter it to use it for selection rendering
-- So you want to pull out the _renderContext_renderedCanvasRegion but throw away the rest of the render context from the output
-- I guess you also want to pull out _renderContext_cache?
-- In the future, selection rendering will have its own context and you won't want to do this I guess?
goat_renderCanvas_selection :: RenderContext -> SuperOwlParliament -> RenderContext
goat_renderCanvas_selection rc_from_canvas next_selection = r where
  newBox = _renderedCanvasRegion_box $ _renderContext_renderedCanvasRegion rc_from_canvas
  rendercontext_forSelection = rc_from_canvas {
      -- NOTE this will render hidden stuff that's selected via layers!!
      _renderContext_layerMetaMap = IM.empty
      -- empty canvas to render our selection in, we just re-render everything for now (in the future you can try and do partial rendering though)
      , _renderContext_renderedCanvasRegion = emptyRenderedCanvasRegion newBox
    }
  selectionselts = toList . fmap _superOwl_id $ unSuperOwlParliament next_selection
  r = render_new newBox selectionselts rendercontext_forSelection

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

renderContextFromGoatState :: GoatState -> RenderContext
renderContextFromGoatState goatState = RenderContext {
    _renderContext_cache = _goatState_renderCache goatState
    , _renderContext_owlTree = _owlPFState_owlTree (goatState_pFState goatState)
    , _renderContext_layerMetaMap = _layersState_meta (_goatState_layersState goatState)
    , _renderContext_broadPhase = _goatState_broadPhaseState goatState
    , _renderContext_renderedCanvasRegion = _goatState_renderedCanvas goatState
  }

goat_setPan :: XY -> GoatState -> GoatState
goat_setPan (V2 dx dy) goatState = r where
  -- set the pan
  -- render move
  -- render selection
  V2 cx0 cy0 = _goatState_pan goatState
  next_pan = V2 (cx0+dx) (cy0 + dy) 

  rc = renderContextFromGoatState goatState
  (rc_aftermove, _) = goat_renderCanvas_move rc next_pan (_goatState_screenRegion goatState)
  rc_afterselection = goat_renderCanvas_selection rc_aftermove (_goatState_selection goatState)
  r = goatState {
      _goatState_pan = next_pan
      , _goatState_renderedCanvas = _renderContext_renderedCanvasRegion rc_aftermove
      , _goatState_renderedSelection = _renderContext_renderedCanvasRegion rc_afterselection
    }



computeCanvasSelection :: GoatState -> CanvasSelection
computeCanvasSelection goatState = r where
  pfs = goatState_pFState goatState
  filterHiddenOrLocked sowl = not $ layerMetaMap_isInheritHiddenOrLocked (_owlPFState_owlTree pfs) (_superOwl_id sowl) (_layersState_meta (_goatState_layersState goatState))
  r = superOwlParliament_convertToCanvasSelection (_owlPFState_owlTree pfs) filterHiddenOrLocked (_goatState_selection goatState)


goat_autoExpandFoldersOfSelection :: GoatState -> GoatState
goat_autoExpandFoldersOfSelection goatState = r where
  -- auto expand folders for selected elements + (this will also auto expand when you drag or paste stuff into a folder)
  -- NOTE this will prevent you from ever collapsing a folder that has a selected child in it (that's not true, you can still collapse it but it will rexpand the moment you make any changes, which might be kind of buggy)
  -- so maybe auto expand should only happen on newly created elements or add a way to detect for newly selected elements (e.g. diff between old selection)
  next_layersState = expandAllCollapsedParents (_goatState_selection goatState) (goatState_pFState goatState) (_goatState_layersState goatState)
  r = goatState { _goatState_layersState = next_layersState }


goat_setSelection :: Bool -> SuperOwlParliament -> GoatState -> GoatState
goat_setSelection add selection goatState = r where

  -- set the new selection
  ot = hasOwlTree_owlTree . goatState_pFState $ goatState
  next_selection = SuperOwlParliament . Seq.sortBy (owlTree_superOwl_comparePosition ot) . unSuperOwlParliament $ if add
    then superOwlParliament_disjointUnionAndCorrect ot (_goatState_selection goatState) selection
    else selection
  goatState_afterSelection = goat_autoExpandFoldersOfSelection goatState { _goatState_selection = next_selection }

  -- set the new canvas selection
  -- create new handler as appropriate
  -- rerender selection
  next_canvasSelection = computeCanvasSelection goatState_afterSelection
  next_handler = maybeUpdateHandlerFromSelection (_goatState_handler goatState_afterSelection) next_canvasSelection
  -- MAYBE TODO consider rendering selected hidden/locked stuff too (it's still possible to select them via layers)? 
  -- Except we removed it from the BroadPhase already. And it would be weird because you bulk selected you would edit only the non-hidden/locked stuff
  rc_afterselection = goat_renderCanvas_selection (renderContextFromGoatState goatState_afterSelection) (SuperOwlParliament $ unCanvasSelection next_canvasSelection)
  r = goatState_afterSelection {
      _goatState_handler = next_handler
      , _goatState_renderedSelection = _renderContext_renderedCanvasRegion rc_afterselection
      , _goatState_canvasSelection = next_canvasSelection
    }



goat_setLayersStateWithChangesFromToggleHide :: LayersState -> SuperOwlChanges -> GoatState -> GoatState
goat_setLayersStateWithChangesFromToggleHide ls changes goatState = r where
  
  -- set the broadphase
  (needsupdateaabbs, next_broadPhaseState) = update_bPTree (goatState_pFState goatState) changes (_broadPhaseState_bPTree (_goatState_broadPhaseState goatState))
  goatState_afterUpdateBroadPhase = goatState { _goatState_broadPhaseState = next_broadPhaseState }

  -- set layers state  
  -- render changes
  -- render selection
  rc = renderContextFromGoatState goatState_afterUpdateBroadPhase
  rc_afterupdate = goat_renderCanvas_update rc needsupdateaabbs changes
  rc_afterselection = goat_renderCanvas_selection rc_afterupdate (_goatState_selection goatState_afterUpdateBroadPhase)
  r = goatState_afterUpdateBroadPhase {
      _goatState_layersState = ls
      -- MAYBE TODO refresh the layers handler (), this might be relevant if we support shared lock/hide state of layers in the future
      --, _goatState_layersHandler = fromMaybe (SomePotatoHandler (def :: LayersHandler)) ....
      , _goatState_renderedCanvas = _renderContext_renderedCanvasRegion rc_afterupdate
      , _goatState_renderedSelection = _renderContext_renderedCanvasRegion rc_afterselection
    }


goat_processHandlerOutput_noSetHandler :: PotatoHandlerOutput -> GoatState -> GoatState
goat_processHandlerOutput_noSetHandler pho goatState = trace ("goat_processHandlerOutput_noSetHandler HANDLER OUTPUT: " <> show pho <> " CURRENT HANDLER: " <> show (_goatState_handler goatState)) $ r where
  needsUndoFirst po = case po of
    PO_Start          -> False
    PO_StartAndCommit -> False
    _                 -> True

  r = case _potatoHandlerOutput_action pho of
    HOA_Select x y -> goat_setSelection x y goatState
    HOA_Pan x -> goat_setPan x goatState
    HOA_Layers x y -> goat_setLayersStateWithChangesFromToggleHide x y goatState


    -- TODO this is bugged, you need to regenerate the handler if the handler returned Nothing
    -- TODO set preview stack
    HOA_Preview (Preview po x) -> goat_applyWSEventNoResetHandler WSEventType_Local_NoRefresh (WSEApplyLlama (needsUndoFirst po, x)) goatState
    HOA_Preview Preview_Cancel -> goat_applyWSEventNoResetHandler WSEventType_Local_NoRefresh WSEUndo goatState
    HOA_Preview Preview_Commit -> goatState
    HOA_Nothing -> goatState


goat_processLayersHandlerOutput :: PotatoHandlerOutput -> GoatState -> GoatState
goat_processLayersHandlerOutput pho goatState = goat_processHandlerOutput_noSetHandler pho $ goatState { _goatState_layersHandler = fromMaybe (_goatState_layersHandler goatState) (_potatoHandlerOutput_nextHandler pho) }

goat_processCanvasHandlerOutput :: PotatoHandlerOutput -> GoatState -> GoatState
goat_processCanvasHandlerOutput pho goatState = r where
  canvasSelection = computeCanvasSelection goatState
  nextHandler = fromMaybe (makeHandlerFromSelection canvasSelection) (_potatoHandlerOutput_nextHandler pho)
  goatState' = goatState { _goatState_handler = nextHandler }
  r = goat_processHandlerOutput_noSetHandler pho $ goatState'


data WSEventType = WSEventType_Local_NoRefresh | WSEventType_Local_Refresh | WSEventType_Remote_Refresh deriving (Eq, Show)

wSEventType_isRemote :: WSEventType -> Bool
wSEventType_isRemote WSEventType_Remote_Refresh = True
wSEventType_isRemote _ = False

wSEventType_needsRefresh :: WSEventType -> Bool
wSEventType_needsRefresh WSEventType_Local_Refresh = True
wSEventType_needsRefresh WSEventType_Remote_Refresh = True
wSEventType_needsRefresh _ = False

goat_applyWSEvent :: WSEventType -> WSEvent -> GoatState -> GoatState
goat_applyWSEvent = goat_applyWSEvent' True


-- TODO DELETE ME, no need to pass in resetHandlerIfInactive, you can just always reset the handler once you properly do handler active stuff
goat_applyWSEventNoResetHandler :: WSEventType -> WSEvent -> GoatState -> GoatState
goat_applyWSEventNoResetHandler = goat_applyWSEvent' False


goat_applyWSEvent' :: Bool -> WSEventType -> WSEvent -> GoatState -> GoatState
goat_applyWSEvent' resetHandlerIfInactive wsetype wse goatState = goatState_final where

  -- apply the event
  last_pFState = goatState_pFState goatState
  (workspace_afterEvent, cslmap_afterEvent) = updateOwlPFWorkspace wse (_goatState_workspace goatState)
  goatState_afterEvent = goatState { _goatState_workspace = workspace_afterEvent }
  pFState_afterEvent = _owlPFWorkspace_owlPFState workspace_afterEvent


  -- compute selection based on changes from updating OwlPFState (i.e. auto select newly created stuff if appropriate)
  (isNewSelection, next_selection) = if IM.null cslmap_afterEvent
    then (False, _goatState_selection goatState_afterEvent)
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

      wasLoad = case wse of
        WSELoad _ -> True
        _             -> False

      -- TODO add `|| wasRemoteChange` condition
      r = if wasLoad || null newlyCreatedSEltls || wSEventType_isRemote wsetype
        -- if there are no newly created elts, we still need to update the selection
        then (\x -> (False, SuperOwlParliament x)) $ catMaybesSeq . flip fmap (unSuperOwlParliament (_goatState_selection goatState_afterEvent)) $ \sowl ->
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
  goatState_afterSelection = goatState_afterEvent { _goatState_selection = next_selection }

  -- | refresh the handler if there was a non-canvas or non-local state change |
  goatState_afterRefreshHandler = if wSEventType_needsRefresh wsetype
    then let
        layersHandler = _goatState_layersHandler goatState_afterSelection
        canvasHandler = _goatState_handler goatState_afterSelection 
        -- TODO remove this assert, this will happen for stuff like boxtexthandler
        -- since we don't have multi-user events, the handler should never be active when this happens
        checkvalid = assert (not (pIsHandlerActive canvasHandler) && not (pIsHandlerActive layersHandler))

        -- safe for now, since `potatoHandlerInputFromGoatState` does not use `_goatState_handler/_goatState_layersHandler finalGoatState` which is set to `next_handler/next_layersHandler`
        next_potatoHandlerInput = potatoHandlerInputFromGoatState goatState_afterSelection
        canvasSelection = computeCanvasSelection goatState_afterSelection
        refreshedCanvasHandler = fromMaybe (makeHandlerFromSelection canvasSelection) ( pRefreshHandler canvasHandler next_potatoHandlerInput)
        refreshedLayersHandler = fromMaybe (SomePotatoHandler (def :: LayersHandler)) (pRefreshHandler layersHandler next_potatoHandlerInput)

        -- TODO
        -- TODO pRefreshHandler needs to return an output action and the only possible output is HOA_Preview PreviewCancel (or HOA_Nothing for the layers handler)
        -- TODO
        
      in checkvalid goatState_afterSelection {
          _goatState_handler = refreshedCanvasHandler
          , _goatState_layersHandler = refreshedLayersHandler
        }
    else goatState_afterSelection

  -- | update LayersState based from SuperOwlChanges after applying events |
  next_layersState' = updateLayers pFState_afterEvent cslmap_afterEvent (_goatState_layersState goatState_afterRefreshHandler)
  goatState_afterSetLayersState = goat_autoExpandFoldersOfSelection $ goatState_afterRefreshHandler { _goatState_layersState = next_layersState' }

  -- | set the new handler based on the new Selection and LayersState
  next_canvasSelection = computeCanvasSelection goatState_afterSetLayersState -- (TODO pretty sure this is the same as `canvasSelection = computeCanvasSelection goatState_afterSelection` above..)

  -- TODO currently we use to only reset the handler here if the prev handler returned Nothing (did not capture input) using `resetHandlerIfInactive` to pipe this info in
  -- instead, we can just always reset it if the handler is inactive once we properly do handler active stuff
  next_handler = if resetHandlerIfInactive then maybeUpdateHandlerFromSelection (_goatState_handler goatState_afterSetLayersState) next_canvasSelection else _goatState_handler goatState_afterSetLayersState
  goatState_afterSetHandler = goatState_afterSetLayersState {
      _goatState_handler = next_handler
      , _goatState_canvasSelection = next_canvasSelection
    }

  -- | update AttachmentMap based on new state and clear the cache on these changes |
  (needsupdateaabbs, goatState_afterUpdateAttachmentsAndRenderState) = goat_updateAttachmentsAndRenderStateFromChanges cslmap_afterEvent goatState_afterSetHandler

  -- | render everything
  rc = renderContextFromGoatState goatState_afterUpdateAttachmentsAndRenderState
  rc_afterRenderCanvas = goat_renderCanvas_update rc needsupdateaabbs cslmap_afterEvent
  rc_afterRenderSelection = goat_renderCanvas_selection rc (_goatState_selection goatState_afterUpdateAttachmentsAndRenderState)

  -- | DONE
  goatState_final = goatState_afterUpdateAttachmentsAndRenderState {
      _goatState_renderedCanvas = _renderContext_renderedCanvasRegion rc_afterRenderCanvas
      , _goatState_renderedSelection = _renderContext_renderedCanvasRegion rc_afterRenderSelection
    }


-- this one also updates attachment map based on changes
goat_updateAttachmentsAndRenderStateFromChanges :: SuperOwlChanges -> GoatState -> ([AABB], GoatState)
goat_updateAttachmentsAndRenderStateFromChanges cslmap_afterEvent goatState = r where

  pFState_afterEvent = goatState_pFState goatState

  -- | update AttachmentMap based on new state and clear the cache on these changes |
  next_attachmentMap = updateAttachmentMapFromSuperOwlChanges cslmap_afterEvent (_goatState_attachmentMap goatState)
  -- we need to union with `_goatState_attachmentMap` as next_attachmentMap does not contain deleted targets and stuff we detached from
  attachmentMapForComputingChanges = IM.unionWith IS.union next_attachmentMap (_goatState_attachmentMap goatState)
  --attachmentChanges = trace "ATTACHMENTS" $ traceShow (IM.size cslmap_afterEvent) $ traceShowId $ getChangesFromAttachmentMap (_owlPFState_owlTree pFState_afterEvent) attachmentMapForComputingChanges cslmap_afterEvent
  attachmentChanges = getChangesFromAttachmentMap (_owlPFState_owlTree pFState_afterEvent) attachmentMapForComputingChanges cslmap_afterEvent

  -- | compute SuperOwlChanges for rendering |
  cslmap_withAttachments = IM.union cslmap_afterEvent attachmentChanges

  -- | clear the cache at places that have changed
  renderCache_resetOnChangesAndAttachments = renderCache_clearAtKeys (_goatState_renderCache goatState) (IM.keys cslmap_withAttachments)

  -- | update the BroadPhase
  (needsupdateaabbs, next_broadPhaseState) = update_bPTree (_owlPFState_owlTree pFState_afterEvent) cslmap_withAttachments (_broadPhaseState_bPTree (_goatState_broadPhaseState goatState))

  r = (needsupdateaabbs, goatState {
      _goatState_attachmentMap = next_attachmentMap
      , _goatState_broadPhaseState = next_broadPhaseState
      , _goatState_renderCache = renderCache_resetOnChangesAndAttachments
    })

