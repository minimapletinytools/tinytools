{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.Goat (
  goatState_pFState
  , GoatState(..)
  , GoatCmd(..)
  , foldGoatFn

  -- exposed for testing
  , potatoHandlerInputFromGoatState
) where

import           Relude

import           Potato.Flow.BroadPhase
import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.OwlLayers
import           Potato.Flow.Controller.Manipulator.Box
import           Potato.Flow.Controller.Manipulator.Common
import           Potato.Flow.Controller.Manipulator.Layers
import           Potato.Flow.Controller.Manipulator.Line
import           Potato.Flow.Controller.Manipulator.CartLine
import           Potato.Flow.Controller.Manipulator.Pan
import           Potato.Flow.Controller.Manipulator.Select
import           Potato.Flow.Controller.Types
import           Potato.Flow.Math
import           Potato.Flow.Render
import           Potato.Flow.SEltMethods
import           Potato.Flow.OwlState
import           Potato.Flow.Owl
import           Potato.Flow.OwlWorkspace
import           Potato.Flow.Types

import           Control.Exception                         (assert)
import           Data.Default
import qualified Data.IntMap                               as IM
import           Data.Maybe
import qualified Data.Sequence                             as Seq


catMaybesSeq :: Seq (Maybe a) -> Seq a
catMaybesSeq = fmap fromJust . Seq.filter isJust


data GoatState = GoatState {

    -- TODO Refactor these out into GoatTab or something like that
    -- TODO consider including handlers (vs regenerating from selection each time you switch tabs)
    _goatState_workspace       :: OwlPFWorkspace
    , _goatState_pan             :: XY -- panPos is position of upper left corner of canvas relative to screen
    , _goatState_selection       :: Selection
    , _goatState_canvasSelection :: CanvasSelection
    , _goatState_broadPhaseState :: BroadPhaseState
    , _goatState_layersState     :: LayersState
    , _goatState_potatoDefaultParameters :: PotatoDefaultParameters

    , _goatState_renderedCanvas  :: RenderedCanvasRegion
    , _goatState_renderedSelection  :: RenderedCanvasRegion -- TODO need sparse variant
    , _goatState_screenRegion    :: XY

    , _goatState_selectedTool    :: Tool
    , _goatState_mouseDrag       :: MouseDrag -- last mouse dragging state, this is a little questionable, arguably we should only store stuff needed, not the entire mouseDrag

    , _goatState_handler         :: SomePotatoHandler
    , _goatState_layersHandler   :: SomePotatoHandler
    , _goatState_clipboard       :: Maybe SEltTree

    , _goatState_debugLabel      :: Text
    , _goatState_debugCammands   :: [GoatCmd]

  } deriving (Show)

goatState_pFState :: GoatState -> OwlPFState
goatState_pFState = _owlPFWorkspace_pFState . _goatState_workspace

goatState_owlTree :: GoatState -> OwlTree
goatState_owlTree = _owlPFState_owlTree . goatState_pFState

-- TODO deprecate this in favor of Endo style
data GoatCmd =
  GoatCmdTool Tool
  | GoatCmdLoad EverythingLoadState

  -- command based input
  | GoatCmdWSEvent WSEvent
  | GoatCmdSetCanvasRegionDim XY
  | GoatCmdNewFolder

  -- canvas direct input
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


data GoatCmdTempOutput = GoatCmdTempOutput {
  _goatCmdTempOutput_goatState     :: GoatState
  --, _goatCmdTempOutput_wasCanvasInput :: Bool
  --, _goatCmdTempOutput_wasLayerInput :: Bool

  -- NOTE the value of _potatoHandlerOutput_nextHandler is not directly translated here
  -- PotatoHandlerOutput interpretation: isNothing _potatoHandlerOutput_nextHandler => handler does not capture input
  -- GoatCmdTempOutput interpretation (when non-canvas input):
  --    -isNothing _potatoHandlerOutput_nextHandler => event not related to canvas handler
  --    -so we set _goatCmdTempOutput_nextHandler = Just _goatState_handler
  , _goatCmdTempOutput_nextHandler :: Maybe SomePotatoHandler

  , _goatCmdTempOutput_select      :: Maybe (Bool, Selection)
  , _goatCmdTempOutput_pFEvent     :: Maybe (Bool, WSEvent) -- bool is true if it was a canvas handler event
  , _goatCmdTempOutput_pan         :: Maybe XY
  , _goatCmdTempOutput_layersState :: Maybe LayersState
  , _goatCmdTempOutput_changesFromToggleHide :: SuperOwlChanges
} deriving (Show)

-- helpers to extract stuff out of goatState because we use record wildcards and can't access otherwise
goatCmdTempOutput_screenRegion :: GoatCmdTempOutput -> XY
goatCmdTempOutput_screenRegion = _goatState_screenRegion . _goatCmdTempOutput_goatState


instance Default GoatCmdTempOutput where
  def = GoatCmdTempOutput {
      _goatCmdTempOutput_goatState = undefined
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
    , _goatCmdTempOutput_nextHandler = Just (_goatState_handler goatState)
  }

makeGoatCmdTempOutputFromNothingClearHandler :: GoatState -> GoatCmdTempOutput
makeGoatCmdTempOutputFromNothingClearHandler goatState = def {
    _goatCmdTempOutput_goatState = goatState
  }

makeGoatCmdTempOutputFromEvent :: GoatState -> WSEvent -> GoatCmdTempOutput
makeGoatCmdTempOutputFromEvent goatState wsev = (makeGoatCmdTempOutputFromNothing goatState) {
    _goatCmdTempOutput_pFEvent = Just (False, wsev)
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

-- | hack function for resetting both handlers
-- It would be nice if we actually cancel/reset the handlers (such that in progress operations are undone), but I don't think it really matters
forceResetBothHandlersAndMakeGoatCmdTempOutput :: (PotatoHandler h, PotatoHandler h') => GoatState -> PotatoHandlerInput -> h -> h' -> GoatCmdTempOutput
forceResetBothHandlersAndMakeGoatCmdTempOutput goatState potatoHandlerInput h lh = r where

  -- TODO this is incorrect, we don't want to call pRefreshHandler as it does not correctly reset the handler state
  --msph_h = pRefreshHandler h potatoHandlerInput
  -- lh is layers handler!
  --msph_lh = pRefreshHandler lh potatoHandlerInput

  -- I think this is Ok
  msph_h = Nothing
  msph_lh = Just (SomePotatoHandler (def :: LayersHandler))

  r = def {
      _goatCmdTempOutput_goatState = goatState {
          _goatState_layersHandler = case msph_lh of
            Just h  -> h
            Nothing -> error "expected LayersHandler to return a new handler"
        }
      , _goatCmdTempOutput_nextHandler = msph_h
    }

-- TODO rename to makeHandlerFromCanvasSelection
makeHandlerFromSelection :: CanvasSelection -> SomePotatoHandler
makeHandlerFromSelection selection = case computeSelectionType selection of
  SMTBox         -> SomePotatoHandler $ (def :: BoxHandler)
  SMTBoxText     -> SomePotatoHandler $ (def :: BoxHandler)
  SMTLine        -> SomePotatoHandler $ (def :: SimpleLineHandler)
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

deleteSelectionEvent :: GoatState -> Maybe WSEvent
deleteSelectionEvent GoatState {..} = if isParliament_null _goatState_selection
  then Nothing
  else Just $ WSERemoveElt (superOwlParliament_toOwlParliament _goatState_selection)

potatoHandlerInputFromGoatState :: GoatState -> PotatoHandlerInput
potatoHandlerInputFromGoatState GoatState {..} = r where
  last_workspace = _goatState_workspace
  last_pFState = _owlPFWorkspace_pFState last_workspace
  r = PotatoHandlerInput {
    _potatoHandlerInput_pFState       = last_pFState
    , _potatoHandlerInput_potatoDefaultParameters = _goatState_potatoDefaultParameters
    , _potatoHandlerInput_broadPhase  = _goatState_broadPhaseState
    , _potatoHandlerInput_tool = _goatState_selectedTool

    , _potatoHandlerInput_layersState     = _goatState_layersState
    , _potatoHandlerInput_selection   = _goatState_selection
    , _potatoHandlerInput_canvasSelection = _goatState_canvasSelection
  }


-- TODO probably should have done "Endo GoatState" instead of "GoatCmd"
-- TODO extract this method into another file
-- TODO make State monad for this
foldGoatFn :: GoatCmd -> GoatState -> GoatState
--foldGoatFn cmd goatState@GoatState {..} = trace ("FOLDING " <> show cmd) $ finalGoatState where
foldGoatFn cmd goatStateIgnore@GoatState {..} = finalGoatState where

  -- TODO do some sort of rolling buffer here prob
  goatState = goatStateIgnore { _goatState_debugCammands = cmd:_goatState_debugCammands }
  --goatState = goatStateIgnore

  last_workspace = _goatState_workspace
  last_pFState = _owlPFWorkspace_pFState last_workspace
  last_layerMetaMap = _layersState_meta _goatState_layersState

  potatoHandlerInput = potatoHandlerInputFromGoatState goatState

  goatCmdTempOutput = case _goatState_handler of
    SomePotatoHandler handler -> case cmd of
      --GoatCmdSetDebugLabel x -> traceShow x $ makeGoatCmdTempOutputFromNothing (goatState { _goatState_debugLabel = x })
      GoatCmdSetDebugLabel x -> makeGoatCmdTempOutputFromNothing $ goatState { _goatState_debugLabel = x }
      GoatCmdSetCanvasRegionDim x -> makeGoatCmdTempOutputFromNothing $ goatState { _goatState_screenRegion = x }
      GoatCmdTool x -> makeGoatCmdTempOutputFromNothing $ goatState { _goatState_selectedTool = x }
      GoatCmdWSEvent x ->  makeGoatCmdTempOutputFromEvent goatState x
      GoatCmdNewFolder -> makeGoatCmdTempOutputFromEvent goatState newFolderEv where
        folderPos = lastPositionInSelection (_owlPFState_owlTree . _owlPFWorkspace_pFState $  _goatState_workspace) _goatState_selection
        newFolderEv = WSEAddFolder (folderPos, "folder")

      GoatCmdLoad (spf, cm) -> r where
        -- HACK this won't get generated until later but we need this to generate layersState...
        -- someday we'll split up foldGoatFn by `GoatCmd` (or switch to Endo `GoatState`) and clean this up
        tempOwlPFStateHack = sPotatoFlow_to_owlPFState spf
        r = (makeGoatCmdTempOutputFromEvent goatState (WSELoad spf)) {
            _goatCmdTempOutput_pan = Just $ _controllerMeta_pan cm
            , _goatCmdTempOutput_layersState = Just $ makeLayersStateFromOwlPFState tempOwlPFStateHack (_controllerMeta_layers cm)
          }
      --GoatCmdMouse mouseData -> traceShow _goatState_handler $ do
      GoatCmdMouse mouseData ->
        let
          sameSource = _mouseDrag_isLayerMouse _goatState_mouseDrag == _lMouseData_isLayerMouse mouseData
          mouseSourceFailure = _mouseDrag_state _goatState_mouseDrag /= MouseDragState_Up && not sameSource
          mouseDrag = case _mouseDrag_state _goatState_mouseDrag of
            MouseDragState_Up        -> newDrag mouseData
            MouseDragState_Cancelled -> (continueDrag mouseData _goatState_mouseDrag) { _mouseDrag_state = MouseDragState_Cancelled }
            _                        ->  continueDrag mouseData _goatState_mouseDrag

          canvasDrag = toRelMouseDrag last_pFState _goatState_pan mouseDrag
          goatState' = goatState { _goatState_mouseDrag = mouseDrag }
          noChangeOutput = makeGoatCmdTempOutputFromNothing goatState'

          isLayerMouse = _mouseDrag_isLayerMouse mouseDrag

        in case _mouseDrag_state mouseDrag of

          -- hack to recover after sameSource issue
          -- TODO TEST
          _ | mouseSourceFailure -> assert False $
            forceResetBothHandlersAndMakeGoatCmdTempOutput goatState' potatoHandlerInput handler _goatState_layersHandler

          -- if mouse was cancelled, update _goatState_mouseDrag accordingly
          MouseDragState_Cancelled -> if _lMouseData_isRelease mouseData
            then makeGoatCmdTempOutputFromNothing $ goatState' { _goatState_mouseDrag = def }
            else noChangeOutput -- still cancelled

          -- if mouse is intended for layers
          _ | isLayerMouse -> case pHandleMouse _goatState_layersHandler potatoHandlerInput (RelMouseDrag mouseDrag) of
            Just pho -> makeGoatCmdTempOutputFromLayersPotatoHandlerOutput goatState' pho
            Nothing  -> noChangeOutput

          -- TODO for non-layers mouse input case, you want to send some event to layers such that we can exit "renaming" mode 🤔

          -- special case, if mouse down and pan tool, we override with a new handler
          MouseDragState_Down | _goatState_selectedTool == Tool_Pan -> case pHandleMouse (def :: PanHandler) potatoHandlerInput canvasDrag of
            Just pho -> makeGoatCmdTempOutputFromPotatoHandlerOutput goatState' pho
            Nothing  -> error "this should never happen"
          -- special case, if mouse down and creation tool, we override with a new handler
          MouseDragState_Down | tool_isCreate _goatState_selectedTool -> assert (not $ pIsHandlerActive handler) r where
            someNewHandler = case _goatState_selectedTool of
              Tool_Box    -> SomePotatoHandler $ def { _boxHandler_creation = BoxCreationType_Box }
              Tool_Line   -> SomePotatoHandler $ def { _simpleLineHandler_isCreation = True }
              Tool_CartLine -> SomePotatoHandler $ def { _cartLineHandler_isCreation = True }
              Tool_Select -> SomePotatoHandler $ (def :: SelectHandler)
              Tool_Text   -> SomePotatoHandler $ def { _boxHandler_creation = BoxCreationType_Text }
              Tool_TextArea -> SomePotatoHandler $ def { _boxHandler_creation = BoxCreationType_TextArea }
              _           -> error "not valid tool"

            -- pass input onto newly created handler
            r = case someNewHandler of
              SomePotatoHandler newHandler -> case pHandleMouse newHandler potatoHandlerInput canvasDrag of
                Just pho -> makeGoatCmdTempOutputFromPotatoHandlerOutput goatState' pho
                Nothing -> error "this should never happen, although if it did, we have many choices to gracefully recover (and I couldn't pick which one so I just did the error thing instead)"


          -- _ -> trace "handler mouse case:\nmouse: " $ traceShow mouseDrag $ trace "prev goatState:" $ traceShow goatState $ trace "handler" $ traceShow _goatState_handler $ case pHandleMouse handler potatoHandlerInput canvasDrag of
          _ -> case pHandleMouse handler potatoHandlerInput canvasDrag of
            Just pho -> r where
              -- reset the tool back to Select after creating a new elt
              goatState'' = goatState' {
                  _goatState_selectedTool =
                    -- NOTE we only want to reset back to Tool_Select if a SElt was actually created. This condition might be too weak in the future.
                    if _mouseDrag_state mouseDrag == MouseDragState_Up && tool_isCreate _goatState_selectedTool
                      then Tool_Select
                      else _goatState_selectedTool
                }
              r = makeGoatCmdTempOutputFromPotatoHandlerOutput goatState'' pho
            -- input not captured by handler, do select or select+drag
            Nothing | _mouseDrag_state mouseDrag == MouseDragState_Down -> assert (not $ pIsHandlerActive handler) r where
              nextSelection@(SuperOwlParliament sowls) = selectMagic last_pFState last_layerMetaMap _goatState_broadPhaseState canvasDrag
              -- since selection came from canvas, it's definitely a valid CanvasSelection, no need to convert
              nextCanvasSelection = CanvasSelection sowls
              shiftClick = isJust $ find (==KeyModifier_Shift) (_mouseDrag_modifiers mouseDrag)
              r = if isParliament_null nextSelection || shiftClick
                -- clicked on nothing or shift click, start SelectHandler
                -- NOTE this is weird because:
                -- 1. doesn't select until mouse up, which is regression from when you do regular click on an elt (via BoxHandler which immediately selects)
                -- 2. if you let go of shift before mouse up, it just does a standard mouse selection
                -- I think this is still better than letting BoxHandler handle this case
                then case pHandleMouse (def :: SelectHandler) potatoHandlerInput canvasDrag of
                  Just pho -> makeGoatCmdTempOutputFromPotatoHandlerOutput goatState' pho
                  Nothing -> error "handler was expected to capture this mouse state"

                -- special select+drag case, override the selection
                -- alternative, we could let the BoxHandler do this but that would mean we query broadphase twice
                -- (once to determine that we should create the BoxHandler, and again to set the selection in BoxHandler)
                -- also NOTE BoxHandler here is used to move all SElt types, upon release, it will either return the correct handler type or not capture the input in which case GoatWidget will set the correct handler type
                else case pHandleMouse (def { _boxHandler_creation = BoxCreationType_DragSelect }) (potatoHandlerInput { _potatoHandlerInput_selection = nextSelection, _potatoHandlerInput_canvasSelection = nextCanvasSelection }) canvasDrag of
                  -- it's a little weird because we are forcing the selection from outside the handler and ignoring the new selection results returned by pho (which should always be Nothing)
                  Just pho -> assert (isNothing . _potatoHandlerOutput_select $ pho)
                    $ makeGoatCmdTempOutputFromPotatoHandlerOutput goatState' (pho { _potatoHandlerOutput_select = Just (False, nextSelection) })
                  Nothing -> error "handler was expected to capture this mouse state"

            Nothing -> error $ "handler " <> show (pHandlerName handler) <> "was expected to capture mouse state " <> show (_mouseDrag_state mouseDrag)

      GoatCmdKeyboard kbd -> case kbd of
        -- special case, treat escape cancel mouse drag as a mouse input
        KeyboardData KeyboardKey_Esc _ | mouseDrag_isActive _goatState_mouseDrag -> r where
          canceledMouse = cancelDrag _goatState_mouseDrag
          goatState' = goatState {
              _goatState_mouseDrag = canceledMouse
            }
          r = if _mouseDrag_isLayerMouse _goatState_mouseDrag
            then case pHandleMouse _goatState_layersHandler potatoHandlerInput (RelMouseDrag canceledMouse) of
              Just pho -> makeGoatCmdTempOutputFromLayersPotatoHandlerOutput goatState' pho
              Nothing  -> makeGoatCmdTempOutputFromNothingClearHandler goatState'
            else case pHandleMouse handler potatoHandlerInput (toRelMouseDrag last_pFState _goatState_pan canceledMouse) of
              Just pho -> makeGoatCmdTempOutputFromPotatoHandlerOutput goatState' pho
              Nothing  -> makeGoatCmdTempOutputFromNothingClearHandler goatState'

        -- we are in the middle of mouse drag, ignore all keyboard inputs
        -- perhaps a better way to do this is to have handlers capture all inputs when active
        _ | mouseDrag_isActive _goatState_mouseDrag -> makeGoatCmdTempOutputFromNothing goatState

        _ ->
          let
            maybeHandleLayers = do
              guard $ _mouseDrag_isLayerMouse _goatState_mouseDrag
              pho <- pHandleKeyboard _goatState_layersHandler potatoHandlerInput kbd
              return $ makeGoatCmdTempOutputFromLayersPotatoHandlerOutput goatState pho
          in case maybeHandleLayers of
            Just x -> x
            Nothing -> case pHandleKeyboard handler potatoHandlerInput kbd of
              Just pho -> makeGoatCmdTempOutputFromPotatoHandlerOutput goatState pho
              -- input not captured by handler
              -- TODO consider wrapping this all up in KeyboardHandler or something? Unfortunately, copy needs to modify goatState which PotatoHandlerOutput can't atm
              Nothing -> case kbd of
                KeyboardData KeyboardKey_Esc _ ->
                  (makeGoatCmdTempOutputFromNothing goatState) {
                      -- TODO change tool back to select?
                      -- cancel selection if we are in a neutral mouse state and there is no handler
                      _goatCmdTempOutput_select = case _mouseDrag_state _goatState_mouseDrag of
                        MouseDragState_Up        -> Just (False, isParliament_empty)
                        MouseDragState_Cancelled -> Just (False, isParliament_empty)
                        _                        -> Nothing
                    }

                KeyboardData (KeyboardKey_Delete) [] -> r where
                  r = makeGoatCmdTempOutputFromMaybeEvent goatState (deleteSelectionEvent goatState)
                KeyboardData (KeyboardKey_Char 'c') [KeyModifier_Ctrl] -> r where
                  copied = makeClipboard goatState
                  r = makeGoatCmdTempOutputFromNothing $ goatState { _goatState_clipboard = copied }
                KeyboardData (KeyboardKey_Char 'x') [KeyModifier_Ctrl] -> r where
                  copied = makeClipboard goatState
                  r = makeGoatCmdTempOutputFromMaybeEvent (goatState { _goatState_clipboard = copied }) (deleteSelectionEvent goatState)
                KeyboardData (KeyboardKey_Char 'v') [KeyModifier_Ctrl] -> case _goatState_clipboard of
                  Nothing    -> makeGoatCmdTempOutputFromNothing goatState
                  Just stree -> r where

                    -- TODO this is totally wrong, it won't handle parent/children stuff correctly
                    -- TODO convert to MiniOwlTree :D
                    offsetstree = offsetSEltTree (V2 1 1) stree
                    minitree' = owlTree_fromSEltTree offsetstree
                    maxid1 = owlTree_maxId minitree' + 1
                    maxid2 = owlPFState_nextId (_owlPFWorkspace_pFState _goatState_workspace)
                    minitree = owlTree_reindex (max maxid1 maxid2) minitree'
                    spot = lastPositionInSelection (goatState_owlTree goatState) _goatState_selection
                    treePastaEv = WSEAddTree (spot, minitree)



                    r = makeGoatCmdTempOutputFromEvent (goatState { _goatState_clipboard = Just offsetstree }) treePastaEv
                KeyboardData (KeyboardKey_Char 'z') [KeyModifier_Ctrl] -> r where
                  r = makeGoatCmdTempOutputFromEvent goatState WSEUndo
                KeyboardData (KeyboardKey_Char 'y') [KeyModifier_Ctrl] -> r where
                  r = makeGoatCmdTempOutputFromEvent goatState WSERedo
                -- tool hotkeys
                KeyboardData (KeyboardKey_Char key) _ -> r where
                  newTool = case key of
                    'v'  -> Tool_Select
                    'p' -> Tool_Pan
                    'b'  -> Tool_Box
                    'l' -> Tool_Line
                    't'  -> Tool_Text
                    'n'  -> Tool_TextArea
                    _    -> _goatState_selectedTool
                  -- TODO consider clearing/resetting the handler?
                  r = makeGoatCmdTempOutputFromNothing $ goatState { _goatState_selectedTool = newTool }

                -- unhandled input
                _ -> makeGoatCmdTempOutputFromNothing goatState

  -- update OwlPFWorkspace from pho
  (next_workspace, cslmap_afterEvent) = case _goatCmdTempOutput_pFEvent goatCmdTempOutput of
    -- if there was no update, then changes are not valid
    Nothing   -> (_goatState_workspace, IM.empty)
    Just (_, wsev) -> (r1,r2) where
      r1 = updateOwlPFWorkspace wsev _goatState_workspace
      r2 = _owlPFWorkspace_lastChanges r1
  next_pFState = _owlPFWorkspace_pFState next_workspace

  -- update pan from pho
  next_pan = case _goatCmdTempOutput_pan goatCmdTempOutput of
    Nothing -> _goatState_pan
    Just (V2 dx dy) -> V2 (cx0+dx) (cy0 + dy) where
      V2 cx0 cy0 = _goatState_pan

  -- update layersState from pho
  next_layersState'' = case _goatCmdTempOutput_layersState goatCmdTempOutput of
    Nothing -> _goatState_layersState
    Just ls -> ls

  -- get selection from pho
  mSelectionFromPho = case _goatCmdTempOutput_select goatCmdTempOutput of
    Nothing -> Nothing
    --Just (add, sel) -> assert (superOwlParliament_isValid nextot r) $ Just r where
    Just (add, sel) -> assert (superOwlParliament_isValid nextot r) (Just r)where
      nextot = _owlPFState_owlTree next_pFState
      r' = if add
        then superOwlParliament_disjointUnionAndCorrect nextot _goatState_selection sel
        else sel
      r = SuperOwlParliament . Seq.sortBy (owlTree_superOwl_comparePosition nextot) . unSuperOwlParliament $ r'

  -- update selection based on changes from updating OwlPFState (i.e. auto select newly created stuff if appropriate)
  (isNewSelection', selectionAfterChanges) = if IM.null cslmap_afterEvent
    then (False, _goatState_selection)
    else r where

      -- TODO need to sort
      -- extract elements that got created
      newEltFoldMapFn rid v = case v of
        Nothing     -> []
        Just sowl -> if IM.member rid (_owlTree_mapping . _owlPFState_owlTree $ last_pFState) then [] else [sowl]
      newlyCreatedSEltls = IM.foldMapWithKey newEltFoldMapFn cslmap_afterEvent

      wasLoad = case cmd of
        GoatCmdLoad _ -> True
        _             -> False

      r = if wasLoad || null newlyCreatedSEltls
        -- if there are no newly created elts, we still need to update the selection
        then (\x -> (False, SuperOwlParliament x)) $ catMaybesSeq . flip fmap (unSuperOwlParliament _goatState_selection) $ \sowl ->
          case IM.lookup (_superOwl_id sowl) cslmap_afterEvent of
            -- no changes means not deleted
            Nothing                  -> Just sowl
            -- if deleted, remove it
            Just Nothing             -> Nothing
            -- it was changed, update selection to newest version
            Just (Just x) -> Just x
        else (True, SuperOwlParliament $ Seq.fromList newlyCreatedSEltls)


  (isNewSelection, next_selection) = case mSelectionFromPho of
    Just x  -> assert (not isNewSelection') (True, x)
    -- better/more expensive check to ensure mSelectionFromPho stuff is mutually exclusive to selectionAfterChanges
    --Just x -> assert (selectionAfterChanges == _goatState_selection) (True, x)
    Nothing -> (isNewSelection', selectionAfterChanges)




  -- TODO auto expand folders for selected elements
  -- the problem comes from when you try and collapse a folder that has a selected child.... therefore maybe auto expand should only happen on newly created elements or add a way to detect for newly selected elements (e.g. diff between old selection)
  next_layersState' = next_layersState''

  mHandlerFromPho = _goatCmdTempOutput_nextHandler goatCmdTempOutput

  filterHiddenOrLocked sowl = not $ layerMetaMap_isInheritHiddenOrLocked (_owlPFState_owlTree next_pFState) (_superOwl_id sowl) (_layersState_meta next_layersState')
  next_canvasSelection = superOwlParliament_convertToCanvasSelection (_owlPFState_owlTree next_pFState) filterHiddenOrLocked next_selection

  nextHandlerFromSelection = makeHandlerFromSelection next_canvasSelection

  next_handler' = if isNewSelection
    -- if there is a new selection, update the handler with new selection if handler wasn't active
    then maybeUpdateHandlerFromSelection (fromMaybe (SomePotatoHandler EmptyHandler) mHandlerFromPho) next_canvasSelection
    -- otherwise, use the returned handler or make a new one from selection
    else fromMaybe nextHandlerFromSelection mHandlerFromPho

  next_handler = case _goatCmdTempOutput_pFEvent goatCmdTempOutput of
    -- TODO you only need to do this if handler is one that came from mHandlerFromPho
    -- if there was a non-canvas event, reset the handler D:
    -- since we don't have multi-user events, the handler should never be active when this happens
    -- TODO you also want to reset layers handler here too
    Just (False, _) -> assert (not (pIsHandlerActive next_handler')) $ fromMaybe nextHandlerFromSelection ( pRefreshHandler next_handler' potatoHandlerInput)
    _ -> next_handler'

  -- TODO if cslmap_afterEvent has a newly created folder (i.e. we just createda folder) then we want to enter rename mode for that folder
    -- this is not correct, we want a condition for when we hit the "new folder" button. Perhaps there needs to be a separate command for enter rename and FE triggers 2 events in succession?
  --_goatState_layersHandler

  next_layersState = updateLayers next_pFState cslmap_afterEvent next_layersState'
  cslmap_fromLayersHide = _goatCmdTempOutput_changesFromToggleHide goatCmdTempOutput
  cslmap_forRendering = cslmap_afterEvent `IM.union` cslmap_fromLayersHide
  (needsupdateaabbs, next_broadPhaseState) = update_bPTree cslmap_forRendering (_broadPhaseState_bPTree _goatState_broadPhaseState)

  -- update the rendered region if we moved the screen
  canvasRegionBox = LBox (-next_pan) (goatCmdTempOutput_screenRegion goatCmdTempOutput)
  newBox = canvasRegionBox
  didScreenRegionMove = _renderedCanvasRegion_box _goatState_renderedCanvas /= newBox
  next_renderedCanvas' = if didScreenRegionMove
    then moveRenderedCanvasRegion next_broadPhaseState (_owlPFState_owlTree next_pFState, _layersState_meta next_layersState) newBox _goatState_renderedCanvas
    else _goatState_renderedCanvas

  -- render the scene if there were changes, note that updates from actual changes are mutually exclusive from updates due to panning (although I think it would still work even if it weren't)
  next_renderedCanvas = if IM.null cslmap_forRendering
    then next_renderedCanvas'
    else (assert $ not didScreenRegionMove) $ updateCanvas cslmap_forRendering needsupdateaabbs next_broadPhaseState (_owlPFState_owlTree next_pFState, _layersState_meta next_layersState) next_renderedCanvas'

  -- render the selection (just rerender it every time)
  selectionselts = toList . fmap superOwl_toSElt_hack $ unSuperOwlParliament next_selection
  next_renderedSelection = if _goatState_selection == next_selection && not didScreenRegionMove && IM.null cslmap_forRendering
    -- nothing changed, we can keep our selection rendering
    then _goatState_renderedSelection
    -- TODO omit hidden stuff here???  (or maybe not!)
    else render newBox selectionselts (emptyRenderedCanvasRegion newBox)

  {- TODO render only parts of selection that have changed TODO broken
  next_renderedSelection' = if didScreenRegionMove
    then moveRenderedCanvasRegion next_broadPhaseState (_owlPFState_owlTree next_pFState) newBox _goatState_renderedSelection
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
    else updateCanvas cslmapForSelectionRendering needsupdateaabbsforrenderselection next_broadPhaseState next_pFState next_renderedSelection'
  -}



  finalGoatState = (_goatCmdTempOutput_goatState goatCmdTempOutput) {
      _goatState_workspace      = next_workspace
      , _goatState_pan             = next_pan
      , _goatState_handler         = next_handler
      , _goatState_selection       = next_selection
      , _goatState_canvasSelection = next_canvasSelection
      , _goatState_broadPhaseState = next_broadPhaseState
      , _goatState_renderedCanvas = next_renderedCanvas
      , _goatState_renderedSelection = next_renderedSelection
      , _goatState_layersState     = next_layersState
    }
