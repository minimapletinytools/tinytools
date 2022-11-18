{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.Handler where

import           Relude

import           Potato.Flow.BroadPhase
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.OwlLayers
import           Potato.Flow.Controller.Types
import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.OwlItem
import Potato.Flow.OwlState
import Potato.Flow.OwlWorkspace
import Potato.Flow.Owl

import qualified Potato.Data.Text.Zipper                          as TZ

import           Data.Default
import qualified Data.IntMap                   as IM
import qualified Data.Sequence                 as Seq
import qualified Data.Text                     as T
import qualified Text.Show

data PotatoHandlerOutput = PotatoHandlerOutput {
    _potatoHandlerOutput_nextHandler   :: Maybe SomePotatoHandler
    , _potatoHandlerOutput_select      :: Maybe (Bool, Selection)
    , _potatoHandlerOutput_pFEvent     :: Maybe WSEvent
    , _potatoHandlerOutput_pan         :: Maybe XY
    , _potatoHandlerOutput_layersState :: Maybe LayersState
    , _potatoHandlerOutput_changesFromToggleHide :: SuperOwlChanges
  } deriving (Show)

instance Default PotatoHandlerOutput where
  def = PotatoHandlerOutput {
      _potatoHandlerOutput_nextHandler = Nothing
      , _potatoHandlerOutput_pFEvent = Nothing
      , _potatoHandlerOutput_pan = Nothing
      , _potatoHandlerOutput_select = Nothing
      , _potatoHandlerOutput_layersState = Nothing
      , _potatoHandlerOutput_changesFromToggleHide = IM.empty
    }

-- TODO replace this with just GoatState
data PotatoHandlerInput = PotatoHandlerInput {
    -- from PFOutput
    _potatoHandlerInput_pFState       :: OwlPFState
    , _potatoHandlerInput_potatoDefaultParameters :: PotatoDefaultParameters
    , _potatoHandlerInput_broadPhase  :: BroadPhaseState

    -- from Frontend
    , _potatoHandlerInput_layersState :: LayersState
    , _potatoHandlerInput_screenRegion :: LBox


    -- from Backend
    -- basically, handlers are created based on contents of selection, and handlers themselves are expected to use partial methods on selection to get relevant information in order to modify the selection
    -- note that selection is dynamically updated each type a change is made so it always has up to date information during a multi-step manipulate
    -- this is sort of just how it is right now, I wish it weren't so :_(
    , _potatoHandlerInput_selection   :: Selection
    , _potatoHandlerInput_canvasSelection :: CanvasSelection

    -- TODO
    --, _potatoHandlerInput_canvasSelection :: CanvasSelection
    -- superOwlParliament_convertToCanvasSelection
  }

type ColorType = ()
data SimpleBoxHandlerRenderOutput = SimpleBoxHandlerRenderOutput {
    _simpleBoxHandlerRenderOutput_box :: LBox
    , _simpleBoxHandlerRenderOutput_fillText :: Maybe PChar -- fills the entire box with the same char
    , _simpleBoxHandlerRenderOutput_fillTextColor :: ColorType
    , _simpleBoxHandlerRenderOutput_bgColor :: ColorType
  }

-- TODO remove renaming and move it into LayersHandlerRenderEntry
data LayersHandlerRenderEntrySelectedState = LHRESS_ChildSelected | LHRESS_Selected | LHRESS_InheritSelected | LHRESS_None deriving (Show, Eq)

{--instance Eq LayersHandlerRenderEntrySelectedState where
  (==) (LHRESS_Renaming x) (LHRESS_Renaming y) = x == y
  (==) LHRESS_Selected LHRESS_Selected = True
  (==) LHRESS_InheritSelected LHRESS_InheritSelected = True
  (==) LHRESS_None LHRESS_None = True
  (==) LHRESS_ChildSelected LHRESS_ChildSelected = True
  (==) _ _ = False--}

-- depth at which dots are added if any
type LayersHandlerRenderEntryDots = Maybe Int
-- are we renaming this one
type LayersHandlerRenderEntryRenaming = Maybe TZ.TextZipper

data LayersHandlerRenderEntry =
  LayersHandlerRenderEntryNormal LayersHandlerRenderEntrySelectedState LayersHandlerRenderEntryDots LayersHandlerRenderEntryRenaming LayerEntry
  | LayersHandlerRenderEntryDummy Int
  deriving (Eq, Show)

layersHandlerRenderEntry_depth :: LayersHandlerRenderEntry -> Int
layersHandlerRenderEntry_depth (LayersHandlerRenderEntryNormal _ _ _ lentry) = layerEntry_depth lentry
layersHandlerRenderEntry_depth (LayersHandlerRenderEntryDummy i) = i

-- hack to render layers view via HandlerRenderOutput (we could have just as well put this in LayerState I guesss)
data LayersViewHandlerRenderOutput = LayersViewHandlerRenderOutput {
    _layersViewHandlerRenderOutput_entries :: Seq LayersHandlerRenderEntry
  } deriving (Eq, Show)

instance Default LayersViewHandlerRenderOutput where
  def = LayersViewHandlerRenderOutput {
      _layersViewHandlerRenderOutput_entries = Seq.empty
    }

data RenderHandleColor = RHC_Default | RHC_Attachment | RHC_AttachmentHighlight deriving (Show, Eq)

-- TODO come up with better name
data RenderHandle = RenderHandle {
    _renderHandle_box :: LBox
    , _renderHandle_char :: Maybe PChar
    , _renderHandle_color :: RenderHandleColor
  } deriving (Show, Eq)

defaultRenderHandle :: LBox -> RenderHandle
defaultRenderHandle lbox = RenderHandle lbox (Just 'X') RHC_Default

-- TODO come up with better name
data HandlerRenderOutput = HandlerRenderOutput {
    _handlerRenderOutput_temp :: [RenderHandle] -- list of coordinates where there are handles
  } deriving (Eq)

instance Semigroup HandlerRenderOutput where
  a <> b = HandlerRenderOutput {
      _handlerRenderOutput_temp = _handlerRenderOutput_temp a <> _handlerRenderOutput_temp b
    }

instance Default HandlerRenderOutput where
  def = emptyHandlerRenderOutput

emptyHandlerRenderOutput :: HandlerRenderOutput
emptyHandlerRenderOutput = HandlerRenderOutput { _handlerRenderOutput_temp = [] }


-- we check handler name for debug reasons so it's useful to have constants
-- there should be no non-test code that depends on comparing pHandlerName
handlerName_box :: Text
handlerName_box = "BoxHandler"
handlerName_simpleLine :: Text
handlerName_simpleLine = "AutoLineHandler"
handlerName_simpleLine_endPoint :: Text
handlerName_simpleLine_endPoint = "AutoLineEndPointHandler"
handlerName_simpleLine_midPoint :: Text
handlerName_simpleLine_midPoint = "AutoLineMidPointHandler"
handlerName_simpleLine_textLabel :: Text
handlerName_simpleLine_textLabel = "AutoLineTextLabelHandler"
handlerName_layers :: Text
handlerName_layers = "LayersHandler"
handlerName_layersRename :: Text
handlerName_layersRename = "LayersRenameHandler"
handlerName_cartesianLine :: Text
handlerName_cartesianLine = "CartesianLineHandler"
handlerName_boxText :: Text
handlerName_boxText = "BoxTextHandler"
handlerName_boxLabel :: Text
handlerName_boxLabel = "BoxLabelHandler"
handlerName_textArea :: Text
handlerName_textArea = "TextAreaHandler"
handlerName_pan :: Text
handlerName_pan = "PanHandler"
handlerName_select :: Text
handlerName_select = "SelectHandler"
handlerName_empty :: Text
handlerName_empty = "EmptyHandler"


-- TODO prob replace this with 'data PotatoHandler' rather than typeclass
-- TODO rename methods in here..
-- TODO rename to Manipulator XD
class PotatoHandler h where
  pHandlerName :: h -> Text

  -- TODO do the generic thing where (Show h) whatever (I guess this only works when you use deriving or something though?)
  pHandlerDebugShow :: h -> Text
  pHandlerDebugShow h = pHandlerName h <> " <no debug info>"

  -- TODO consider removing Selection from input args since it should be static through lifetime of handler and therefore passed in during construction
  -- i.e. invariant is selection changed -> new handler

  -- TODO need to add broadphase to args as it's used for finding new selections..
  -- TODO maybe split into handleLayerMouse (MouseDrag) and handleCanvasMouse (RelMosueDrag)?
  -- NOTE, MouseDragState_Cancelled will never be passed into this
  -- return type of Nothing means input is not captured
  pHandleMouse :: h -> PotatoHandlerInput -> RelMouseDrag -> Maybe PotatoHandlerOutput

  -- return type of Nothing means input is not captured
  pHandleKeyboard :: h -> PotatoHandlerInput -> KeyboardData -> Maybe PotatoHandlerOutput

  -- reset handler if an event came in in between (e.g. due to undo, redo)
  --
  -- FOR NOW we expect this to only be called if handler is not active
  -- FOR NOW this is only allowed to return the existing handler
  -- when we have multi-user, this may return actions (to undo some inprogress state I guess?), and may happen when a handler is active
  --
  pRefreshHandler :: h -> PotatoHandlerInput -> Maybe SomePotatoHandler
  pRefreshHandler _ _ = Nothing

  -- active manipulators will not be overwritten by new handlers via selection from backend
  pIsHandlerActive :: h -> Bool
  pIsHandlerActive _ = False

  pRenderHandler :: h -> PotatoHandlerInput -> HandlerRenderOutput
  pRenderHandler _ _ = def

  -- ad-hoc render function just for layers
  -- note that this renders layers even when there is no drop location to be rendered (which is owned by the LayersHandler)
  -- a bit of a hack but it's easier this way, the other way to do it would have been to put drop location inside of LayersState
  -- layers are different because when rendering drop location, it's not a strict overlay so normal render/handler render (drop location) are combined
  pRenderLayersHandler :: h -> PotatoHandlerInput -> LayersViewHandlerRenderOutput
  pRenderLayersHandler _ _ = def

  -- helper method used to check that we aren't feeding invalid mouse states
  pValidateMouse :: h -> RelMouseDrag -> Bool
  -- default version that ensures mouse state is valid when handler is active
  pValidateMouse h (RelMouseDrag MouseDrag {..}) = case _mouseDrag_state of
    MouseDragState_Cancelled -> False
    MouseDragState_Down      -> not $ pIsHandlerActive h
    _                        -> True

  -- determine which selected tool to show
  pHandlerTool :: h -> Maybe Tool
  pHandlerTool _ = Nothing

data SomePotatoHandler = forall h . PotatoHandler h  => SomePotatoHandler h

instance PotatoHandler SomePotatoHandler where
  pHandlerName (SomePotatoHandler h) = pHandlerName h
  pHandlerDebugShow (SomePotatoHandler h) = pHandlerDebugShow h
  pHandleMouse (SomePotatoHandler h) = pHandleMouse h
  pHandleKeyboard (SomePotatoHandler h) = pHandleKeyboard h
  pIsHandlerActive (SomePotatoHandler h) = pIsHandlerActive h
  pRefreshHandler (SomePotatoHandler h) = pRefreshHandler h
  pRenderHandler (SomePotatoHandler h) = pRenderHandler h
  pRenderLayersHandler (SomePotatoHandler h) = pRenderLayersHandler h
  pValidateMouse (SomePotatoHandler h) = pValidateMouse h
  pHandlerTool (SomePotatoHandler h) = pHandlerTool h

captureWithNoChange :: (PotatoHandler h) => h -> PotatoHandlerOutput
captureWithNoChange h = def {
    _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler h
  }

setHandlerOnly :: (PotatoHandler h) => h -> PotatoHandlerOutput
setHandlerOnly = captureWithNoChange

instance Show SomePotatoHandler where
  show (SomePotatoHandler h) = T.unpack $ "SomePotatoHandler " <> pHandlerName h <> " active: " <> show (pIsHandlerActive h)

testHandleMouse :: SomePotatoHandler -> PotatoHandlerInput -> RelMouseDrag -> Maybe PotatoHandlerOutput
testHandleMouse (SomePotatoHandler h) phi rmd = pHandleMouse h phi rmd


data EmptyHandler = EmptyHandler

instance PotatoHandler EmptyHandler where
  pHandlerName _ = "EmptyHandler"
  pHandleMouse _ _ _ = Nothing
  pHandleKeyboard _ _ _ = Nothing
  pRenderHandler _ _ = def
  pValidateMouse _ _ = True


{--
-- you can do something like the below to have handlers share some functionality
-- unfortuantely, the design below is not very composable, although maybe this isn't really something that can be composed
data ActiveHandlerState s = ActiveHandlerState {
    _activeHandlerState_isActive :: Bool
    _activeHandlerState_userState :: s
  }

data ActiveHandler s = ActiveHandler {
  _activeHandler_pHandleMouse :: s -> PotatoHandlerInput -> RelMouseDrag -> (Bool, Maybe PotatoHandlerOutput)
  -- ...
}
--}
