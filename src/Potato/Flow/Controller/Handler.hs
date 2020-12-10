{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.Handler (
  PotatoHandlerOutput
  , PotatoHandler(..)
  , PotatoHandlerInput(..)
  , SomePotatoHandler(..)
  , EmptyHandler(..)
  , SelectHandler(..)
) where

import           Relude

import           Potato.Flow.BroadPhase
import           Potato.Flow.Controller.Input
import           Potato.Flow.Entry
import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.State
import           Potato.Flow.Types

import           Data.Dependent.Sum           (DSum ((:=>)))
import qualified Data.IntMap                  as IM
import qualified Data.List                    as L
import qualified Data.Sequence                as Seq
import qualified Data.Text                    as T
import           Data.Tuple.Extra
import qualified Text.Show

-- TODO I don't think the selection thing is necessary.. only Layer drags use it...
-- use DMap if you start having more actions...
type PotatoHandlerOutput = (Maybe SomePotatoHandler, Maybe (Bool, Selection), Maybe PFEventTag)

data PotatoHandlerInput = PotatoHandlerInput {
    _potatoHandlerInput_pFState      :: PFState
    , _potatoHandlerInput_broadPhase :: BroadPhaseState
    , _potatoHandlerInput_selection  :: Selection
  }

-- TODO prob replace this with 'data PotatoHandler' rather than typeclass
-- TODO rename methods in here..
-- rename to Manipulator XD
class PotatoHandler h where
  pHandlerName :: h -> Text

  -- TODO consider removing Selection from input args since it should be static through lifetime of handler and therefore passed in during construction
  -- i.e. invariant is selection changed -> new handler

  -- TODO need to add broadphase to args as it's used for finding new selections..
  -- TODO maybe split into handleLayerMouse (MouseDrag) and handleCanvasMouse (RelMosueDrag)?
  -- NOTE, MouseDragState_Cancelled will never be passed into this
  -- return type of Nothing means input is not captured
  pHandleMouse :: h -> PotatoHandlerInput -> RelMouseDrag -> Maybe PotatoHandlerOutput
  -- NOTE, Escape key is never passed in, instead that goes to pHandleCancel
  -- return type of Nothing means input is not captured
  pHandleKeyboard :: h -> PotatoHandlerInput -> KeyboardData -> Maybe PotatoHandlerOutput
  pHandleCancel :: h -> PotatoHandlerInput -> PotatoHandlerOutput

  -- TODO handler render type??
  --
  pRenderHandler :: h -> ()


  -- helper method used to check that we aren't feeding invalid mouse states
  pValidateMouse :: h -> RelMouseDrag -> Bool

data SomePotatoHandler = forall h . PotatoHandler h  => SomePotatoHandler h

instance Show SomePotatoHandler where
  show (SomePotatoHandler h) = T.unpack $ "SomePotatoHandler " <> pHandlerName h

testHandleMouse :: SomePotatoHandler -> PotatoHandlerInput -> RelMouseDrag -> Maybe PotatoHandlerOutput
testHandleMouse (SomePotatoHandler h) phi rmd = pHandleMouse h phi rmd


data EmptyHandler = EmptyHandler

instance PotatoHandler EmptyHandler where
  pHandlerName _ = "EmptyHandler"
  pHandleMouse _ _ _ = Nothing
  pHandleKeyboard _ _ _ = Nothing
  pHandleCancel _ _ = (Nothing, Nothing, Nothing)
  pRenderHandler = undefined
  pValidateMouse _ _ = True

-- TODO move to another file?
data SelectHandler = SelectHandler {
    _selectHandler_selecting :: Bool
  }

instance PotatoHandler SelectHandler where
  pHandlerName _ = "SelectHandler"
  pHandleMouse sh PotatoHandlerInput {..} (RelMouseDrag MouseDrag {..}) = Just $ case _mouseDrag_state of
    MouseDragState_Down -> (Just $ SomePotatoHandler sh { _selectHandler_selecting = True}, Nothing, Nothing)
    MouseDragState_Dragging -> (Just $ SomePotatoHandler sh, Nothing, Nothing)
    MouseDragState_Up -> (Nothing, newSelection, Nothing) where
      -- TODO need broadphase
      newSelection = undefined
    MouseDragState_Cancelled -> error "unexpected mouse state passed to handler"
  pHandleKeyboard sh PotatoHandlerInput {..} kbd = Nothing
  pHandleCancel sh PotatoHandlerInput {..} = (Nothing, Nothing, Nothing)
  pRenderHandler = undefined
  pValidateMouse sh (RelMouseDrag MouseDrag {..}) = if _selectHandler_selecting sh
    then _mouseDrag_state /= MouseDragState_Down
    else _mouseDrag_state == MouseDragState_Down
