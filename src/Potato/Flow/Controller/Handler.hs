{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.Handler (
  PotatoHandlerOutput
  , PotatoHandler(..)
  , SomePotatoHandler(..)
) where

import           Relude

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
import           Data.Tuple.Extra

-- TODO I don't think the selection thing is necessary.. only Layer drags use it...
-- use DMap if you start having more actions...
type PotatoHandlerOutput = (Maybe SomePotatoHandler, Maybe (Bool, Selection), Maybe PFEventTag)

-- TODO rename methods in here..
-- rename to Manipulator XD
class PotatoHandler h where
  pHandlerName :: h -> Text

  -- TODO maybe split into handleLayerMouse (MouseDrag) and handleCanvasMouse (RelMosueDrag)?
  pHandleMouse :: h -> PFState -> Selection -> RelMouseDrag -> PotatoHandlerOutput
  pHandleKeyboard :: h -> PFState -> Selection -> KeyboardData -> PotatoHandlerOutput
  pHandleCancel :: h -> PFState -> Selection -> KeyboardData -> PotatoHandlerOutput

  -- TODO handler render type??
  --
  pRenderHandler :: h -> ()


  -- helper method used to check that we aren't feeding invalid mouse states
  pValidateMouse :: h -> RelMouseDrag -> Bool

data SomePotatoHandler = forall h . PotatoHandler h  => SomePotatoHandler h

testHandleMouse :: SomePotatoHandler -> PFState -> Selection -> RelMouseDrag -> PotatoHandlerOutput
testHandleMouse (SomePotatoHandler h) pfs sel rmd = pHandleMouse h pfs sel rmd
