{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.Handler (
  SomePotatoHandlerOutput
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

-- use DMap if you start having more actions...
type PotatoHandlerOutput h = (Maybe (PotatoHandlerState h), Maybe (Bool, Selection), Maybe PFEventTag)
type SomePotatoHandlerOutput = (Maybe SomePotatoHandler, Maybe (Bool, Selection), Maybe PFEventTag)

class PotatoHandler h where
  type PotatoHandlerState h :: *
  pHandlerName :: h -> Text
  -- TODO maybe split into handleLayerMouse (MouseDrag) and handleCanvasMouse (RelMosueDrag)?
  pHandleMouse :: h -> PotatoHandlerState h -> PFState -> Selection -> RelMouseDrag -> Maybe (PotatoHandlerOutput h)
  pHandleKeyboard :: h -> PotatoHandlerState h -> PFState -> Selection -> KeyboardData -> Maybe (PotatoHandlerOutput h)


data SomePotatoHandler = forall h . PotatoHandler h  => SomePotatoHandler h (PotatoHandlerState h)

testHandleMouse :: SomePotatoHandler -> PFState -> Selection -> RelMouseDrag -> SomePotatoHandlerOutput
testHandleMouse (SomePotatoHandler h hs) pfs sel rmd = r where
  mho = pHandleMouse h hs pfs sel rmd
  r = case mho of
    Nothing -> (Nothing, Nothing, Nothing)
    Just (mhs, mnewsel, mpfe) -> (mshout, mnewsel, mpfe) where
      mshout = case mhs of
        Nothing    -> Nothing
        Just newhs -> Just $ SomePotatoHandler h newhs
