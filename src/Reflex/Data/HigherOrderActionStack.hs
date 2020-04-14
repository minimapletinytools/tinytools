{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Reflex.Data.HigherOrderActionStack (

  HigherOrderActionStack
  , HigherOrderAction(..)
  , HigherOrderActionStackConfig(..)
  , holdHigherOrderActionStack
) where

import           Relude

import           Reflex
import           Reflex.Potato.Helpers

import           Control.Monad.Fix

import qualified Data.Dependent.Map      as DMap
import           Data.Functor.Misc
import           Data.Wedge
import           Reflex.Data.ActionStack


type HigherOrderActionStack t a = (ActionStack t a, Event t (DoType a), Event t (UndoType a))

-- | optional class to help with creating network that respond to events fired off by elements added to the stack that sampled from _actionStack_eventsForNextDoAction
class HigherOrderAction t a where
  data DoType a :: Type
  data UndoType a :: Type
  _higherOrderAction_doEvent :: Event t (DoType a)
  _higherOrderAction_undoEvent :: Event t (UndoType a)

data HigherOrderActionStackConfig t a = HigherOrderActionStackConfig {
  -- | event to add an element to the stack
  _higherOrderActionStackConfig_do      :: Event t ((Event t (), Event t ()) -> PushM t a)
}

-- TODO finish
holdHigherOrderActionStack ::
  forall t m a. (Reflex t, MonadHold t m, MonadFix m, HigherOrderAction t a)
  => ActionStackConfig t a
  -> HigherOrderActionStackConfig t a
  -> m (HigherOrderActionStack t a)
holdHigherOrderActionStack ascfg HigherOrderActionStackConfig {..} = undefined
{-
   mdo
  let
    newdo = undefined
    newcfg = ascfg { _actionStackConfig_do = newdo }
  as <- holdActionStack newcfg
-}
