{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}
module Potato.Flow.Reflex.ActionStack (
  ActionStack(..)
  , ModifyActionStack(..)
  , holdActionStack
) where

import           Relude

import           Reflex
import           Reflex.Potato
import           Reflex.Stack

import           Control.Monad.Fix




data ActionStack t a = ActionStack {
  as_do          :: Event t a -- ^ fires when element is added to do stack
  , as_undo      :: Event t a -- ^ fires when element is added to undo stack
  , as_doStack   :: DynamicStack t a -- ^ stack of actions we've done
  , as_undoStack :: DynamicStack t a -- ^ stack of actions we've undone
}

data ModifyActionStack t a = ModifyActionStack {
  mas_do      :: Event t a -- ^ event to add an element to the stack
  , mas_undo  :: Event t () -- ^ event to undo top action of do stack
  , mas_redo  :: Event t () -- ^ event to redo top action of undo stack
  , mas_clear :: Event t () -- ^ clears both do/undo stack without firing any events
}

holdActionStack ::
  forall t m a. (Reflex t, MonadHold t m, MonadFix m)
  => ModifyActionStack t a
  -> m (ActionStack t a)
holdActionStack (ModifyActionStack { .. }) = mdo
  let

    mds_done = ModifyDynamicStack {
        mds_push = leftmostwarn "WARNING: simultaneous do and redo" [mas_do, ds_popped undoneStack]
        , mds_pop = mas_undo
        , mds_clear = mas_clear
      }
    mds_undone = ModifyDynamicStack {
        mds_push = ds_popped doneStack
        , mds_pop = mas_redo
        -- a new do event clears the undo stack sorry :(
        , mds_clear = leftmostwarn "WARNING: simultaneous clear and do" [mas_clear, const () <$> mas_do]
      }

    doEv = ds_pushed doneStack
    redoEv = ds_popped undoneStack
    undoEv = ds_pushed undoneStack

  doneStack <- holdDynamicStack [] mds_done
  undoneStack <- holdDynamicStack [] mds_undone

  return $
    ActionStack {
      as_do = leftmostwarn "WARNING: do and redo happened at the same time" [doEv, redoEv]
      , as_undo   = undoEv
      , as_doStack   = doneStack
      , as_undoStack = undoneStack
    }
