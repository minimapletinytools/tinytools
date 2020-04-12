{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.ActionStack (

) where

import           Relude

import           Reflex
import           Reflex.Potato
import           Reflex.Stack

import           Control.Monad.Fix
import           Data.Kind

{-
data ActionEvents t = ActionEvents {
  a_do     :: Event t ()
  , a_undo :: Event t ()
}

-- TODO figure out this nonsense
class Action t a where
  --type doInfo t a :: Type
  --type undoInfo t a :: Type
  makeAction :: ActionEvents t -> PushM t a
  --doAction :: Event t (doInfo t a)
  --undoAction :: Event t (undoInfo t a)



data ActionStack t a = ActionStack {
  as_do          :: Event t a -- ^ fires when element is added to do stack
  , as_undo      :: Event t a -- ^ fires when element is added to undo stack
  , as_doStack   :: DynamicStack t a -- ^ stack of actions we've done
  , as_undoStack :: DynamicStack t a -- ^ stack of actions we've undone
}

data ModifyActionStack t a = ModifyActionStack {
  mas_do      :: Event t (ActionEvents t -> PushM t a) -- ^ event to add an element to the stack. Takes a function that produces the element in the PushM monad
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
    inputDoEvFn :: (ActionEvents t -> PushM t a) -> (Event t () -> PushM t a)
    inputDoEvFn f ev = f aev where
      --[ ]  push       :: (a -> PushM t (Maybe b)) -> Event a -> Event b
      magicIndexTracking :: Event t Int -> PushM t (Maybe ())
      magicIndexTracking = undefined

      aev = ActionEvents {
          a_do = ev
          , a_undo = push magicIndexTracking undoIndexEv
        }


    mds_done = ModifyDynamicStack {
        -- TODO also needs to track redoEv :scream:
        mds_push_rec = fmap inputDoEvFn mas_do
        , mds_pop = mas_undo
        , mds_clear = mas_clear
      }
    mds_undone = ModifyDynamicStack {
        -- we can ignore pop event value in our ctor function because magicIndexTracking above keeps the reference from when we pushed it on the do stack
        mds_push_rec = fmap (const . return) $ ds_popped doneStack
        , mds_pop = mas_redo
        , mds_clear = mas_clear
      }

    undoIndexEv = fmap fst $ ds_poppedAt undoneStack

    doEv = ds_pushed doneStack
    redoEv = ds_popped undoneStack
    undoEv = ds_pushed undoneStack

  -- TODO just internally store an ID for each element added so we can track it. EZ
  doneStack <- holdDynamicStack [] mds_done
  undoneStack <- holdDynamicStack [] mds_undone

  return $
    ActionStack {
      as_do = leftmostwarn "do and redo happened at the same time" [doEv, redoEv]
      , as_undo   = undoEv
      , as_doStack   = doneStack
      , as_undoStack = undoneStack
    }
-}
