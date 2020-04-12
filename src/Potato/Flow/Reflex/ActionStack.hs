{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Reflex.ActionStack (
  ActionStack(..)
  , ActionStackConfig(..)
  , holdActionStack
) where

import           Relude

import           Reflex
import           Reflex.Potato

import           Control.Monad.Fix

import           Data.Wedge



getHere :: Wedge a b -> Maybe a
getHere c = case c of
  Here x -> Just x
  _      -> Nothing

getThere :: Wedge a b -> Maybe b
getThere c = case c of
  There x -> Just x
  _       -> Nothing


data ActionStack t a = ActionStack {
  _actionStack_do            :: Event t a -- ^ fires when element is added to do stack
  , _actionStack_undo        :: Event t a -- ^ fires when element is added to undo stack
  , _actionStack_doneStack   :: Dynamic t [a] -- ^ stack of actions we've done
  , _actionStack_undoneStack :: Dynamic t [a] -- ^ stack of actions we've undone
}

data ActionStackConfig t a = ActionStackConfig {
  _actionStackConfig_do      :: Event t a -- ^ event to add an element to the stack
  , _actionStackConfig_undo  :: Event t () -- ^ event to undo top action of do stack
  , _actionStackConfig_redo  :: Event t () -- ^ event to redo top action of undo stack
  , _actionStackConfig_clear :: Event t () -- ^ clears both do/undo stack without firing any events
}

-- helper type for holdActionStack
data ASCmd t a = ASCDo a | ASCUndo | ASCRedo | ASCClear


holdActionStack ::
  forall t m a. (Reflex t, MonadHold t m, MonadFix m)
  => ActionStackConfig t a
  -> m (ActionStack t a)
holdActionStack (ActionStackConfig { .. }) = do
  let
    changeEvent :: Event t (ASCmd t a)
    changeEvent = leftmostwarn "WARNING: multiple ActionStack events firing at once" [
        fmap ASCDo _actionStackConfig_do
        , fmap (const ASCUndo) _actionStackConfig_undo
        , fmap (const ASCRedo) _actionStackConfig_redo
        , fmap (const ASCClear) _actionStackConfig_clear
      ]

    -- Wedge values:
    -- Here is element that was just added to do stack
    -- There is element that was just added to undo stack
    -- Nowhere is everything else
    foldfn :: (ASCmd t a) -> (Wedge a a, [a],[a]) -> PushM t (Wedge a a, [a],[a])
    foldfn (ASCDo x) (_, xs, _)  = return (Here x, x:xs, []) -- clear undo stack on each new do
    foldfn ASCUndo (_, [], ys)   = return (Nowhere, [], ys)
    foldfn ASCUndo (_, x:xs, ys) = return (There x, xs, x:ys)
    foldfn ASCRedo (_, xs, [])   = return (Nowhere, xs, [])
    foldfn ASCRedo (_, xs, y:ys) = return (Here y, y:xs, ys)
    foldfn ASCClear (_, _, _)    = return (Nowhere, [], [])

  asdyn :: Dynamic t (Wedge a a, [a], [a]) <-
    foldDynM foldfn (Nowhere, [], []) changeEvent

  let
    changedEv :: Event t (Wedge a a)
    changedEv = fmap (\(x,_,_)->x) (updated asdyn)

  return $
    ActionStack {
      _actionStack_do = fmapMaybe getHere changedEv
      , _actionStack_undo   = fmapMaybe getThere changedEv
      , _actionStack_doneStack = fmap (\(_,x,_)->x) asdyn
      , _actionStack_undoneStack = fmap (\(_,_,x)->x) asdyn
    }
