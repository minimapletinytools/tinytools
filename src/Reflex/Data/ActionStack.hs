{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Reflex.Data.ActionStack (
  ActionStack(..)
  , ActionStackConfig(..)
  , holdActionStack
) where

import           Relude

import           Reflex
import           Reflex.Potato.Helpers

import           Control.Monad.Fix

import qualified Data.Dependent.Map    as DMap
import           Data.Functor.Misc
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
  -- | fires when element is added to do stack
  _actionStack_do                      :: Event t a
  -- | fires when element is added to undo stack
  , _actionStack_undo                  :: Event t a
  -- | contains the do/undo event of the next action that is added to the ActionStack
  -- attach this to event that ultimately triggers _actionStackConfig_do to create a dynamic Action that is responsible for its own do/undo
  , _actionStack_eventsForNextDoAction :: Behavior t (Event t (), Event t ())
}

data ActionStackConfig t a = ActionStackConfig {
  -- | event to add an element to the stack
  _actionStackConfig_do      :: Event t a
  -- | event to undo top action of do stack
  , _actionStackConfig_undo  :: Event t ()
  -- | event to redo top action of undo stack
  , _actionStackConfig_redo  :: Event t ()
  -- | clears both do/undo stack without firing any events
  , _actionStackConfig_clear :: Event t ()
}

-- helper types for holdActionStack
data ASCmd a = ASCDo (Int, a) | ASCUndo | ASCRedo | ASCClear
type UID = Int
type IDAct a = (UID,a)
type InternalStack a = (Wedge (IDAct a) (IDAct a), [IDAct a], [IDAct a])

holdActionStack ::
  forall t m a. (Reflex t, MonadHold t m, MonadFix m)
  => ActionStackConfig t a
  -> m (ActionStack t a)
holdActionStack (ActionStackConfig { .. }) = mdo
  let
    changeEvent :: Event t (ASCmd a)
    changeEvent = leftmostwarn "WARNING: multiple ActionStack events firing at once" [
        fmap ASCDo $ attach (current uid) $ _actionStackConfig_do
        , fmap (const ASCUndo) _actionStackConfig_undo
        , fmap (const ASCRedo) _actionStackConfig_redo
        , fmap (const ASCClear) _actionStackConfig_clear
      ]

    -- Wedge values:
    -- Here is element that was just added to do stack
    -- There is element that was just added to undo stack
    -- Nowhere is everything else
    foldfn :: (ASCmd a) -> InternalStack a -> PushM t (InternalStack a)
    foldfn (ASCDo x) (_, xs, _)  = return (Here x, x:xs, []) -- clear undo stack on each new do
    foldfn ASCUndo (_, [], ys)   = return (Nowhere, [], ys)
    foldfn ASCUndo (_, x:xs, ys) = return (There x, xs, x:ys)
    foldfn ASCRedo (_, xs, [])   = return (Nowhere, xs, [])
    foldfn ASCRedo (_, xs, y:ys) = return (Here y, y:xs, ys)
    foldfn ASCClear (_, _, _)    = return (Nowhere, [], [])

  -- internal stack state
  asdyn :: Dynamic t (InternalStack a) <-
    foldDynM foldfn (Nowhere, [], []) changeEvent

  -- internal id state to track each new action added to the ActionStack
  let
    firstId = 0
  uid :: Dynamic t UID <-
    foldDyn (const (+1)) firstId _actionStackConfig_do

  let
    changedEv :: Event t (Wedge (IDAct a) (IDAct a))
    changedEv = fmap (\(x,_,_)->x) (updated asdyn)

    doEv = fmapMaybe getHere changedEv
    undoEv = fmapMaybe getThere changedEv

    doIdEv :: Event t (DMap.DMap (Const2 UID ()) Identity)
    doIdEv = fmap (\x -> DMap.singleton (Const2 $ fst x) mempty) doEv
    undoIdEv :: Event t (DMap.DMap (Const2 UID ()) Identity)
    undoIdEv = fmap (\x -> DMap.singleton (Const2 $ fst x) mempty) undoEv

    selectDoAction :: UID -> (Event t (), Event t ())
    selectDoAction i = (select (fan doIdEv) (Const2 i), select (fan undoIdEv) (Const2 i))

  -- this efficiently generates the do and undo events for the next action that will be added to the list
  nextDoAction <- foldDyn (\i _ -> selectDoAction i) (selectDoAction firstId) $ updated uid

  return
    ActionStack {
      _actionStack_do = fmap snd doEv
      , _actionStack_undo   = fmap snd undoEv
      , _actionStack_eventsForNextDoAction = current nextDoAction

    }
