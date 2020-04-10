{-# LANGUAGE RecordWildCards #-}
module Potato.Flow.Reflex.ActionStack (

) where

import           Relude

import           Reflex

{-
data Action = Action {

}
data ModifyActionStack t a = ModifyActionStack {
  mas_do     :: Event t a
  , mas_undo :: Event t ()
  , mas_redo :: Event t ()
}

data ActionStack t a = ActionStack {
  as_do     :: Event t a
  , as_undo :: Event t a
  , as_doStack :: Dynamic t [a] -- ^ stack of actions we've done
  , as_undoStack :: Dynamic t [a] -- ^ stack of actions we've undone
}

[H]   holdDyn       ::                             a -> Event a -> m (Dynamic a)
[H]   foldDyn       :: (a -> b ->           b ) -> b -> Event a -> m (Dynamic b)
[H]   foldDynMaybe  :: (a -> b ->     Maybe b ) -> b -> Event a -> m (Dynamic b)
[H]   foldDynM      :: (a -> b -> m'        b ) -> b -> Event a -> m (Dynamic b)
[H]   foldDynMaybeM :: (a -> b -> m' (Maybe b)) -> b -> Event a -> m (Dynamic b)

mkActionStack ::
  forall t a. (Reflex t, MonadHold t m)
  => ModifyActionStack t a
  -> m (ActionStack t a)
mkActionStack (ModifyActionStack { .. }) = do

  r_as_doStack =
  r_as_undoStack =
  r_as_undo =
  r_as_redo =
  return
    ActionStack {
      as_do = leftmost [mas_do, r_as_redo]
      , as_undo = r_as_undo
      , as_doStack = r_as_doStack
      , as_undoStack = r_as_undoStack
    }



-- Note, these sigs prob won't work due to not being able to access previous value of a dyn after it changes

-- | element added to stack
pushEv :: Dynamic t [a] -> Event t a
pushEv = undefined

popEv :: Dynamic t [a] -> Event t a
popEv = undefined
-}
