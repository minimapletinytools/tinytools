{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}
module Reflex.List (
  DynamicList(..)
  , ModifyDynamicList(..)
  , defaultModifyDynamicList
  , holdDynamicList
) where

import           Relude

import           Reflex
import           Reflex.Potato

import           Control.Monad.Fix

import           Data.Dependent.Sum
import           Data.List.Index


-- TODO considering changing indexing to something that doesn't have execution ordering issues / partial
-- TODO this needs to be modified to support adding/removing/moving several elements at once
data DynamicList t a = DynamicList {
  dl_add        :: Event t (Int, a)
  , dl_remove   :: Event t a
  , dl_move     :: Event t (Int, a)
  , dl_contents :: Dynamic t [a]
}

data ModifyDynamicList t a = ModifyDynamicList {
  mdl_add       :: Event t (Int, a)
  , mdl_remove  :: Event t Int

  -- this is slightly different than removing then adding as it can be done in 1 frame
  , mdl_move    :: Event t (Int,Int)

  -- these attach index and follow same code path as add/remove
  , mdl_push    :: Event t a
  , mdl_pop     :: Event t ()
  , mdl_enqueue :: Event t a
  , mdl_dequeue :: Event t ()
}

defaultModifyDynamicList :: (Reflex t) => ModifyDynamicList t a
defaultModifyDynamicList = ModifyDynamicList {
    mdl_add = never
    , mdl_remove = never
    , mdl_move = never
    , mdl_push = never
    , mdl_pop = never
    , mdl_enqueue = never
    , mdl_dequeue = never
  }


data LState a = LSInserted (Int, a) | LSRemoved a | LSMoved (Int, a) | LSNothing

-- modify DynamicList event tag
-- TODO switch to normal ADT for consistency
data MDL x a where
  MDL_add :: MDL x (Int, x)
  MDL_remove :: MDL x Int
  MDL_move :: MDL x (Int, Int) -- move (from, to), to index is after removal

-- | create a dynamic list
holdDynamicList ::
  forall t m a. (Reflex t, MonadHold t m, MonadFix m)
  => [a]
  -> ModifyDynamicList t a
  -> m (DynamicList t a)
holdDynamicList initial (ModifyDynamicList {..}) = mdo
  let
    mdl_push' = fmap (\x -> (0,x)) mdl_push
    mdl_pop' = fmap (const 0) mdl_pop
    mdl_enqueue' = attach (fmap length (current dlc)) mdl_enqueue
    mdl_dequeue' = tag (fmap ((+ (-1)) . length) (current dlc)) mdl_dequeue
    mdlAdd :: Event t (DSum (MDL a) Identity)
    mdlAdd = (MDL_add ==> ) <$> leftmost [mdl_add, mdl_push', mdl_enqueue']
    mdlRemove = (MDL_remove ==> ) <$> leftmost [mdl_remove, mdl_pop', mdl_dequeue']
    mdlMove = (MDL_move ==> ) <$> mdl_move

    -- TODO change to leftmost
    -- ensure these events never fire simultaneously as the indexing may be off
    changeEvent :: Event t (DSum (MDL a) Identity)
    changeEvent = leftmostwarn "WARNING: multiple List events firing at once" [mdlMove, mdlRemove, mdlAdd]

    foldfn ::
      DSum (MDL a) Identity
      -> (LState a, [a])
      -> Maybe (LState a, [a])
    foldfn op (_, xs) =
      let
        add' (index, x) xs' = do
          guard $ index >= 0 && index <= length xs'
          return $ insertAt index x xs'
        add :: (Int, a) -> Maybe (LState a, [a])
        add (index, x) = do
          xs' <- add' (index, x) xs
          return $ (LSInserted (index, x), xs')
        remove' index = do
          x <- xs !!? index
          return $ (x, deleteAt index xs)
        remove :: Int -> Maybe (LState a, [a])
        remove index = do
          (x, xs') <- remove' index
          return $ (LSRemoved x, xs')
        move :: (Int, Int) -> Maybe (LState a, [a])
        move (i1, i2) = do
          (x, xs') <- remove' i1
          xs'' <- add' (i2, x) xs'
          return $ (LSMoved (i2, x), xs'')
      in
        case op of
          (MDL_add :=> Identity (index, x)) -> add (index, x)
          (MDL_remove :=> Identity index)   -> remove index
          (MDL_move :=> Identity (i1, i2))  -> move (i1, i2)

  dynInt :: Dynamic t (LState a, [a]) <-
    foldDynMaybe foldfn (LSNothing, initial) changeEvent

  let
    evInt = fmap fst (updated dynInt)

    evAddSelect c = case c of
      LSInserted x -> Just x
      _            -> Nothing
    evRemoveSelect c = case c of
      LSRemoved x -> Just x
      _           -> Nothing
    evMoveSelect c = case c of
      LSMoved x -> Just x
      _         -> Nothing

    dlc = fmap snd dynInt

  return $ DynamicList {
      dl_add = fmapMaybe evAddSelect evInt
      , dl_remove = fmapMaybe evRemoveSelect evInt
      , dl_move = fmapMaybe evMoveSelect evInt
      , dl_contents = dlc
    }
