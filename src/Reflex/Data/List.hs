{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}
module Reflex.Data.List (
  DynamicList(..)
  , DynamicListConfig(..)
  , defaultDynamicListConfig
  , holdDynamicList
) where

import           Relude

import           Reflex
import           Reflex.Potato.Helpers

import           Control.Monad.Fix

import           Data.Dependent.Sum
import           Data.List.Index


-- TODO considering changing indexing to something that doesn't have execution ordering issues / partial
-- TODO this needs to be modified to support adding/removing/moving several elements at once
data DynamicList t a = DynamicList {
  _dynamicList_add        :: Event t (Int, a)
  , _dynamicList_remove   :: Event t a
  , _dynamicList_move     :: Event t (Int, a)
  , _dynamicList_contents :: Dynamic t [a]
}

data DynamicListConfig t a = DynamicListConfig {
  _dynamicListConfig_add       :: Event t (Int, a)
  , _dynamicListConfig_addM    :: Event t (PushM t (Int, a))

  , _dynamicListConfig_remove  :: Event t Int

  -- this is slightly different than removing then adding as it can be done in 1 frame
  , _dynamicListConfig_move    :: Event t (Int,Int)

  -- these attach index and follow same code path as add/remove
  , _dynamicListConfig_push    :: Event t a
  , _dynamicListConfig_pop     :: Event t ()
  , _dynamicListConfig_enqueue :: Event t a
  , _dynamicListConfig_dequeue :: Event t ()
}

defaultDynamicListConfig :: (Reflex t) => DynamicListConfig t a
defaultDynamicListConfig = DynamicListConfig {
    _dynamicListConfig_add = never
    , _dynamicListConfig_addM = never
    , _dynamicListConfig_remove = never
    , _dynamicListConfig_move = never
    , _dynamicListConfig_push = never
    , _dynamicListConfig_pop = never
    , _dynamicListConfig_enqueue = never
    , _dynamicListConfig_dequeue = never
  }


data LState a = LSInserted (Int, a) | LSRemoved a | LSMoved (Int, a) | LSNothing

data DLCmd t a = DLCAdd (PushM t (Int, a)) | DLCRemove Int | DLCMove (Int, Int)

-- | create a dynamic list
holdDynamicList ::
  forall t m a. (Reflex t, MonadHold t m, MonadFix m)
  => [a]
  -> DynamicListConfig t a
  -> m (DynamicList t a)
holdDynamicList initial (DynamicListConfig {..}) = mdo
  let
    _dynamicListConfig_add' = fmap return _dynamicListConfig_add
    _dynamicListConfig_push' = fmap return $ fmap (\x -> (0,x)) _dynamicListConfig_push
    _dynamicListConfig_pop' = fmap  (const 0) _dynamicListConfig_pop
    _dynamicListConfig_enqueue' = fmap return $ attach (fmap length (current dlc)) _dynamicListConfig_enqueue
    _dynamicListConfig_dequeue' = tag (fmap ((+ (-1)) . length) (current dlc)) _dynamicListConfig_dequeue

    dlAdd = leftmost $ DLCAdd <<$>> [_dynamicListConfig_add', _dynamicListConfig_addM, _dynamicListConfig_push', _dynamicListConfig_enqueue']
    dlRemove = leftmost $ DLCRemove <<$>> [_dynamicListConfig_remove, _dynamicListConfig_pop', _dynamicListConfig_dequeue']
    dlMove = DLCMove <$> _dynamicListConfig_move

    -- TODO change to leftmost
    -- ensure these events never fire simultaneously as the indexing may be off
    changeEvent :: Event t (DLCmd t a)
    changeEvent = leftmostwarn "WARNING: multiple List events firing at once" [dlMove, dlRemove, dlAdd]

    foldfn ::
      DLCmd t a
      -> (LState a, [a])
      -> PushM t (Maybe (LState a, [a]))
    foldfn op (_, xs) =
      let
        add' (index, x) xs' = do
          guard $ index >= 0 && index <= length xs'
          return $ insertAt index x xs'
        add :: PushM t (Int, a) -> PushM t (Maybe (LState a, [a]))
        add m = do
          (index, x) <- m
          return $ do
            xs' <- add' (index, x) xs
            return $ (LSInserted (index, x), xs')
        remove' index = do
          x <- xs !!? index
          return $ (x, deleteAt index xs)
        remove :: Int -> PushM t (Maybe (LState a, [a]))
        remove index = return $ do
          (x, xs') <- remove' index
          return $ (LSRemoved x, xs')
        move :: (Int, Int) -> PushM t (Maybe (LState a, [a]))
        move (i1, i2) = return $ do
          (x, xs') <- remove' i1
          xs'' <- add' (i2, x) xs'
          return $ (LSMoved (i2, x), xs'')
      in
        case op of
          DLCAdd m         -> add m
          DLCRemove index  -> remove index
          DLCMove (i1, i2) -> move (i1, i2)

  dynInt :: Dynamic t (LState a, [a]) <-
    foldDynMaybeM foldfn (LSNothing, initial) changeEvent

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
      _dynamicList_add = fmapMaybe evAddSelect evInt
      , _dynamicList_remove = fmapMaybe evRemoveSelect evInt
      , _dynamicList_move = fmapMaybe evMoveSelect evInt
      , _dynamicList_contents = dlc
    }
