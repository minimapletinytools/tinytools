{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Reflex.Stack (
  DynamicStack(..)
  , DynamicStack(..)
  , defaultModifyDynamicStack
  , holdDynamicStack
) where

import           Relude

import           Reflex

import           Control.Monad.Fix

import           Data.Dependent.Sum
import           Data.List.Index
import           Data.Wedge


data DynamicStack t a = DynamicStack {
  ds_pushed   :: Event t a
  , ds_popped :: Event t a
}

data ModifyDynamicStack t a = ModifyDynamicStack {
  -- first tuple is method producing element to add from an event of when the element is removed
  mds_push_rec :: (Reflex t) => (Event t () -> PushM t a, Event t ())
  , mds_pop    :: Event t ()
}

defaultModifyDynamicStack :: (Reflex t) => ModifyDynamicStack t a
defaultModifyDynamicStack = ModifyDynamicStack {
    mds_push_rec = (undefined, never)
    , mds_pop = never
  }

-- | create a dynamic list
holdDynamicStack ::
  forall t m a. (Reflex t, MonadHold t m, MonadFix m)
  => [a]
  -> ModifyDynamicStack t a
  -> m (DynamicStack t a)
holdDynamicStack initial (ModifyDynamicStack {..}) = mdo
  let
    -- left is add, right is remove
    changeEvent :: Event t (NonEmpty (Either () ()))
    changeEvent = mergeList [fmap Left $ snd mds_push_rec, fmap Right mds_pop]

    -- here is add
    -- there is remove
    foldfn :: Either () () -> (Wedge a a, [a]) -> PushM t (Wedge a a, [a])
    foldfn (Left ()) (_, xs) = do
      let
        removeEltEvent = fmapMaybe (\n-> if n == length xs - 1 then Just () else Nothing) popAtEvent
      x <- fst mds_push_rec removeEltEvent
      return (Here x, x:xs)
    foldfn (Right ()) (_, []) = return (Nowhere, [])
    foldfn (Right ()) (_, (x:xs)) = return (There x, xs)

    -- this is prob something like flip (foldM (flip foldfn))
    foldfoldfn :: [Either () ()] -> (Wedge a a, [a]) -> PushM t (Wedge a a, [a])
    foldfoldfn [] b     = return b
    foldfoldfn (a:as) b = foldfn a b >>= foldfoldfn as

  dynInt :: Dynamic t (Wedge a a, [a]) <- foldDynM foldfoldfn (Nowhere, []) (fmap toList changeEvent)

  let
    evInt = fmap fst (updated dynInt)

    evPushSelect c = case c of
      Here x -> Just x
      _      -> Nothing
    evPopSelect c = case c of
      There x -> Just x
      _       -> Nothing

    popEvent = fmapMaybe evPopSelect evInt
    popAtEvent = tag (fmap (length . snd) (current dynInt)) popEvent

  return $ DynamicStack {
      ds_pushed = fmapMaybe evPushSelect evInt
      , ds_popped = popEvent
    }
