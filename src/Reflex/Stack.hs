{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Reflex.Stack (
  DynamicStack(..)
  , ModifyDynamicStack(..)
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
  ds_pushed     :: Event t a
  , ds_popped   :: Event t a
  , ds_poppedAt :: Event t (Int, a) -- ^ same as ds_popped but contains size of stack AFTER popping
  , ds_contents :: Dynamic t [a]
}

data ModifyDynamicStack t a = ModifyDynamicStack {
  -- first tuple is method producing element to add from an event of when the element is removed
  --mds_push_rec :: (Reflex t) => (Event t () -> PushM t a, Event t ())
  mds_push_rec :: Event t (Event t () -> PushM t a)
  , mds_pop    :: Event t () -- ^ event to pop an elt from the stack
  , mds_clear  :: Event t () -- ^ event to clear the stack, this does NOT trigger any pop events!!
}

-- I can't seem to instantiate from this without getting a could not deduce Reflex t0 error
-- it can't seem to match the t inside and the t outside? I don't understand
defaultModifyDynamicStack :: (Reflex t) => ModifyDynamicStack t a
defaultModifyDynamicStack = ModifyDynamicStack {
    mds_push_rec = never
    , mds_pop = never
    , mds_clear = never
  }

-- helper type for holdDynamicStack
data DSCmd t a = DSCPush (Event t () -> PushM t a) | DSCPop | DSCClear

-- | create a dynamic list
holdDynamicStack ::
  forall t m a. (Reflex t, MonadHold t m, MonadFix m)
  => [a]
  -> ModifyDynamicStack t a
  -> m (DynamicStack t a)
holdDynamicStack initial (ModifyDynamicStack {..}) = mdo
  let

    -- switch to leftmost? These events should never trigger at the same time
    changeEvent :: Event t (NonEmpty (DSCmd t a))
    changeEvent = mergeList [
        fmap DSCPush $ mds_push_rec
        , fmap (const DSCPop) mds_pop
        , fmap (const DSCClear) mds_clear
      ]

    -- Wedge values:
    -- Here is element that was just added
    -- There is element that was just removed
    -- Nowhere is initial state or just popped an empty stack or after a clear
    foldfn :: (DSCmd t a) -> (Wedge a a, [a]) -> PushM t (Wedge a a, [a])
    foldfn (DSCPush makeEltCb) (_, xs) = do
      let
        -- n is length of stack AFTER popping
        -- xs is stack BEFORE adding elt
        -- hence if (n == length xs), x matches index of element that got popped
        removeEltEvent = fmapMaybe (\n-> if n == length xs then Just () else Nothing) (fmap fst popAtEvent)
        --removeEltEvent = fmapMaybe (const (Just ())) (traceEvent (show (length xs)) popAtEvent)
      x <- makeEltCb removeEltEvent
      return (Here x, x:xs)
    foldfn DSCPop (_, []) = return (Nowhere, [])
    foldfn DSCPop (_, (x:xs)) = return (There x, xs)
    foldfn DSCClear (_, _) = return (Nowhere, [])

    -- this is prob something like flip (foldM (flip foldfn))
    foldfoldfn :: [(DSCmd t a)] -> (Wedge a a, [a]) -> PushM t (Wedge a a, [a])
    foldfoldfn [] b     = return b
    foldfoldfn (a:as) b = foldfn a b >>= foldfoldfn as

  dynInt :: Dynamic t (Wedge a a, [a]) <- foldDynM foldfoldfn (Nowhere, []) (fmap toList changeEvent)

  let
    evInt :: Event t (Wedge a a)
    evInt = fmap fst (updated dynInt)

    evPushSelect c = case c of
      Here x -> Just x
      _      -> Nothing
    evPopSelect c = case c of
      There x -> Just x
      _       -> Nothing

    popEvent :: Event t a
    popEvent = fmapMaybe evPopSelect evInt
    popAtEvent :: Event t (Int, a)
    popAtEvent = attach (fmap ((+ (-1)) . length . snd) (current dynInt)) popEvent

  return $ DynamicStack {
      ds_pushed = fmapMaybe evPushSelect evInt
      , ds_popped = popEvent
      , ds_poppedAt = popAtEvent
      , ds_contents = fmap snd dynInt
    }
