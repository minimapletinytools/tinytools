{-# LANGUAGE RecursiveDo #-}

module Reflex.StackSpec (
  spec
) where

import           Relude

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit (fromHUnitTest)
import           Test.HUnit

import qualified Data.List                as L (last)

import           Reflex
import           Reflex.Stack
import           Reflex.TestHarness

getLeft :: Either a b -> Maybe a
getLeft (Left x) = Just x
getLeft _        = Nothing

getRight :: Either a b -> Maybe b
getRight (Right x) = Just x
getRight _         = Nothing

dynamic_test_network :: forall t m. TestApp t m (Either Int ()) Int
dynamic_test_network ev = mdo
  let
    -- this element is an event that fire when it's popped
    elFactory :: Int -> Event t () -> PushM t (Event t Int)
    --elFactory n popped = return (traceEvent "pop" $ (fmap (const n) popped))
    elFactory n popped = return $ fmap (const n) popped

    pushEv = fmapMaybe getLeft ev
    popEv = fmapMaybe getRight ev

    mds = ModifyDynamicStack {
        mds_push_rec = fmap elFactory pushEv
        , mds_pop = popEv
      }
    -- this tracks the event of the element that was just popped
    removeEv = coincidence $ ds_popped ds
  ds :: DynamicStack t (Event t Int) <- holdDynamicStack [] mds
  --removeEv <- switchHoldPromptly never $ ds_popped ds
  return $ removeEv

dynamic_test :: Test
dynamic_test = TestLabel "dynamic" $ TestCase $ do
  let
    bs = fmap Left [1..13] <> fmap Right [(),(),()] <> fmap Left [14] <> fmap Right [()]  :: [Either Int ()]
    run = playReflexSeq bs dynamic_test_network
  v <- liftIO run
  --print v
  fmap isNothing v @?= fmap isLeft bs


basic_test_network :: forall t m. TestApp t m (Either Int ()) [Int]
basic_test_network ev = do
  let
    mds = ModifyDynamicStack {
        mds_push_rec = fmapMaybe
          (either (\n -> Just $ const (return n)) (const Nothing)) ev
        , mds_pop = fmapMaybe
          (either (const Nothing) (const $ Just ())) ev
      }
  ds :: DynamicStack t Int <- holdDynamicStack [] (mds :: ModifyDynamicStack t Int)
  return $ updated (ds_contents ds)

basic_test :: Test
basic_test = TestLabel "basic" $ TestCase $ do
  let
    bs = fmap Left [1..13] <> fmap Right [(),(),()]  :: [Either Int ()]
    run = playReflexSeq bs basic_test_network
  v <- liftIO run
  L.last v @?= Just (drop (length (rights bs)) . reverse $ lefts bs)

spec :: Spec
spec = do
  describe "Stack" $ do
    fromHUnitTest basic_test
    fromHUnitTest dynamic_test
