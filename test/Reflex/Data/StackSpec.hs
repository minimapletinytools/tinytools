{-# LANGUAGE RecursiveDo #-}

module Reflex.Data.StackSpec (
  spec
) where

import           Relude

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit (fromHUnitTest)
import           Test.HUnit

import qualified Data.List                as L (last)

import           Reflex
import           Reflex.Data.Stack
import           Reflex.Potato.Helpers
import           Reflex.Test.App

getLeft :: Either a b -> Maybe a
getLeft (Left x) = Just x
getLeft _        = Nothing

getRight :: Either a b -> Maybe b
getRight (Right x) = Just x
getRight _         = Nothing



data TestCmd a = TCPush a | TCPop | TCClear deriving (Eq, Show)

simple_state_network ::
  forall t a s m. (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (a -> s -> s) -- ^ do/redo method to transform state
  -> (a -> s -> s) -- ^ undo method to transform state
  -> s -- ^ initial state
  -> (Event t (TestCmd a) -> PerformEventT t m (Event t s)) -- ^ test app producing final state
simple_state_network fdo fundo initial ev = do
  let
    pushEv = flip fmapMaybe ev $ \case
      TCPush n -> Just n
      _ -> Nothing
    popEv = flip fmapMaybe ev $ \case
      TCPop -> Just ()
      _ -> Nothing
    clearEv = flip fmapMaybe ev $ \case
      TCClear -> Just ()
      _ -> Nothing

    mds = DynamicStackConfig {
        _dynamicStackConfig_push = pushEv
        , _dynamicStackConfig_pop = popEv
        , _dynamicStackConfig_clear = clearEv
      }
  ds :: DynamicStack t a <- holdDynamicStack [] mds
  adder :: Dynamic t s <- foldDynMergeWith initial [fmap fdo (_dynamicStack_pushed ds), fmap fundo (_dynamicStack_popped ds)]
  return $ updated adder

adder_test :: Test
adder_test = TestLabel "adder app" $ TestCase $ do
  let
    bs = fmap TCPush [1..4] <> fmap (const TCPop) [(),(),(),(),(),(),()] <> fmap TCPush [100]
    run = runAppSimple (simple_state_network (+) (flip (-)) (0 :: Int)) bs
  v <- liftIO run
  L.last v @?= [Just 100]


clear_test_network ::
  forall t m. (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (Event t (TestCmd Int) -> PerformEventT t m (Event t [Int]))
clear_test_network ev = do
  let
    pushEv = flip fmapMaybe ev $ \case
      TCPush n -> Just n
      _ -> Nothing
    popEv = fmapMaybe (\x -> if x == TCPop then Just () else Nothing) ev
    clearEv = fmapMaybe (\x -> if x == TCClear then Just () else Nothing) ev

    mds = DynamicStackConfig {
        _dynamicStackConfig_push = pushEv
        , _dynamicStackConfig_pop = popEv
        , _dynamicStackConfig_clear = clearEv
      }
  ds :: DynamicStack t Int <- holdDynamicStack [] (mds :: DynamicStackConfig t Int)
  return $ updated (_dynamicStack_contents ds)

clear_test :: Test
clear_test = TestLabel "clear" $ TestCase $ do
  let
    bs = fmap TCPush [1..13] <> fmap (const TCPop) [(),(),()] <> [TCClear] <> fmap TCPush [100]
    run = runAppSimple clear_test_network bs
  v <- liftIO run
  L.last v @?= [Just [100]]


basic_test_network ::
  forall t m. (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (Event t (Either Int ()) -> PerformEventT t m (Event t [Int]))
basic_test_network ev = do
  let
    pushEv = fmapMaybe getLeft ev
    popEv = fmapMaybe getRight ev

    mds = DynamicStackConfig {
        _dynamicStackConfig_push = pushEv
        , _dynamicStackConfig_pop = popEv
        , _dynamicStackConfig_clear = never
      }
  ds :: DynamicStack t Int <- holdDynamicStack [] (mds :: DynamicStackConfig t Int)
  return $ updated (_dynamicStack_contents ds)

basic_test :: Test
basic_test = TestLabel "basic" $ TestCase $ do
  let
    bs = fmap Left [1..13] <> fmap Right [(),(),()]  :: [Either Int ()]
    run = runAppSimple basic_test_network bs
  v <- liftIO run
  L.last v @?= [Just (drop (length (rights bs)) . reverse $ lefts bs)]

spec :: Spec
spec = do
  describe "Stack" $ do
    fromHUnitTest basic_test
    fromHUnitTest clear_test
    fromHUnitTest adder_test
