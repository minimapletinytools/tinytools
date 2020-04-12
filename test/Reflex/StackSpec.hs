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



adder_network :: forall t m. TestApp t m (Either Int ()) Int
adder_network ev = mdo
  let
    elFactory :: Int -> Event t () -> PushM t (Int, Event t ())
    --elFactory n popped = return (traceEvent "pop" $ (fmap (const n) popped))
    elFactory n popped = return (n, popped)

    pushEv = fmapMaybe getLeft ev
    popEv = fmapMaybe getRight ev

    mds = ModifyDynamicStack {
        mds_push_rec = fmap elFactory pushEv
        , mds_pop = popEv
        , mds_clear = never
      }
    removeEv :: Event t Int
    removeEv = coincidence $ fmap (\(v,e) -> fmap (const v) e) $ ds_popped ds
    addEv :: Event t Int
    addEv = fmap fst $ ds_pushed ds
  ds :: DynamicStack t (Int, Event t ()) <- holdDynamicStack [] mds
  adder :: Dynamic t Int <- foldDyn (+) 0 $ mergeWith (+) [addEv, (fmap negate removeEv)]
  return $ updated adder

adder_test :: Test
adder_test = TestLabel "adder app" $ TestCase $ do
  let
    bs = fmap Left [1,2,3,4] <> fmap Right [(),(),()] <> fmap Left [10] <> fmap Right [()]  :: [Either Int ()]
    run = playReflexSeq bs adder_network
  v <- liftIO run
  --print v
  -- TODO check results
  --fmap isNothing v @?= fmap isLeft bs
  return ()


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
        , mds_clear = never
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



data TestCmd a = TCPush a | TCPop | TCClear deriving (Eq, Show)

clear_test_network :: forall t m. TestApp t m (TestCmd Int) [Int]
clear_test_network ev = do
  let
    pushEv = flip fmapMaybe ev $ \case
      TCPush n -> Just n
      _ -> Nothing
    popEv = fmapMaybe (\x -> if x == TCPop then Just () else Nothing) ev
    clearEv = fmapMaybe (\x -> if x == TCClear then Just () else Nothing) ev

    mds = ModifyDynamicStack {
        mds_push_rec = fmap (\n -> const (return n)) pushEv
        , mds_pop = popEv
        , mds_clear = clearEv
      }
  ds :: DynamicStack t Int <- holdDynamicStack [] (mds :: ModifyDynamicStack t Int)
  return $ updated (ds_contents ds)

clear_test :: Test
clear_test = TestLabel "clear" $ TestCase $ do
  let
    bs = fmap TCPush [1..13] <> fmap (const TCPop) [(),(),()] <> [TCClear] <> fmap TCPush [100]
    run = playReflexSeq bs clear_test_network
  v <- liftIO run
  L.last v @?= Just [100]


basic_test_network :: forall t m. TestApp t m (Either Int ()) [Int]
basic_test_network ev = do
  let
    pushEv = fmapMaybe getLeft ev
    popEv = fmapMaybe getRight ev

    mds = ModifyDynamicStack {
        mds_push_rec = fmap (\n -> const (return n)) pushEv
        , mds_pop = popEv
        , mds_clear = never
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
    fromHUnitTest clear_test
    fromHUnitTest dynamic_test
    fromHUnitTest adder_test
