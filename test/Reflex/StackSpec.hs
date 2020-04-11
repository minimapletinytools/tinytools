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

{-
queue_network :: TestApp t m Int [Int]
queue_network ev = mdo
  let
    changed = updated (dl_contents dl)
    -- add from beginning
    addEvent = fmap (\x -> (0,x)) ev
    -- remove from end, this kind of knot tying does not work in reflex
    --changedMap xs = if length xs > 10 then Just 10 else Nothing
    --removeEvent = fmapMaybe changedMap changed
    removeEvent = never
    mdl = defaultModifyDynamicList {
        mdl_add = addEvent
        , mdl_remove = removeEvent
      }
  dl <- holdDynamicList [] mdl
  --_ <- performEvent $ fmap (const (print "hi")) (updated $ dl_contents dl)
  return changed


-- use list as a queue of fixed size
queue_test :: Test
queue_test = TestLabel "queue" $ TestCase $ do
  let
    bs = [1..10] :: [Int]
    run = playReflexSeq bs queue_network
  v <- liftIO run
  print v
  {-let
    expected = fmap Just . L.tail . scanl (\acc x -> x:acc) [] $ bs
  expected @?= v-}
-}

--
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
