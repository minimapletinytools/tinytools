{-# LANGUAGE RecursiveDo #-}

module Reflex.ListSpec (
  spec
) where

import           Relude

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit (fromHUnitTest)
import           Test.HUnit

import qualified Data.List                as L (last, tail)

import           Reflex
import           Reflex.List
import           Reflex.TestHarness

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


-- basic test case, add to list on each event tick
push_enqueue_pop_dequeue_test :: Test
push_enqueue_pop_dequeue_test = TestLabel "push/enqueue/pop/dequeue" $ TestCase $ do
  let
    bs = [0,1,0,1,0,1,0,1,2,3,3] :: [Int]
    network ev = do
      let
        mdl = defaultModifyDynamicList {
            mdl_push = fmapMaybe (\x -> if x `mod` 4 == 0 then Just x else Nothing) ev
            , mdl_enqueue = fmapMaybe (\x -> if x `mod` 4 == 1 then Just x else Nothing) ev
            , mdl_pop = fmapMaybe (\x -> if x `mod` 4 == 2 then Just () else Nothing) ev
            , mdl_dequeue = fmapMaybe (\x -> if x `mod` 4 == 3 then Just () else Nothing) ev
          }
      dl <- holdDynamicList [] mdl
      return $ updated (dl_contents dl)
    run = playReflexSeq bs network
  v <- liftIO run
  let
    expected = Just [0,0,0,1,1]
  L.last v @?= expected

-- basic test case, add to list on each event tick
add_test :: Test
add_test = TestLabel "add" $ TestCase $ do
  let
    bs = [1..10] :: [Int]
    network ev = do
      let
        mdl = defaultModifyDynamicList {
            mdl_add = (fmap (\x -> (0,x)) ev)
          }
      dl <- holdDynamicList [] mdl
      return $ updated (dl_contents dl)
    run = playReflexSeq bs network
  v <- liftIO run
  let
    expected = fmap Just . L.tail . scanl (\acc x -> x:acc) [] $ bs
  v @?= expected

spec :: Spec
spec = do
  describe "List" $ do
    fromHUnitTest add_test
    fromHUnitTest queue_test
    fromHUnitTest push_enqueue_pop_dequeue_test
