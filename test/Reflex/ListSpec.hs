{-# LANGUAGE RecursiveDo #-}

module Reflex.ListSpec (
  spec
) where

import           Relude

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit (fromHUnitTest)
import           Test.HUnit

import qualified Data.List                as L (tail)

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
    mdl = ModifyDynamicList addEvent removeEvent never
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
add_test :: Test
add_test = TestLabel "add" $ TestCase $ do
  let
    bs = [1..10] :: [Int]
    network ev = do
      let
        -- insert at beginning
        mdl = ModifyDynamicList (fmap (\x -> (0,x)) ev) never never
        -- insert at end
        --mdl = ModifyDynamicList (fmap (\x -> (x-1,x)) ev) never never
      dl <- holdDynamicList [] mdl
      return $ updated (dl_contents dl)
    run = playReflexSeq bs network
  v <- liftIO run
  let
    expected = fmap Just . L.tail . scanl (\acc x -> x:acc) [] $ bs
  expected @?= v

spec :: Spec
spec = do
  describe "List" $ do
    fromHUnitTest add_test
    fromHUnitTest queue_test
