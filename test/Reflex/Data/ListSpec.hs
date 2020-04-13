{-# LANGUAGE RecursiveDo #-}

module Reflex.Data.ListSpec (
  spec
) where

import           Relude

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit  (fromHUnitTest)
import           Test.HUnit

import qualified Data.List                 as L (last, tail)

import           Reflex
import           Reflex.Data.List
import           Reflex.Potato.TestHarness

pushAdd_network :: forall t m. TestApp t m Int [Int]
pushAdd_network ev = mdo
  let
    -- element in the list is a dynamic int that adds to itself each new element added to the list
    -- this includes the element itself!
    pushAddEvent :: Int -> PushM t (Int, Dynamic t Int)
    pushAddEvent n = do
      -- this causes an RTE, maybe a bug?
      --addedEvExcludeSelf <- tailE addedEv
      addedEvExcludeSelf <- return addedEv
      let
        foldfn :: (Int, Dynamic t Int) -> Int -> PushM t (Maybe Int)
        foldfn (_, justAdded) old = do
          addme <- sample . current $ justAdded
          return $ Just (old + addme)
      dyn <- foldDynMaybeM foldfn n addedEvExcludeSelf
      return (0, dyn)
    mdl = defaultDynamicListConfig {
        _dynamicListConfig_add = pushAlways pushAddEvent ev
        , _dynamicListConfig_remove = never
      }
    addedEv = _dynamicList_add dl
  dl <- holdDynamicList [] mdl
  return . updated . join . fmap sequence $ _dynamicList_contents dl


-- use list as a queue of fixed size
pushAdd_test :: Test
pushAdd_test = TestLabel "pushAdd" $ TestCase $ do
  let
    bs = [1,1,1,1,1] :: [Int]
    run = playReflexSeq bs pushAdd_network
  v <- liftIO run
  --print v
  let
    expected = [2,3,4,5,6]
  L.last v @?= Just expected


-- basic test case, add to list on each event tick
push_enqueue_pop_dequeue_test :: Test
push_enqueue_pop_dequeue_test = TestLabel "push/enqueue/pop/dequeue" $ TestCase $ do
  let
    bs = [0,1,0,1,0,1,0,1,2,3,3] :: [Int]
    network ev = do
      let
        mdl = defaultDynamicListConfig {
            _dynamicListConfig_push = fmapMaybe (\x -> if x `mod` 4 == 0 then Just x else Nothing) ev
            , _dynamicListConfig_enqueue = fmapMaybe (\x -> if x `mod` 4 == 1 then Just x else Nothing) ev
            , _dynamicListConfig_pop = fmapMaybe (\x -> if x `mod` 4 == 2 then Just () else Nothing) ev
            , _dynamicListConfig_dequeue = fmapMaybe (\x -> if x `mod` 4 == 3 then Just () else Nothing) ev
          }
      dl <- holdDynamicList [] mdl
      return $ updated (_dynamicList_contents dl)
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
        mdl = defaultDynamicListConfig {
            _dynamicListConfig_add = (fmap (\x -> (0,x)) ev)
          }
      dl <- holdDynamicList [] mdl
      return $ updated (_dynamicList_contents dl)
    run = playReflexSeq bs network
  v <- liftIO run
  let
    expected = fmap Just . L.tail . scanl (\acc x -> x:acc) [] $ bs
  v @?= expected

spec :: Spec
spec = do
  describe "List" $ do
    fromHUnitTest add_test
    fromHUnitTest push_enqueue_pop_dequeue_test
    fromHUnitTest pushAdd_test
