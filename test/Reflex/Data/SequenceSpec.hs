{-# LANGUAGE RecursiveDo #-}

module Reflex.Data.SequenceSpec (
  spec
) where

import           Relude                   hiding (empty, fromList)

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit (fromHUnitTest)
import           Test.HUnit

import qualified Data.List                as L (last)
import           Data.Sequence

import           Reflex
import           Reflex.Data.Sequence
import           Reflex.Potato.Helpers

import           Reflex.Test.App

data SeqCmd a = SCInsert (Int, [a]) | SCRemove (Int, Int) | SCClear deriving (Eq, Show)

seq_network ::
  forall t m a. (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (Event t (SeqCmd a) -> PerformEventT t m (Event t (Seq a)))
seq_network ev = do
  let
    insertEv = flip fmapMaybe ev $ \case
      SCInsert (i, a) -> Just (i, fromList a)
      _ -> Nothing
    removeEv = flip fmapMaybe ev $ \case
      SCRemove x -> Just x
      _ -> Nothing
    clearEv = flip fmapMaybe ev $ \case
      SCClear -> Just ()
      _ -> Nothing
    dseqc = DynamicSeqConfig {
        _dynamicSeqConfig_insert = insertEv
        , _dynamicSeqConfig_remove = removeEv
        , _dynamicSeqConfig_clear = clearEv
      }
  dseq <- holdDynamicSeq empty dseqc
  return $ updated $ _dynamicSeq_contents dseq

basic_test :: Test
basic_test = TestLabel "basic" $ TestCase $ do
  let
    bs = [SCInsert (0,[1..10]), SCClear, SCInsert (0,[1..10]), SCRemove (5,5), SCInsert (3,[100])] :: [SeqCmd Int]
    run :: IO [[Maybe (Seq Int)]]
    run = runAppSimple seq_network bs
  v <- liftIO run
  L.last v @?= [Just (fromList [1,2,3,100,4,5])]


spec :: Spec
spec = do
  describe "Sequence" $ do
    fromHUnitTest basic_test
