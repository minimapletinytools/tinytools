{-# LANGUAGE RecursiveDo #-}

module Reflex.Data.ActionStackSpec (
  spec
) where

import           Relude

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit  (fromHUnitTest)
import           Test.HUnit

import qualified Data.List                 as L (last)

import           Reflex
import           Reflex.Data.ActionStack
import           Reflex.Potato.Helpers
import           Reflex.Potato.TestHarness
import           Reflex.Test.App

data TestCmd a = TCDo a | TCUndo | TCRedo | TCClear deriving (Eq, Show)

simple_state_network ::
  forall t a s m. (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (a -> s -> s) -- ^ do/redo method to transform state
  -> (a -> s -> s) -- ^ undo method to transform state
  -> s -- ^ initial state
  -> (Event t (TestCmd a) -> PerformEventT t m (Event t s)) -- ^ test app producing final state
simple_state_network fdo fundo initial ev = do
  let
    doEv = flip fmapMaybe ev $ \case
      TCDo a -> Just a
      _ -> Nothing
    undoEv = flip fmapMaybe ev $ \case
      TCUndo -> Just ()
      _ -> Nothing
    redoEv = flip fmapMaybe ev $ \case
      TCRedo -> Just ()
      _ -> Nothing
    clearEv = flip fmapMaybe ev $ \case
      TCClear -> Just ()
      _ -> Nothing

    mas = ActionStackConfig {
        _actionStackConfig_do = doEv
        , _actionStackConfig_undo = undoEv
        , _actionStackConfig_redo = redoEv
        , _actionStackConfig_clear = clearEv
      }
  as :: ActionStack t a <- holdActionStack mas
  adder :: Dynamic t s <- foldDynMergeWith initial [fmap fdo (_actionStack_do as), fmap fundo (_actionStack_undo as)]
  return $ updated adder

adder_test :: Test
adder_test = TestLabel "adder app" $ TestCase $ do
  let
    bs = [TCDo 1, TCDo 2, TCDo 3, TCUndo, TCUndo, TCRedo, TCDo 100]
    run = runAppSimple (simple_state_network (+) (flip (-)) (0 :: Int)) bs
  v <- liftIO run
  L.last v @?= [Just 103]

spec :: Spec
spec = do
  describe "ActionStack" $ do
    fromHUnitTest adder_test
