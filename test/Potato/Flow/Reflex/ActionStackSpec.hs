{-# LANGUAGE RecursiveDo #-}

module Potato.Flow.Reflex.ActionStackSpec (
  spec
) where

import           Relude

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit       (fromHUnitTest)
import           Test.HUnit

import qualified Data.List                      as L (last)

import           Potato.Flow.Reflex.ActionStack

import           Reflex
import           Reflex.Potato
import           Reflex.TestHarness

data TestCmd a = TCDo a | TCUndo | TCRedo | TCClear deriving (Eq, Show)

simple_state_network ::
  forall t a s m.
  (a -> s -> s) -- ^ do/redo method to transform state
  -> (a -> s -> s) -- ^ undo method to transform state
  -> s -- ^ initial state
  -> TestApp t m (TestCmd a) s -- ^ test app producing final state
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

    mas = ModifyActionStack {
        mas_do = doEv
        , mas_undo = undoEv
        , mas_redo = redoEv
        , mas_clear = clearEv
      }
  as :: ActionStack t a <- holdActionStack mas
  adder :: Dynamic t s <- foldDynMergeWith initial [fmap fdo (as_do as), fmap fundo (as_undo as)]
  return $ updated adder

adder_test :: Test
adder_test = TestLabel "adder app" $ TestCase $ do
  let
    bs = [TCDo 1, TCDo 2, TCDo 3, TCUndo, TCUndo, TCRedo, TCDo 100]
    run = playReflexSeq bs (simple_state_network (+) (flip (-)) 0)
  v <- liftIO run
  L.last v @?= Just 103

spec :: Spec
spec = do
  describe "ActionStack" $ do
    fromHUnitTest adder_test
