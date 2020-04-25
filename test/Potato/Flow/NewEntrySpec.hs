{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.NewEntrySpec
  ( spec
  )
where

import           Relude                   hiding (empty, fromList)

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit (fromHUnitTest)
import           Test.HUnit

import           Reflex
import           Reflex.Test.Host

import qualified Data.List                as L (last)
import qualified Data.Text                as T
import           Data.These
--import           Text.Pretty.Simple       (pPrint)

import           Potato.Flow
import           Potato.Flow.Testing



save_network
  :: forall t m. (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (Event t FCmd -> PerformEventT t m (Event t SEltTree))
save_network ev = do
  pfo <- setup_network ev
  return $ _pfo_saved pfo


bs_save_0 :: ([FCmd],[FCmd])
bs_save_0 =
  ([FCAddElt (SEltBox simpleSBox), FCUndo, FCRedo, FCSave]
  , [FCAddElt (SEltBox simpleSBox), FCSave])

bs_save_1 :: ([FCmd],[FCmd])
bs_save_1 =
  ([FCCustom_Add_SBox_1, FCCustom_CBox_1 0, FCUndo, FCRedo, FCCustom_Add_SBox_1, FCSave]
  , [FCCustom_Add_SBox_1, FCCustom_Add_SBox_1, FCCustom_CBox_1 0, FCSave])

bs_save_2 :: ([FCmd],[FCmd])
bs_save_2 =
  ([FCCustom_Add_SBox_1, FCCustom_Add_SBox_1, FCDeleteElt 1, FCUndo, FCUndo, FCUndo, FCUndo, FCSave]
  , [FCSave])

bs_save_3 :: ([FCmd],[FCmd])
bs_save_3 =
  ([FCCustom_Add_SBox_1, FCDeleteElt 0, FCUndo, FCRedo, FCSave]
  , [FCSave])

-- TODO maybe drop the `t ~ SpiderTimeline Global` constraint
-- you'll need to modify reflex-test-host for this
pair_test :: forall t m a. (t ~ SpiderTimeline Global, m ~ SpiderHost Global, Eq a, Show a)
  => Text
  -> (Event t FCmd -> PerformEventT t m (Event t a))
  -> ([FCmd],[FCmd])
  -> Test
pair_test name network (bs1, bs2) = TestLabel ("pairs: " ++ T.unpack name) $ TestCase $ do
  let
    run1 = runAppSimple network bs1
    run2 = runAppSimple network bs2
  v1 <- liftIO run1
  v2 <- liftIO run2
  L.last (join v1) @?= L.last (join v2)

nstep_test :: forall t m.
  (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => Int -> Test
nstep_test n0 = TestLabel (show n0 <> " steps") $ TestCase $ runSpiderHost $ do
  appFrame <- getAppFrame step_state_network ()
  let
    loop 0 _ = return ()
    loop n st = do
      action <- liftIO $ randomActionFCmd True st
      --action <- return FCCustom_Add_SBox_1
      --liftIO $ print action
      _ <- tickAppFrame appFrame $ Just $ That action
      out <- tickAppFrame appFrame $ Just $ That FCNone
      --liftIO $ do
        --putStrLn $ "ticked: " <> show (fst out)
        --threadDelay 10000
        --hasStats <- getRTSStatsEnabled
        --when (not hasStats) $ error "no stats"
        --stats <- getRTSStats
        --print (toImportant stats)
      case L.last out of
        (nst, _) -> do
          --liftIO $ print "ticked"
          --liftIO $ print nst
          loop (n-1) nst
  loop n0 []

undoredo_test :: forall t m.
  (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => Int -> Test
undoredo_test n0 = TestLabel (show n0 <> " undos") $ TestCase $ runSpiderHost $ do
  appFrame <- getAppFrame step_state_network ()
  let
    m0 = 100 -- num commands to do to set up state
    l0 = 100 -- num commands to do and the undo
    setupLoop 0 st = return st
    setupLoop n st = do
      action <- liftIO $ randomActionFCmd False st
      _ <- tickAppFrame appFrame $ Just $ That action
      out <- tickAppFrame appFrame $ Just $ That FCNone
      case L.last out of
        (nst, _) -> setupLoop (n-1) nst
    undoredoLoop _ 0 st = return st
    undoredoLoop isUndo n st = do
      _ <- tickAppFrame appFrame $ Just $ That (if isUndo then FCUndo else FCRedo)
      out <- tickAppFrame appFrame $ Just $ That FCNone
      case L.last out of
        (nst, _) -> undoredoLoop isUndo (n-1) nst
  forM_ [1..n0] $ \_ -> do
    st0 <- setupLoop m0 []
    st1 <- setupLoop l0 st0
    st2 <- undoredoLoop True l0 st1
    st3 <- undoredoLoop False l0 st2
    liftIO (st2 @?= st0)
    liftIO (st3 @?= st1)


spec :: Spec
spec = do
  describe "Potato Flow" $ do
    fromHUnitTest $ pair_test "save0" save_network bs_save_0
    fromHUnitTest $ pair_test "save1" save_network bs_save_1
    fromHUnitTest $ pair_test "save2" save_network bs_save_2
    fromHUnitTest $ pair_test "save3" save_network bs_save_3
    fromHUnitTest $ undoredo_test 100
    fromHUnitTest $ nstep_test 50000