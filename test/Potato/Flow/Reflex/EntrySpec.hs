{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.EntrySpec
  ( spec
  )
where

import           Relude                           hiding (empty, fromList)

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit         (fromHUnitTest)
import           Test.HUnit

import           Reflex
import           Reflex.Test.Host

import           Control.Concurrent
import           Data.Aeson
import qualified Data.List                        as L (last)
import           Data.Maybe                       (fromJust)
import qualified Data.Text                        as T
import           Data.These
import           GHC.Stats
--import           Text.Pretty.Simple       (pPrint)

import           Potato.Flow
import           Potato.Flow.Reflex.Entry.Testing



-- bespoke testing

save_network
  :: forall t m. (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (Event t FCmd -> TestGuestT t m (Event t SEltTree))
save_network ev = do
  pfo <- setup_network ev
  return $ fmap _sPotatoFlow_sEltTree $ _pfo_saved pfo

-- second one is "expected"
bs_save_0 :: ([FCmd],[FCmd])
bs_save_0 =
  ([FCAddElt 0 (SEltBox simpleSBox), FCUndo, FCRedo, FCSave]
  , [FCAddElt 0 (SEltBox simpleSBox), FCSave])

bs_save_1 :: ([FCmd],[FCmd])
bs_save_1 =
  ([FCCustom_Add_SBox_1, FCCustom_CBox_1 0, FCUndo, FCRedo, FCCustom_Add_SBox_1, FCSave]
  , [FCCustom_Add_SBox_1, FCCustom_Add_SBox_1, FCCustom_CBox_1 1, FCSave])

bs_save_2 :: ([FCmd],[FCmd])
bs_save_2 =
  ([FCCustom_Add_SBox_1, FCCustom_Add_SBox_1, FCDeleteElts [1], FCUndo, FCUndo, FCUndo, FCUndo, FCSave]
  , [FCSave])

bs_save_3 :: ([FCmd],[FCmd])
bs_save_3 =
  ([FCCustom_Add_SBox_1, FCDeleteElts [0], FCUndo, FCRedo, FCSave]
  , [FCSave])

bs_save_4 :: ([FCmd],[FCmd])
bs_save_4 =
  ([FCAddElt 0 SEltFolderStart, FCAddElt 1 SEltFolderStart, FCAddElt 1 SEltFolderStart, FCSave]
  , [FCAddElt 0 SEltFolderStart, FCAddElt 1 SEltFolderStart, FCAddElt 3 SEltFolderStart, FCUndo, FCUndo, FCRedo, FCRedo, FCSave])

bs_save_5 :: ([FCmd],[FCmd])
bs_save_5 =
  ([FCAddElt 0 SEltFolderStart, FCSave, FCNone, FCSave, FCUndo, FCSave]
  , [FCAddElt 0 SEltFolderStart, FCNone, FCUndo, FCSave])

-- test multi-delete
bs_save_6 :: ([FCmd],[FCmd])
bs_save_6 =
  ([FCCustom_Add_SBox_1, FCCustom_Add_SBox_1, FCCustom_Add_SBox_1, FCCustom_Add_SBox_1, FCDeleteElts [0..3], FCUndo, FCRedo, FCSave]
  , [FCSave])

-- test multi-delete
bs_save_7 :: ([FCmd],[FCmd])
bs_save_7 =
  ([FCCustom_Add_SBox_1, FCCustom_Add_SBox_1, FCCustom_Add_SBox_1, FCCustom_Add_SBox_1, FCDeleteElts [1,2], FCUndo, FCDeleteElts [2,3], FCCustom_Add_SBox_1, FCSave]
  , [FCCustom_Add_SBox_1, FCCustom_Add_SBox_1, FCCustom_Add_SBox_1, FCSave])

-- copy paste
bs_save_8 :: ([FCmd],[FCmd])
bs_save_8 =
  ([FCAddElt 0 SEltFolderStart, FCCustom_Add_SBox_1, FCCopy [0], FCPaste 3, FCSave]
  , [FCCustom_Add_SBox_1,  FCAddElt 0 SEltFolderStart, FCCustom_Add_SBox_1, FCSave])

-- TODO maybe drop the `t ~ SpiderTimeline Global` constraint
-- you'll need to modify reflex-test-host for this
pair_test :: forall t m a. (t ~ SpiderTimeline Global, m ~ SpiderHost Global, Eq a, Show a)
  => Text
  -> (Event t FCmd -> TestGuestT t m (Event t a))
  -> ([FCmd],[FCmd])
  -> Test
pair_test name network (bs1, bs2) = TestLabel ("pairs: " ++ T.unpack name) $ TestCase $ do
  let
    run1 = runAppSimple network bs1
    run2 = runAppSimple network bs2
  v1 <- liftIO run1
  v2 <- liftIO run2
  L.last (join v1) @?= L.last (join v2)



-- randomized testing

data FCmdType = AllCmd | ActionOnly | UndoOnly | RedoOnly

doStuff :: forall t m. (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => AppFrame t () FCmd () SPotatoFlow m
  -> FCmdType
  -> Int
  -> SEltTree
  -> m SEltTree
doStuff _ _ 0 st = return st
doStuff appFrame fcmdType n st = do
  action <- case fcmdType of
    UndoOnly   -> return FCUndo
    RedoOnly   -> return FCRedo
    AllCmd     -> liftIO $ randomActionFCmd True st
    ActionOnly -> liftIO $ randomActionFCmd False st
  _ <- tickAppFrame appFrame $ Just $ That action
  out <- tickAppFrame appFrame $ Just $ That FCSave
  case L.last out of
    (_, mspf) -> case mspf of
      Nothing  -> error "expected state"
      Just spf -> doStuff appFrame fcmdType (n-1) (_sPotatoFlow_sEltTree spf)


nstep_test :: forall t m.
  (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => Int -> Test
nstep_test n0 = TestLabel (show n0 <> " steps") $ TestCase $ runSpiderHost $ do
  appFrame <- getAppFrame step_state_network ()
  void $ doStuff appFrame AllCmd n0 []

undoredo_test :: forall t m.
  (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => Int -> Test
undoredo_test n0 = TestLabel (show n0 <> " undos") $ TestCase $ runSpiderHost $ do
  appFrame <- getAppFrame step_state_network ()
  let
    m0 = 100 -- num commands to do to set up state
    l0 = 100 -- num commands to do and the undo
  forM_ [1..n0] $ \_ -> do
    st0 <- doStuff appFrame ActionOnly m0 []
    st1 <- doStuff appFrame ActionOnly l0 st0
    st2 <- doStuff appFrame UndoOnly l0 st1
    st3 <- doStuff appFrame RedoOnly l0 st2
    liftIO (st2 @?= st0)
    liftIO (st3 @?= st1)

leak_test :: forall t m.
  (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => Int -> Test
leak_test maxBytes = TestLabel (show maxBytes <> " leaks") $ TestCase $ runSpiderHost $ do
  appFrame <- getAppFrame step_state_network ()
  let
    m0 = 100 -- num commands to do to set up state
    l0 = 100 -- num commands to do and the undo
  st0 <- doStuff appFrame ActionOnly m0 []
  forM_ [1..(1000 :: Int)] $ \_ -> do
    st1 <- doStuff appFrame ActionOnly l0 st0
    _ <- doStuff appFrame UndoOnly l0 st1
    liftIO $ do
      threadDelay 1000
      stats <- getRTSStats
      --print $ fromIntegral $ max_live_bytes stats
      True @?= (fromIntegral $ max_live_bytes stats) < maxBytes

serialization_test :: forall t m.
  (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => Test
serialization_test = TestLabel "serialization" $ TestCase $ runSpiderHost $ do
  appFrame <- getAppFrame step_state_network ()
  final <- doStuff appFrame AllCmd 1000 []
  let
    jsontree = encode final
    mfinal' = decode jsontree
  liftIO $ do
    final @?= fromJust mfinal'
    encodeFile "serialization_test_output.json" final

save_load_test :: forall t m.
  (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => Test
save_load_test = TestLabel "save load" $ TestCase $ runSpiderHost $ do
  appFrame <- getAppFrame step_state_network ()
  let
    saved = fromJust . snd . L.last
  doStuff appFrame AllCmd 1000 []
  final1 <- fmap saved  $ tickAppFrame appFrame $ Just $ That FCSave
  doStuff appFrame AllCmd 1000 []
  final2 <- fmap saved  $ tickAppFrame appFrame $ Just $ That FCSave
  tickAppFrame appFrame $ Just $ That (FCLoad final1)
  final1' <- fmap saved  $ tickAppFrame appFrame $ Just $ That FCSave
  tickAppFrame appFrame $ Just $ That (FCLoad final2)
  final2' <- fmap saved  $ tickAppFrame appFrame $ Just $ That FCSave
  liftIO $ do
    final1 @?= final1'
    final2 @?= final2'



spec :: Spec
spec = do
  describe "Potato Flow" $ do
    return ()
    fromHUnitTest $ pair_test "save0" save_network bs_save_0
    fromHUnitTest $ pair_test "save1" save_network bs_save_1
    fromHUnitTest $ pair_test "save2" save_network bs_save_2
    fromHUnitTest $ pair_test "save3" save_network bs_save_3
    fromHUnitTest $ pair_test "save4" save_network bs_save_4
    fromHUnitTest $ pair_test "save5" save_network bs_save_5
    fromHUnitTest $ pair_test "save6" save_network bs_save_6
    fromHUnitTest $ pair_test "save7" save_network bs_save_7
    fromHUnitTest $ pair_test "save8" save_network bs_save_8

    fromHUnitTest $ undoredo_test 10
    --fromHUnitTest $ nstep_test 10000
    fromHUnitTest $ serialization_test
    fromHUnitTest $ save_load_test

    -- can't seem to enable GHC stats in tests :(
    --fromHUnitTest $ leak_test 500000
