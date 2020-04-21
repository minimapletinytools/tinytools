--testing for leaks, nothing to see here

{-# LANGUAGE RecordWildCards #-}

import           Relude

import           GHC.Stats

import           Reflex
import           Reflex.Data.ActionStack
import           Reflex.Potato.Helpers
import           Reflex.Test.Host

import qualified Data.List                     as L
import           Data.These

import           Control.Concurrent

main :: IO ()
main = memtest2

-- does not leak
dyntest_network
  :: forall t m
   . (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (AppIn t () Int -> PerformEventT t m (AppOut t () Int))
dyntest_network AppIn {..} = do
  tick <- foldDyn (+) 0 _appIn_event
  let zeroDyn = constDyn 0
      foldfn _ _ = holdDyn 0 (updated tick)
  doubleDyn <- foldDynM
    foldfn
    zeroDyn
    (fmapMaybe (\x -> if x == 1 then Just 1 else Nothing) $ updated tick)
  return AppOut { _appOut_behavior = constant ()
                , _appOut_event    = updated $ join doubleDyn
                }


memtest2 :: IO ()
memtest2 = runSpiderHost $ do
  appFrame <- getAppFrame dyntest_network ()
  let loop n = do
        out <- tickAppFrame appFrame (Just (That (n `mod` 2)))
        liftIO $ do
          putStrLn $ "ticked: " <> show out
          threadDelay 10000
          hasStats <- getRTSStatsEnabled
          when (not hasStats) $ error "no stats"
          stats <- getRTSStats
          print (toImportant stats)
        loop (n + 1)
  loop 0




data TestCmd a = TCDo a | TCUndo | TCRedo | TCClear deriving (Eq, Show)

simple_state_network
  :: forall t a s m
   . (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (a -> s -> s) -- ^ do/redo method to transform state
  -> (a -> s -> s) -- ^ undo method to transform state
  -> s -- ^ initial state
  -> (AppIn t () (TestCmd a) -> PerformEventT t m (AppOut t () s))
simple_state_network fdo fundo initial AppIn {..} = do
  let ev   = _appIn_event
      doEv = flip fmapMaybe ev $ \case
        TCDo a -> Just a
        _      -> Nothing
      undoEv = flip fmapMaybe ev $ \case
        TCUndo -> Just ()
        _      -> Nothing
      redoEv = flip fmapMaybe ev $ \case
        TCRedo -> Just ()
        _      -> Nothing
      clearEv = flip fmapMaybe ev $ \case
        TCClear -> Just ()
        _       -> Nothing

      mas = ActionStackConfig { _actionStackConfig_do    = doEv
                              , _actionStackConfig_undo  = undoEv
                              , _actionStackConfig_redo  = redoEv
                              , _actionStackConfig_clear = clearEv
                              }
  as :: ActionStack t a <- holdActionStack mas
  adder :: Dynamic t s  <- foldDynMergeWith
    initial
    [fmap fdo (_actionStack_do as), fmap fundo (_actionStack_undo as)]
  return AppOut { _appOut_behavior = constant ()
                , _appOut_event    = updated adder
                }

-- does not leak
basic_network
  :: forall t m
   . (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (AppIn t Int Int -> PerformEventT t m (AppOut t Int Int))
basic_network AppIn {..} = return AppOut
  { _appOut_behavior = fmap (* (-1)) _appIn_behavior
  , _appOut_event    = fmap (\(b, e) -> e + b)
                         $ attach _appIn_behavior _appIn_event
  }

toImportant :: RTSStats -> ImportantStats
toImportant RTSStats {..} = ImportantStats
  { _gcs                       = gcs
  , _max_live_bytes            = max_live_bytes
  , _max_mem_in_use_bytes      = max_mem_in_use_bytes
  , _gcdetails_allocated_bytes = gcdetails_allocated_bytes gc
  , _gcdetails_live_bytes      = gcdetails_live_bytes gc
  }

data ImportantStats = ImportantStats {
  _gcs                         :: Word32
  , _max_live_bytes            :: Word64
  , _max_mem_in_use_bytes      :: Word64
  , _gcdetails_allocated_bytes :: Word64
  , _gcdetails_live_bytes      :: Word64
} deriving (Show)

memtest :: IO ()
memtest = runSpiderHost $ do
  --appFrame <- getAppFrame basic_network (1 :: Int)
  appFrame <- getAppFrame (simple_state_network (+) (flip (-)) (0 :: Int)) ()
  let loop n = do
        --out <- tickAppFrame appFrame (Just (That 1))
        out <- tickAppFrame appFrame $ case n `mod` 2 of
          0 -> Just (That (TCDo 1))
          1 -> Just (That (TCUndo))
        liftIO $ do
          putStrLn $ "ticked: " <> show out
          threadDelay 10000
          hasStats <- getRTSStatsEnabled
          when (not hasStats) $ error "no stats"
          stats <- getRTSStats
          print (toImportant stats)
        loop (n + 1)
  loop 0
