{-# LANGUAGE RecordWildCards #-}

import           Relude

import           GHC.Stats

import           Reflex
import           Reflex.Data.ActionStack
import           Reflex.Potato.Helpers
import           Reflex.Test.Host

import qualified Data.List               as L
import           Data.These

import           Control.Concurrent

import           Potato.Flow
import           Potato.Flow.Testing

main :: IO ()
main = runSpiderHost $ do
  liftIO $ do
    hasStats <- getRTSStatsEnabled
    when (not hasStats) $ error "no stats"
  appFrame <- getAppFrame step_state_network ()
  let
    m0 = 100 -- num commands to do to set up state
    l0 = 100 -- num commands to do and the undo
    setupLoop 0 st = return st
    setupLoop n st = do
      action <- liftIO $ randomActionFCmd False st
      _ <- tickAppFrame appFrame $ Just $ That action
      out <- tickAppFrame appFrame $ Just $ That FCSave
      case L.last out of
        (_, mspf) -> case mspf of
          Nothing  -> error "expected state"
          Just spf -> setupLoop (n-1) (_sPotatoFlow_sEltTree spf)
    undoredoLoop _ (0 :: Int) st = return st
    undoredoLoop isUndo n st = do
      _ <- tickAppFrame appFrame $ Just $ That (if isUndo then FCUndo else FCRedo)
      out <- tickAppFrame appFrame $ Just $ That FCSave
      case L.last out of
        (_, mspf) -> case mspf of
          Nothing  -> error "expected state"
          Just spf -> undoredoLoop isUndo (n-1) (_sPotatoFlow_sEltTree spf)
    loopForever st n = do
      st1 <- setupLoop l0 st
      st2 <- undoredoLoop True l0 st1
      liftIO $ do
        threadDelay 1000
        --print (length st2)
        stats <- getRTSStats
        printStats n stats
      loopForever st2 (n+1)
  -- first create some non empty initial state
  st0 <- setupLoop m0 []
  loopForever st0 0


printStats :: Int -> RTSStats -> IO ()
printStats n RTSStats {..} = do
  putStrLn $ show n <> ": " <> show max_live_bytes <> " " <> show (gcdetails_live_bytes gc)
