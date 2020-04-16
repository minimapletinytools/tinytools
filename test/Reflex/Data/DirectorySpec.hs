{-# LANGUAGE RecursiveDo #-}

module Reflex.Data.DirectorySpec (
  spec
) where

import           Relude

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit  (fromHUnitTest)
import           Test.HUnit

import qualified Data.List                 as L (last)

import           Reflex
import           Reflex.Data.Directory
import           Reflex.Potato.Helpers
import           Reflex.Potato.TestHarness

import           Reflex.Host.Basic

data TestCmd a = TCDo a | TCUndo | TCRedo | TCClear deriving (Eq, Show)


basic_network :: forall t m. BasicGuestConstraints t m => Event t [Int] -> BasicGuest t m (Event t (Int,Int))
basic_network ev = do
  let
    neEv = fmap fromList ev
    diac = DirectoryIdAssignerConfig {
        _directoryIdAssignerConfig_assign = neEv
      }
  dia <- holdDirectoryIdAssigner diac

  let
    idAssignedEv = _directoryIdAssignerConfig_attach dia neEv
    -- TODO test more stuff
    dmc = DirectoryConfig {
        _directoryMapConfig_add = idAssignedEv
        , _directoryMapConfig_remove = never
      }
  dm <- holdDirectory dmc

  return $ fmap last $ _directoryMap_added dm

test_basic_network :: Test
test_basic_network = TestLabel "basic" $ TestCase $ do
  let
    bs = [[0..100],[0],[123,3,4],[1],[3],[1..10]] :: [[Int]]
    run :: IO [[Maybe (Int,Int)]]
    run = basicHostWithStaticEvents bs basic_network
  v <- liftIO run
  --print v
  return ()
  L.last (join v) @?= Just (length (join bs) - 1,10)

spec :: Spec
spec = do
  describe "ActionStack" $ do
    fromHUnitTest test_basic_network
