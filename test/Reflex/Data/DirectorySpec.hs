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


basic_network :: forall t m. BasicGuestConstraints t m => Event t Int -> BasicGuest t m (Event t (Int,Int))
basic_network ev = do
  let
    diac = DirectoryIdAssignerConfig {
        _directoryIdAssignerConfig_assign = fmap (:| []) ev
      }
  dia <- holdDirectoryIdAssigner diac

  -- TODO
  --attachPromptlyDyn (_directoryIdAssigner_assigned dia) ev
  let
    -- TODO test more stuff
    dmc = DirectoryConfig {
        _directoryMapConfig_add = updated (_directoryIdAssigner_assigned dia)
        , _directoryMapConfig_remove = never
      }
  dm <- holdDirectory dmc

  return $ fmap head $ _directoryMap_added dm

test_basic_network :: Test
test_basic_network = TestLabel "basic" $ TestCase $ do
  let
    bs = [0..10] :: [Int]
    run :: IO [[Maybe (Int,Int)]]
    run = basicHostWithStaticEvents bs basic_network
  v <- liftIO run
  --print v
  return ()
  L.last (join v) @?= Just (10,10)

spec :: Spec
spec = do
  describe "ActionStack" $ do
    fromHUnitTest test_basic_network
