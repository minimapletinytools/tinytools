{-# LANGUAGE RecursiveDo #-}

module Reflex.Data.DirectorySpec
  ( spec
  )
where

import           Relude

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit (fromHUnitTest)
import           Test.HUnit

import qualified Data.List                as L (last)

import           Reflex
import           Reflex.Data.Directory
import           Reflex.Test.Host

{- TODO add better test cases
import qualified Text.Show

data TestCmd a = TCAdd a | TCRemove DirId | TCModify (DirId, a -> a)

instance (Show a) => Show (TestCmd a) where
  show (TCAdd (i,a))    = "TCAdd " <> show i <> " " <> show a
  show (TCRemove i)     = "TCRemove " <> show i
  show (TCModify (i,_)) = "TCModify " <> show i

  let addEv = flip fmapMaybe ev $ \case
        TCAdd x -> Just x
        _      -> Nothing
      removeEv = flip fmapMaybe ev $ \case
        TCRemove x -> Just x
        _      -> Nothing
      modifyEv = flip fmapMaybe ev $ \case
        TCModify x -> Just x
        _      -> Nothing
-}

basic_network
  :: forall t m
   . (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (Event t [Int] -> PerformEventT t m (Event t (Int, Int)))
basic_network ev = do
  let neEv = fmap fromList ev
      diac =
        DirectoryIdAssignerConfig { _directoryIdAssignerConfig_assign = neEv }
  dia <- holdDirectoryIdAssigner diac

  let idAssignedEv = _directoryIdAssigner_tag dia neEv
      -- TODO test more stuff
      dmc          = DirectoryConfig { _directoryConfig_add    = idAssignedEv
                                     , _directoryConfig_remove = never
                                     , _directoryConfig_modifyWith = never
                                     , _directoryConfig_set = never
                                     }
  dm <- holdDirectory dmc

  return $ fmap last $ _directory_added dm

test_basic_network :: Test
test_basic_network = TestLabel "basic" $ TestCase $ do
  let bs = [[0 .. 100], [0], [123, 3, 4], [1], [3], [1 .. 10]] :: [[Int]]
      run :: IO [[Maybe (Int, Int)]]
      run = runAppSimple basic_network bs
  v <- liftIO run
  --print v
  return ()
  L.last (join v) @?= Just (length (join bs) - 1, 10)

spec :: Spec
spec = do
  describe "Directory" $ do
    fromHUnitTest test_basic_network
