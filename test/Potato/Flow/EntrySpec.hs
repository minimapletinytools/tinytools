{-# LANGUAGE RecursiveDo #-}

module Potato.Flow.EntrySpec (
  spec
) where

import           Relude                   hiding (empty, fromList)

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit (fromHUnitTest)
import           Test.HUnit

import           Reflex
import           Reflex.Potato.Helpers

import           Reflex.Host.Basic

import qualified Data.List                as L (last)
import           Data.Sequence

import           Potato.Flow

data FCmd = FCNone deriving (Eq, Show)

basic_network :: forall t m a. BasicGuestConstraints t m => Event t FCmd -> BasicGuest t m (Event t ())
basic_network ev = do
  let
    noEv = flip fmapMaybe ev $ \case
      FCNone -> Just ()
      _ -> Nothing

{-
    pfc = data PFConfig t = PFConfig {
      _pfc_addElt       :: Event t SEltLabel
      , _pfc_removeElt  :: Event t REltId
      , _pfc_manipulate :: Event t ()
    }
  pf <- holdPF pfc
-}
  return noEv

basic_test :: Test
basic_test = TestLabel "basic" $ TestCase $ do
  let
    bs = [FCNone]
    run :: IO [[Maybe ()]]
    run = basicHostWithStaticEvents bs basic_network
  v <- liftIO run
  L.last (join v) @?= Just ()

spec :: Spec
spec = do
  describe "Potato Flow" $ do
    fromHUnitTest basic_test
