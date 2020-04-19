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

import           Potato.Flow

simpleSBox :: SBox
simpleSBox = SBox nilLBox defaultSLineStyle

data FCmd =
  FCNone
  | FCAddSElt SElt
  | FCRemoveRElt Int -- position in layers to remove at
  | FCManipulate ()
  | FCUndo
  | FCRedo
  deriving (Eq, Show)

basic_network :: forall t m a. BasicGuestConstraints t m => Event t FCmd -> BasicGuest t m (Event t Int)
basic_network ev = do
  let
    -- TODO maybe a good place to try out template haskell
    addEv = flip fmapMaybe ev $ \case
      FCAddSElt x -> Just (SEltLabel "blank" x)
      _ -> Nothing
    removeEv = flip fmapMaybe ev $ \case
      FCRemoveRElt x -> Just x
      _ -> Nothing
    manipEv = flip fmapMaybe ev $ \case
      FCManipulate x -> Just x
      _ -> Nothing
    redoEv = flip fmapMaybe ev $ \case
      FCRedo -> Just ()
      _ -> Nothing
    undoEv = flip fmapMaybe ev $ \case
      FCUndo -> Just ()
      _ -> Nothing

    pfc = PFConfig {
      _pfc_addElt       = addEv
      , _pfc_removeElt  = removeEv
      , _pfc_manipulate = never
      , _pfc_undo = undoEv
      , _pfc_redo = redoEv
    }
  pf <- holdPF pfc
  return $ updated . fmap length . _layerTree_view . _pfo_layers $ pf

basic_test :: Test
basic_test = TestLabel "basic" $ TestCase $ do
  let
    bs = [
        FCNone
        , FCAddSElt (SEltBox simpleSBox)
        , FCAddSElt (SEltBox simpleSBox)
        , FCAddSElt (SEltBox simpleSBox)
        , FCAddSElt (SEltBox simpleSBox)
        , FCAddSElt (SEltBox simpleSBox)
        , FCAddSElt (SEltBox simpleSBox)
        , FCRemoveRElt 0
        , FCRemoveRElt 3
        , FCRemoveRElt 0
        , FCAddSElt (SEltBox simpleSBox)
        , FCUndo , FCUndo , FCUndo , FCUndo , FCUndo , FCUndo
        , FCRedo , FCRedo , FCRedo , FCRedo , FCRedo , FCRedo
      ]
    run :: IO [[Maybe Int]]
    run = basicHostWithStaticEvents bs basic_network
  v <- liftIO run
  print v
  return ()
  --L.last (join v) @?= Just ()

spec :: Spec
spec = do
  describe "Potato Flow" $ do
    fromHUnitTest basic_test
