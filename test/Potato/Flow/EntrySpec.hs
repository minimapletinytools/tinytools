{-# LANGUAGE RecursiveDo #-}

module Potato.Flow.EntrySpec
  ( spec
  )
where

import           Relude                   hiding (empty, fromList)

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit (fromHUnitTest)
import           Test.HUnit

import           Reflex
import           Reflex.Potato.Helpers
import           Reflex.Test.Host

import qualified Data.List                as L (last)

import           Potato.Flow

simpleSBox :: SBox
simpleSBox = SBox nilLBox defaultSLineStyle

data FCmd =
  FCNone
  | FCAddElt SElt
  | FCDeleteElt Int -- position in layers to remove at
  | FCManipulate ()
  | FCUndo
  | FCRedo
  deriving (Eq, Show)

{-
TODO
randomFCmd :: SEltTree -> IO FCmd
randomFCmd stree = do
  let n = length stree
-}



basic_network
  :: forall t m
   . (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (Event t FCmd -> PerformEventT t m (Event t Int))
basic_network ev = do
  let
    -- TODO maybe a good place to try out template haskell
      addEv = flip fmapMaybe ev $ \case
        FCAddElt x -> Just (0, SEltLabel "blank" x)
        _           -> Nothing
      removeEv = flip fmapMaybe ev $ \case
        FCDeleteElt x -> Just x
        _              -> Nothing
      manipEv = flip fmapMaybe ev $ \case
        FCManipulate x -> Just x
        _              -> Nothing
      redoEv = flip fmapMaybe ev $ \case
        FCRedo -> Just ()
        _      -> Nothing
      undoEv = flip fmapMaybe ev $ \case
        FCUndo -> Just ()
        _      -> Nothing

      pfc = PFConfig { _pfc_addElt     = addEv
                     , _pfc_removeElt  = removeEv
                     , _pfc_manipulate = never
                     , _pfc_undo       = undoEv
                     , _pfc_redo       = redoEv
                     }
  pf <- holdPF pfc
  return $ updated . fmap length . _sEltLayerTree_view . _pfo_layers $ pf

basic_test :: Test
basic_test = TestLabel "basic" $ TestCase $ do
  let bs =
        [ FCNone
        , FCAddElt (SEltBox simpleSBox)
        , FCAddElt (SEltBox simpleSBox)
        , FCAddElt (SEltBox simpleSBox)
        , FCAddElt (SEltBox simpleSBox)
        , FCAddElt (SEltBox simpleSBox)
        , FCAddElt (SEltBox simpleSBox)
        , FCDeleteElt 0
        , FCDeleteElt 3
        , FCDeleteElt 0
        , FCAddElt (SEltBox simpleSBox)
        , FCUndo
        , FCUndo
        , FCUndo
        , FCUndo
        , FCUndo
        , FCUndo
        , FCRedo
        , FCRedo
        , FCRedo
        , FCRedo
        , FCRedo
        , FCRedo
        ]
      run :: IO [[Maybe Int]]
      run = runAppSimple basic_network bs
  v <- liftIO run
  print v
  L.last (join v) @?= Just 4

spec :: Spec
spec = do
  describe "Potato Flow" $ do
    fromHUnitTest basic_test
