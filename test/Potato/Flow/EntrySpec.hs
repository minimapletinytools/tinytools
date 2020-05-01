-- DELETE

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
import           Reflex.Data.Directory
import           Reflex.Potato.Helpers
import           Reflex.Test.Host

import           Data.Default             (def)
import           Data.Dependent.Sum       (DSum ((:=>)), (==>))
import qualified Data.IntMap.Strict       as IM
import qualified Data.List                as L (last)
import           Data.Maybe               (fromJust)

import           Potato.Flow

simpleSBox :: SBox
simpleSBox = SBox nilLBox def

data FCmd =
  FCNone
  | FCAddElt SElt
  | FCDeleteElt Int -- position in layers to remove at
  | FCUndo
  | FCRedo
  | FCScaleBy10 LayerPos
  deriving (Eq, Show)

basic_network
  :: forall t m
   . (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (Event t FCmd -> PerformEventT t m (Event t Int))
basic_network ev = mdo
  let
    addEv = flip fmapMaybe ev $ \case
      FCAddElt x -> Just (0, SEltLabel "blank" x)
      _           -> Nothing
    removeEv = flip fmapMaybe ev $ \case
      FCDeleteElt x -> Just x
      _              -> Nothing
    manipEv = flip push ev $ \case
      FCScaleBy10 lp -> do
        (_,rid,SEltLabel _ selt) <- fromJust <$> sEltLayerTree_sampleSuperSEltByPos layerTree lp
        let
          cbox = CBox {
              _cBox_deltaBox    = DeltaLBox (LPoint (V2 1 1)) (LSize (V2 5 5))
            }
        return . Just $ IM.singleton rid (CTagBox ==> cbox)
      _              -> return Nothing
    redoEv = flip fmapMaybe ev $ \case
      FCRedo -> Just ()
      _      -> Nothing
    undoEv = flip fmapMaybe ev $ \case
      FCUndo -> Just ()
      _      -> Nothing

    pfc = PFConfig { _pfc_addElt     = addEv
                   , _pfc_removeElt  = removeEv
                   , _pfc_manipulate = manipEv
                   , _pfc_resizeCanvas = never
                   , _pfc_undo       = undoEv
                   , _pfc_redo       = redoEv
                   , _pfc_load = never
                   , _pfc_save = never
                   }
  pf <- holdPF pfc
  let
    layerTree = _pfo_layers $ pf
    newEltsEv = updated . fmap length . _sEltLayerTree_view $ layerTree
    changes :: Event t (REltIdMap (Maybe SEltLabel, Maybe SEltLabel))
    changes = _sEltLayerTree_changeView layerTree
    directory :: Directory t SEltLabel
    directory = _sEltLayerTree_directory layerTree

  return newEltsEv

basic_test :: Test
basic_test = TestLabel "basic" $ TestCase $ do
  let bs =
        [ FCNone
        , FCAddElt (SEltBox simpleSBox)
        , FCScaleBy10 0
        , FCUndo
        , FCRedo
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
  L.last (join v) @?= Just 4

spec :: Spec
spec = do
  describe "Potato Flow" $ do
    fromHUnitTest basic_test
