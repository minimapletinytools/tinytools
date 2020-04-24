{-# LANGUAGE RecursiveDo #-}

module Potato.Flow.NewEntrySpec
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

import           Data.Dependent.Sum       (DSum ((:=>)), (==>))
import qualified Data.IntMap.Strict       as IM
import qualified Data.List                as L (last)
import           Data.Maybe               (fromJust)
import qualified Data.Text                as T
import           Text.Pretty.Simple       (pPrint)

import           Potato.Flow

simpleSBox :: SBox
simpleSBox = SBox (LBox (LPoint (V2 5 5)) (LSize (V2 5 5))) defaultSLineStyle

data FCmd =
  FCNone
  | FCAddElt SElt
  | FCDeleteElt Int -- position in layers to remove at, must be valid
  | FCUndo
  | FCRedo
  | FCSave

  | FCCustom_Add_SBox_1
  | FCCustom_CBox_1 LayerPos
  deriving (Eq, Show)

setup_network
  :: forall t m
   . (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => Event t FCmd -> PerformEventT t m (PFOutput t)
setup_network ev = mdo
  let
    addEv = flip fmapMaybe ev $ \case
      FCAddElt x -> Just (0, SEltLabel "blank" x)
      FCCustom_Add_SBox_1 -> Just (0, SEltLabel "customsbox" (SEltBox simpleSBox))
      _           -> Nothing

    removeEv = flip fmapMaybe ev $ \case
      FCDeleteElt x -> Just x
      _              -> Nothing
    manipEv = flip push ev $ \case
      FCCustom_CBox_1 lp -> do
        (_,rid,SEltLabel _ selt) <- fromJust <$> sEltLayerTree_sampleSuperSEltByPos layerTree lp
        let
          cbox = CBox {
              _cBox_box    = DeltaLBox (LPoint (V2 1 1)) (LSize (V2 5 5))
            }
        return . Just $ IM.singleton rid (CTagBox ==> cbox)
      _              -> return Nothing
    redoEv = flip fmapMaybe ev $ \case
      FCRedo -> Just ()
      _      -> Nothing
    undoEv = flip fmapMaybe ev $ \case
      FCUndo -> Just ()
      _      -> Nothing
    saveEv = flip fmapMaybe ev $ \case
      FCSave -> Just ()
      _      -> Nothing

    pfc = PFConfig { _pfc_addElt     = addEv
                   , _pfc_removeElt  = removeEv
                   , _pfc_manipulate = manipEv
                   , _pfc_undo       = undoEv
                   , _pfc_redo       = redoEv
                   , _pfc_save = saveEv
                   }
  pfo <- holdPF pfc
  let
    layerTree = _pfo_layers $ pfo
  return pfo

save_network
  :: forall t m. (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (Event t FCmd -> PerformEventT t m (Event t [SEltLabel]))
save_network ev = do
  pfo <- setup_network ev
  return $ _pfo_saved pfo


bs_save_0 :: ([FCmd],[FCmd])
bs_save_0 =
  ([FCAddElt (SEltBox simpleSBox), FCUndo, FCRedo, FCSave]
  , [FCAddElt (SEltBox simpleSBox), FCSave])

bs_save_1 :: ([FCmd],[FCmd])
bs_save_1 =
  ([FCCustom_Add_SBox_1, FCCustom_CBox_1 0, FCUndo, FCRedo, FCCustom_Add_SBox_1, FCSave]
  , [FCCustom_Add_SBox_1, FCCustom_Add_SBox_1, FCCustom_CBox_1 0, FCSave])

bs_save_2 :: ([FCmd],[FCmd])
bs_save_2 =
  ([FCCustom_Add_SBox_1, FCCustom_Add_SBox_1, FCDeleteElt 1, FCUndo, FCUndo, FCUndo, FCUndo, FCSave]
  , [FCSave])

bs_save_3 :: ([FCmd],[FCmd])
bs_save_3 =
  ([FCCustom_Add_SBox_1, FCDeleteElt 0, FCUndo, FCRedo, FCSave]
  , [FCSave])

-- TODO maybe drop the `t ~ SpiderTimeline Global` constraint
-- you'll need to modify reflex-test-host for this
pair_test :: forall t m a. (t ~ SpiderTimeline Global, m ~ SpiderHost Global, Eq a, Show a)
  => Text
  -> (Event t FCmd -> PerformEventT t m (Event t a))
  -> ([FCmd],[FCmd])
  -> Test
pair_test name network (bs1, bs2) = TestLabel ("pairs: " ++ T.unpack name) $ TestCase $ do
  let
    run1 = runAppSimple network bs1
    run2 = runAppSimple network bs2
  v1 <- liftIO run1
  v2 <- liftIO run2
  L.last (join v1) @?= L.last (join v2)

spec :: Spec
spec = do
  describe "Potato Flow" $ do
    fromHUnitTest $ pair_test "save0" save_network bs_save_0
    fromHUnitTest $ pair_test "save1" save_network bs_save_1
    fromHUnitTest $ pair_test "save2" save_network bs_save_2
    fromHUnitTest $ pair_test "save3" save_network bs_save_3
