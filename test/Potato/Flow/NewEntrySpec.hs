{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

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
import qualified Data.List                as L (last, (!!))
import qualified Data.List.Index          as L
import           Data.Maybe               (fromJust)
import qualified Data.Text                as T
import           Data.These
import           Text.Pretty.Simple       (pPrint)

import qualified Control.Monad.Random     as R

import           Potato.Flow

simpleSBox :: SBox
simpleSBox = SBox (LBox (LPoint (V2 5 5)) (LSize (V2 5 5))) defaultSLineStyle

data FCmd =
  FCNone
  -- TODO add position param
  | FCAddElt SElt
  -- TODO change to take a Selection
  | FCDeleteElt Int
  | FCUndo
  | FCRedo
  | FCSave

  | FCCustom_Add_SBox_1
  | FCCustom_CBox_1 LayerPos
  deriving (Eq, Show)

isElement :: SEltLabel -> Bool
isElement (SEltLabel _ selt) = case selt of
  SEltNone        -> False
  SEltFolderStart -> False
  SEltFolderEnd   -> False
  _               -> True

randomActionFCmd :: SEltTree -> IO FCmd
randomActionFCmd stree = do
  let
    nElts = length stree
    eltsOnly = filter (isElement . snd) $  L.indexed stree
    nCmds = 4
  rcmd :: Int <- R.getRandomR (0, nCmds-1)
  if null eltsOnly || rcmd == 0
    then do
      --pos <- R.getRandomR (0, nElts)
      return $ FCAddElt $ SEltBox simpleSBox
    else do
      rindex <- R.getRandomR (0, length eltsOnly - 1)
      let (pos, (SEltLabel _ selt)) = eltsOnly L.!! rindex
      case rcmd of
        1 -> return $ FCDeleteElt pos
        2 -> return FCUndo
        3 -> return FCRedo
        4 -> case selt of
          SEltBox _ -> return $ FCCustom_CBox_1 pos
          _         -> return FCNone


setup_network:: forall t m. (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
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
  => (Event t FCmd -> PerformEventT t m (Event t SEltTree))
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


step_forever_test :: forall t a s m.
  (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => Int -> Test
step_forever_test n0 = TestCase $ runSpiderHost $ do
  let
    app_network AppIn {..} = do
      pfo <- setup_network _appIn_event
      return
        AppOut {
          _appOut_behavior = _pfo_state pfo
          , _appOut_event  = never
        }
  appFrame <- getAppFrame app_network ()
  let
    loop 0 _ = return ()
    loop n st = do
      action <- liftIO $ randomActionFCmd st
      --action <- return FCCustom_Add_SBox_1
      --liftIO $ print action
      _ <- tickAppFrame appFrame $ Just $ That action
      out <- tickAppFrame appFrame $ Just $ That FCNone
      --liftIO $ do
        --putStrLn $ "ticked: " <> show (fst out)
        --threadDelay 10000
        --hasStats <- getRTSStatsEnabled
        --when (not hasStats) $ error "no stats"
        --stats <- getRTSStats
        --print (toImportant stats)
      case L.last out of
        (nst, _) -> do
          --liftIO $ print "ticked"
          --liftIO $ print nst
          loop (n-1) nst
  loop n0 []

{-
  simple_state_network
    :: forall t a s m
     . (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
    => (a -> s -> s) -- ^ do/redo method to transform state
    -> (a -> s -> s) -- ^ undo method to transform state
    -> s -- ^ initial state
    -> (AppIn t () (TestCmd a) -> PerformEventT t m (AppOut t () s))
  simple_state_network fdo fundo initial AppIn {..} = do

    return AppOut { _appOut_behavior = constant ()
                  , _appOut_event    = updated adder
                  }

toApp :: forall t m eIn eOut. (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (Event t eIn -> PerformEventT t m eOut)
-}



spec :: Spec
spec = do
  describe "Potato Flow" $ do
    fromHUnitTest $ pair_test "save0" save_network bs_save_0
    fromHUnitTest $ pair_test "save1" save_network bs_save_1
    fromHUnitTest $ pair_test "save2" save_network bs_save_2
    fromHUnitTest $ pair_test "save3" save_network bs_save_3
    fromHUnitTest $ step_forever_test 10000
