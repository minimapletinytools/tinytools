{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Testing (
  simpleSBox
  , FCmd(..)
  , randomActionFCmd
  , setup_network
  , step_state_network
) where

import           Relude                 hiding (empty, fromList)

import           Reflex
import           Reflex.Test.Host

import           Data.Constraint.Extras (Has')
import           Data.Default           (def)
import           Data.Dependent.Sum     ((==>))
import qualified Data.IntMap.Strict     as IM
import qualified Data.List              as L (take, (!!))
import qualified Data.List.Index        as L
import           Data.Maybe             (fromJust)
import           Data.Tuple.Extra

import qualified Control.Monad.Random   as R
import           System.Random.Shuffle

import           Potato.Flow

simpleSBox :: SBox
simpleSBox = SBox (LBox (LPoint (V2 5 5)) (LSize (V2 5 5))) def

data FCmd =
  FCNone
  | FCAddElt LayerPos SElt
  | FCDeleteElt LayerPos
  | FCModify LayerPos Controller
  | FCModifyMany [(LayerPos, Controller)]
  | FCResizeCanvas DeltaLBox
  | FCUndo
  | FCRedo
  | FCSave
  | FCLoad SPotatoFlow

  | FCCustom_Add_SBox_1
  | FCCustom_CBox_1 LayerPos
  deriving (Eq, Show)

setup_network:: forall t m. (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => Event t FCmd -> PerformEventT t m (PFOutput t)
setup_network ev = mdo
  let
    addEv = fforMaybe ev $ \case
      FCAddElt p x -> Just (p, SEltLabel "blank" x)
      FCCustom_Add_SBox_1 -> Just (0, SEltLabel "customsbox" (SEltBox simpleSBox))
      _           -> Nothing

    removeEv = fforMaybe ev $ \case
      FCDeleteElt p -> Just p
      _              -> Nothing
    manipEv = flip push ev $ \case
      FCModify p c -> do
        (rid, _, SEltLabel _ selt) <- fromJust <$> sEltLayerTree_sampleSuperSEltByPos layerTree p
        -- `deepseq` here prevents a leak, fml.
        return . Just $ selt `deepseq` IM.singleton rid c
      FCModifyMany pcs -> do
        sseltls <- forM pcs (\(p,c) -> sEltLayerTree_sampleSuperSEltByPos layerTree p >>= \seltl -> seltl `deepseq` return (seltl,c))
        return . Just . IM.fromList
          . map (\((rid,_,_),c) -> (rid, c))
          . map (\(mseltl, c) -> (fromJust mseltl, c))
          $ sseltls
      FCCustom_CBox_1 p -> do
        (rid, _, SEltLabel _ selt) <- fromJust <$> sEltLayerTree_sampleSuperSEltByPos layerTree p
        let
          cbox = CBox {
              _cBox_deltaBox    = DeltaLBox (LPoint (V2 1 1)) (LSize (V2 5 5))
            }
        return . Just $ selt `deepseq` IM.singleton rid (CTagBox ==> cbox)
      _              -> return Nothing
    resizeCanvasEv = fforMaybe ev $ \case
      FCResizeCanvas x -> Just x
      _ -> Nothing
    redoEv = fforMaybe ev $ \case
      FCRedo -> Just ()
      _      -> Nothing
    undoEv = fforMaybe ev $ \case
      FCUndo -> Just ()
      _      -> Nothing
    loadEv = fforMaybe ev $ \case
      FCLoad x -> Just x
      _      -> Nothing
    saveEv = fforMaybe ev $ \case
      FCSave -> Just ()
      _      -> Nothing

    pfc = PFConfig { _pfc_addElt     = addEv
                   , _pfc_removeElt  = removeEv
                   , _pfc_manipulate = manipEv
                   , _pfc_resizeCanvas = resizeCanvasEv
                   , _pfc_undo       = undoEv
                   , _pfc_redo       = redoEv
                   , _pfc_load = loadEv
                   , _pfc_save = saveEv
                   }
  pfo <- holdPF pfc
  let
    layerTree = _pfo_layers $ pfo
  return pfo


-- | make sure to tick with 'FCNone' to ensure output behavior is most recent
step_state_network :: forall t m.
  (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (AppIn t () FCmd -> PerformEventT t m (AppOut t SEltTree SPotatoFlow))
step_state_network AppIn {..} = do
  pfo <- setup_network _appIn_event
  return
    AppOut {
      _appOut_behavior = fmap (fmap thd3) $ _pfo_potato_state pfo
      --, _appOut_event =
      --, _appOut_event  = never
      , _appOut_event = leftmost
        [_pfo_saved pfo
        -- this is needed to force the changeView event and prevent leaks
        , fmapMaybe (const Nothing) $ _sEltLayerTree_changeView (_pfo_layers pfo)]
    }


isElement :: SEltLabel -> Bool
isElement (SEltLabel _ selt) = case selt of
  SEltNone        -> False
  SEltFolderStart -> False
  SEltFolderEnd   -> False
  _               -> True

randomXY :: (R.MonadRandom m) => m XY
randomXY = do
  x <- R.getRandomR (-99999, 99999)
  y <- R.getRandomR (-99999, 99999)
  return $ V2 x y

randomActionFCmd ::
 (R.MonadRandom m, Has' Show CTag Identity)
 => Bool -> SEltTree -> m FCmd
randomActionFCmd doundo stree = do
  let
    nElts = length stree
    eltsOnly = filter (isElement . snd) $  L.indexed stree
    startCmd = if doundo then 0 else 2
  rcmd <- if null eltsOnly
    then return (2 :: Int)
    else R.getRandomR (startCmd, 5)
  case rcmd of
    0 -> return FCUndo
    1 -> return FCRedo
    2 -> do
      pos <- R.getRandomR (0, nElts)
      stype <- R.getRandomR (0, 2 :: Int)
      p1 <- randomXY
      p2 <- randomXY
      case stype of
        0 -> return $ FCAddElt pos $ SEltBox
          SBox {
            _sBox_box = LBox (LPoint p1) (LSize p2)
            , _sBox_style = def
          }
        1 -> return $ FCAddElt pos $ SEltLine
          SLine {
            _sLine_start = LPoint p1
            , _sLine_end = LPoint p2
            , _sLine_style = def
          }
        2 -> return $ FCAddElt pos $ SEltText
          SText {
            _sText_box = LBox (LPoint p1) (LSize p2)
            , _sText_text = "moo"
            , _sText_style = def
          }
        _ -> undefined
    3 -> do
      p1 <- randomXY
      p2 <- randomXY
      return $ FCResizeCanvas $ DeltaLBox (LPoint p1) (LSize p2)
    _ -> do
      -- just one random elements
      rindex <- R.getRandomR (0, length eltsOnly - 1)
      let (pos, (SEltLabel _ selt)) = eltsOnly L.!! rindex

      -- many random elements
      shuffled <- shuffleM eltsOnly
      -- TODO for delete you don't want to delete too many otherwise you'll always end up with like no elements
      -- i.e. prob want weighted random
      nElts <- R.getRandomR (1, length eltsOnly)
      let randomElts = L.take nElts shuffled

      case rcmd of
        4 -> return $ FCDeleteElt pos
        5 -> fmap FCModifyMany . forM randomElts $ \(pos, (SEltLabel name selt)) -> do
          rename <- (==0) <$> R.getRandomR (0, 10 :: Int)
          if rename then do
            newName <- show <$> R.getRandomR (0, 1000000 :: Int)
            return $ (,) pos $ CTagRename ==>
              CRename {
                _cRename_deltaLabel = (name, newName)
              }
          else do
            p1 <- randomXY
            p2 <- randomXY
            case selt of
              SEltBox _ -> return $ (,) pos $ CTagBox ==>
                CBox {
                  _cBox_deltaBox = DeltaLBox (LPoint p1) (LSize p2)
                }
              SEltLine _ -> return $ (,) pos $ CTagLine ==>
                CLine {
                  _cLine_deltaStart = LPoint p1
                  , _cLine_deltaEnd = LPoint p2
                }
              SEltText (SText _ before _) -> return $ (,) pos $ CTagText ==>
                CText {
                  _cText_deltaBox = DeltaLBox (LPoint p1) (LSize p2)
                  , _cText_deltaText = (before, "meow meow")
                }
              _ -> error "this should never happen"
        _ -> error "this should never happen"
