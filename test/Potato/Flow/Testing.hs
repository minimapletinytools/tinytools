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
import           Data.Dependent.Sum     ((==>))
import qualified Data.IntMap.Strict     as IM
import qualified Data.List              as L ((!!))
import qualified Data.List.Index        as L
import           Data.Maybe             (fromJust)

import qualified Control.Monad.Random   as R

import           Potato.Flow

simpleSBox :: SBox
simpleSBox = SBox (LBox (LPoint (V2 5 5)) (LSize (V2 5 5))) defaultSLineStyle

data FCmd =
  FCNone
  | FCAddElt Int SElt
  | FCDeleteElt Int
  | FCModify Int Controller
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

randomXY :: IO XY
randomXY = do
  x <- R.getRandomR (-99999, 99999)
  y <- R.getRandomR (-99999, 99999)
  return $ V2 x y

randomActionFCmd ::
 (Has' Show CTag Identity)
 => Bool -> SEltTree -> IO FCmd
randomActionFCmd doundo stree = do
  let
    nElts = length stree
    eltsOnly = filter (isElement . snd) $  L.indexed stree
    nCmds = if doundo then 5 else 3
  rcmd :: Int <- R.getRandomR (0, nCmds-1)
  if null eltsOnly || rcmd == 0
    then do
      pos <- R.getRandomR (0, nElts)
      stype <- R.getRandomR (0, 2 :: Int)
      p1 <- randomXY
      p2 <- randomXY
      case stype of
        0 -> return $ FCAddElt pos $ SEltBox
          SBox {
            _sBox_box = LBox (LPoint p1) (LSize p2)
            , _sBox_style = defaultSLineStyle
          }
        1 -> return $ FCAddElt pos $ SEltLine
          SLine {
            _sLine_start = LPoint p1
            , _sLine_end = LPoint p2
            , _sLine_style = defaultSLineStyle
          }
        2 -> return $ FCAddElt pos $ SEltText
          SText {
            _sText_box = LBox (LPoint p1) (LSize p2)
            , _sText_text = "moo"
            , _sText_style = defaultSTextStyle
          }
        _ -> undefined
    else do
      rindex <- R.getRandomR (0, length eltsOnly - 1)
      p1 <- randomXY
      p2 <- randomXY
      let (pos, (SEltLabel _ selt)) = eltsOnly L.!! rindex
      case rcmd of
        1 -> return $ FCDeleteElt pos
        2 -> case selt of
          SEltBox _ -> return $ FCModify pos $ CTagBox ==>
            CBox {
              _cBox_box = DeltaLBox (LPoint p1) (LSize p2)
            }
          SEltLine _ -> return $ FCModify pos $ CTagLine ==>
            CLine {
              _cLine_start = LPoint p1
              , _cLine_end = LPoint p2
            }
          SEltText (SText _ before _) -> return $ FCModify pos $ CTagText ==>
            CText {
              _cText_box = DeltaLBox (LPoint p1) (LSize p2)
              , _cText_text = (before, "meow meow")
            }
          _ -> undefined
        3 -> return FCUndo
        4 -> return FCRedo
        _ -> undefined


setup_network:: forall t m. (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => Event t FCmd -> PerformEventT t m (PFOutput t)
setup_network ev = mdo
  let
    addEv = flip fmapMaybe ev $ \case
      FCAddElt p x -> Just (p, SEltLabel "blank" x)
      FCCustom_Add_SBox_1 -> Just (0, SEltLabel "customsbox" (SEltBox simpleSBox))
      _           -> Nothing

    removeEv = flip fmapMaybe ev $ \case
      FCDeleteElt p -> Just p
      _              -> Nothing
    manipEv = flip push ev $ \case
      FCModify p c -> do
        (rid, _, SEltLabel _ selt) <- fromJust <$> sEltLayerTree_sampleSuperSEltByPos layerTree p
        -- `deepseq` here prevents a leak, fml.
        return . Just $ selt `deepseq` IM.singleton rid c
      FCCustom_CBox_1 p -> do
        (rid, _, SEltLabel _ selt) <- fromJust <$> sEltLayerTree_sampleSuperSEltByPos layerTree p
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


-- | make sure to tick with 'FCNone' to ensure output behavior is most recent
step_state_network :: forall t m.
  (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (AppIn t () FCmd -> PerformEventT t m (AppOut t SEltTree ()))
step_state_network AppIn {..} = do
  pfo <- setup_network _appIn_event
  return
    AppOut {
      _appOut_behavior = _pfo_state pfo
      --, _appOut_event  = never
      , _appOut_event = fmap (\x -> x `deepseq` ()) $ _sEltLayerTree_changeView (_pfo_layers pfo)
    }
