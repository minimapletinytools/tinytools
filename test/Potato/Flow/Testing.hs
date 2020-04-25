{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Testing (
  simpleSBox
  , FCmd(..)
  , randomActionFCmd
  , setup_network
  , step_state_network
) where

import           Relude               hiding (empty, fromList)

import           Reflex
import           Reflex.Test.Host

import           Data.Dependent.Sum   ((==>))
import qualified Data.IntMap.Strict   as IM
import qualified Data.List            as L ((!!))
import qualified Data.List.Index      as L
import           Data.Maybe           (fromJust)

import qualified Control.Monad.Random as R

import           Potato.Flow

simpleSBox :: SBox
simpleSBox = SBox (LBox (LPoint (V2 5 5)) (LSize (V2 5 5))) defaultSLineStyle

data FCmd =
  FCNone
  | FCAddElt Int SElt
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

randomActionFCmd :: Bool -> SEltTree -> IO FCmd
randomActionFCmd doundo stree = do
  let
    nElts = length stree
    eltsOnly = filter (isElement . snd) $  L.indexed stree
    nCmds = if doundo then 5 else 3
  rcmd :: Int <- R.getRandomR (0, nCmds-1)
  if null eltsOnly || rcmd == 0
    then do
      pos <- R.getRandomR (0, nElts)
      return $ FCAddElt pos $ SEltBox simpleSBox
    else do
      rindex <- R.getRandomR (0, length eltsOnly - 1)
      let (pos, (SEltLabel _ selt)) = eltsOnly L.!! rindex
      case rcmd of
        1 -> return $ FCDeleteElt pos
        2 -> case selt of
          SEltBox _ -> return $ FCCustom_CBox_1 pos
          _         -> undefined
          --_         -> return FCNone
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
