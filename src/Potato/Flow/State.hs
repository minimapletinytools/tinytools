{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.State (
  PFState(..)
  , debugPrintPFState
  , pFState_isValid
  , pFState_selectionIsValid
  , pFState_copyElts
  , pFState_getSuperSEltByPos
  , pFState_getSEltLabels
  , pFState_maxID
  , sPotatoFlow_to_pFState
  , pFState_to_sPotatoFlow
  , emptyPFState
  , do_newElts
  , undo_newElts
  , do_deleteElts
  , undo_deleteElts

  -- TODO test
  , do_move
  , undo_move

  , do_resizeCanvas
  , undo_resizeCanvas
  , do_manipulate
  , undo_manipulate
) where

import           Relude


import           Potato.Flow.Math
import           Potato.Flow.Layers
import           Potato.Flow.SEltMethods
import           Potato.Flow.Types
import           Potato.Flow.SElts

import           Control.Exception        (assert)
import           Data.Aeson
import qualified Data.IntMap.Strict       as IM
import           Data.Maybe
import qualified Data.Sequence            as Seq


data PFState = PFState {
  _pFState_layers      :: Seq REltId
  -- TODO cache REltId -> Layers map with dirty flag probably
  , _pFState_directory :: REltIdMap SEltLabel
  , _pFState_canvas    :: SCanvas
} deriving (Show, Generic)

instance FromJSON PFState
instance ToJSON PFState
instance NFData PFState

debugPrintPFState :: (IsString a) => PFState -> a
debugPrintPFState PFState {..} = fromString $ "PFState:\n" <> show _pFState_layers <> "\n" <> show (IM.keys _pFState_directory) <> "\n"

pFState_isValid :: PFState -> Bool
pFState_isValid pfs@PFState {..} = pFState_selectionIsValid pfs ([0..Seq.length _pFState_layers - 1])
{-validElts && validScope where
  validElts = all isJust . toList $ fmap ((IM.!?) _pFState_directory) _pFState_layers
  validScope = hasScopingProperty scopeFn _pFState_layers
  scopeFn x = case IM.lookup x _pFState_directory of
    Nothing                            -> Nothing -- this will fail in vaildElts case so it doesn't matter what we do here
    Just (SEltLabel _ SEltFolderStart) -> Just True
    Just (SEltLabel _ SEltFolderEnd)   -> Just False
    _                                  -> Nothing
-}

pFState_selectionIsValid :: PFState -> [LayerPos] -> Bool
pFState_selectionIsValid PFState {..} lps = validElts && validScope where
  validElts = all isJust . toList $ fmap ((IM.!?) _pFState_directory) _pFState_layers
  validScope = selectionHasScopingProperty scopeFn _pFState_layers lps
  scopeFn x = case IM.lookup x _pFState_directory of
    Nothing                            -> Nothing -- this will fail in vaildElts case so it doesn't matter what we do here
    Just (SEltLabel _ SEltFolderStart) -> Just True
    Just (SEltLabel _ SEltFolderEnd)   -> Just False
    _                                  -> Nothing

-- TODO SOMETHING BROKEN HERE
-- lps must be valid
pFState_copyElts :: PFState -> [LayerPos] -> [SEltLabel]
pFState_copyElts PFState {..} lps = r where
  ridfn lp = Seq.index _pFState_layers lp
  seltlfn rid = fromJust $ IM.lookup rid _pFState_directory
  r = map (seltlfn . ridfn) lps

pFState_getSuperSEltByPos :: PFState -> LayerPos -> Maybe SuperSEltLabel
pFState_getSuperSEltByPos PFState {..} lp = do
  rid <- Seq.lookup lp _pFState_layers
  seltl <- IM.lookup rid _pFState_directory
  return (rid, lp, seltl)

pFState_getSEltLabels :: PFState -> [REltId] -> REltIdMap (Maybe SEltLabel)
pFState_getSEltLabels PFState {..} rids = foldr (\rid acc -> IM.insert rid (IM.lookup rid _pFState_directory) acc) IM.empty rids

pFState_maxID :: PFState -> REltId
pFState_maxID s = maybe 0 fst (IM.lookupMax (_pFState_directory s))

emptyPFState :: PFState
emptyPFState = PFState Seq.empty IM.empty (SCanvas (LBox 0 0))

sPotatoFlow_to_pFState :: SPotatoFlow -> PFState
sPotatoFlow_to_pFState SPotatoFlow {..} = r where
  elts = zip [0..] _sPotatoFlow_sEltTree
  dir = foldr (\(rid, e) acc -> IM.insert rid e acc) IM.empty elts
  layers = Seq.fromList (map fst elts)
  r = PFState layers dir _sPotatoFlow_sCanvas

pFState_to_sPotatoFlow :: PFState -> SPotatoFlow
--pFState_to_sPotatoFlow pfs@PFState {..} = trace ("SAVING: " <> debugPrintPFState pfs) $r where
pFState_to_sPotatoFlow PFState {..} = r where
  selttree = toList . fmap (fromJust . \rid -> IM.lookup rid _pFState_directory) $ _pFState_layers
  r = SPotatoFlow _pFState_canvas selttree

do_newElts :: [SuperSEltLabel] -> PFState -> (PFState, SEltLabelChanges)
do_newElts seltls PFState {..} = (r, fmap Just changes) where
  poss = map (\(x,y,_) -> (y,x)) seltls
  els = map (\(x,_,z) -> (x,z)) seltls
  changes = IM.fromList els
  newLayers = insertEltList_indexAfterInsertion poss _pFState_layers
  newDir = changes `IM.union` _pFState_directory
  r = PFState newLayers newDir _pFState_canvas

undo_newElts :: [SuperSEltLabel] -> PFState -> (PFState, SEltLabelChanges)
undo_newElts seltls PFState {..} = (r, changes) where
  poss = map (\(_,y,_) -> y) seltls
  els = map (\(x,_,z) -> (x,z)) seltls
  newLayers = removeEltList poss _pFState_layers
  newDir = _pFState_directory `IM.difference` IM.fromList els
  r = PFState newLayers newDir _pFState_canvas
  changes = IM.fromList $ map (\(x,y)->(x,Nothing)) els

do_deleteElts :: [SuperSEltLabel] -> PFState -> (PFState, SEltLabelChanges)
do_deleteElts = undo_newElts

undo_deleteElts :: [SuperSEltLabel] -> PFState -> (PFState, SEltLabelChanges)
undo_deleteElts = do_newElts

do_move :: ([LayerPos], LayerPos) -> PFState -> (PFState, SEltLabelChanges)
do_move (lps, dst) pfs@PFState {..} = assert (pFState_selectionIsValid pfs lps) (r, changes) where
  r = PFState (moveEltList lps dst _pFState_layers) _pFState_directory _pFState_canvas
  changes = pFState_getSEltLabels pfs (fmap (Seq.index _pFState_layers) lps)
{--
  rids = foldr (\l acc -> Seq.index _pFState_layers l : acc) [] lps
  newLayers' = assert (isSorted lps) $ foldr (\l acc -> Seq.deleteAt l acc) _pFState_layers lps
  moveToIndex = dst - (length (takeWhile (\x -> x < dst) lps))
  (leftL, rightL) = Seq.splitAt moveToIndex newLayers'
  newLayers = leftL >< fromList rids >< rightL
  r = PFState newLayers _pFState_directory _pFState_canvas
--}

undo_move :: ([LayerPos], LayerPos) -> PFState -> (PFState, SEltLabelChanges)
undo_move (lps, dst) pfs@PFState {..} =  (r, changes) where
  r = PFState (undoMoveEltList lps dst _pFState_layers) _pFState_directory _pFState_canvas
  changes = pFState_getSEltLabels pfs (fmap (Seq.index _pFState_layers) lps)
{--
  --assert (isSorted lps)
  nMoved = length lps
  moveToIndex = dst - (length (takeWhile (\x -> x < dst) lps))
  (leftL,rightL') = Seq.splitAt moveToIndex _pFState_layers
  (toMove,rightL) = Seq.splitAt nMoved rightL'
  newLayers' = leftL >< rightL
  newLayers = insertEltList (zip lps (toList toMove)) newLayers'
  r = PFState newLayers _pFState_directory _pFState_canvas
--}

do_resizeCanvas :: DeltaLBox -> PFState -> PFState
do_resizeCanvas d pfs = pfs { _pFState_canvas = newCanvas } where
  newCanvas = SCanvas $ plusDelta (_sCanvas_box (_pFState_canvas pfs)) d

undo_resizeCanvas :: DeltaLBox -> PFState -> PFState
undo_resizeCanvas d pfs = pfs { _pFState_canvas = newCanvas } where
  newCanvas = SCanvas $ minusDelta (_sCanvas_box (_pFState_canvas pfs)) d

manipulate :: Bool -> ControllersWithId -> PFState -> (PFState, SEltLabelChanges)
manipulate isDo cs pfs = (r, fmap Just changes) where
  dir = _pFState_directory pfs
  changes = IM.intersectionWith (updateFnFromController isDo) cs dir
  newDir = IM.union changes dir
  r = pfs { _pFState_directory = newDir }

do_manipulate :: ControllersWithId -> PFState -> (PFState, SEltLabelChanges)
do_manipulate = manipulate True

undo_manipulate :: ControllersWithId -> PFState -> (PFState, SEltLabelChanges)
undo_manipulate = manipulate False
