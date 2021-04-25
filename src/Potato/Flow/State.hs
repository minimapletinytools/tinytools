-- DEPRECATED
-- keeping around because we use the types for testing

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
  , pFState_getLayerPosMap
  , sPotatoFlow_to_pFState
  , pFState_to_sPotatoFlow
  , pFState_toCanvasCoordinates
  , pfState_layerPos_to_superSEltLabel
  , pFState_to_superSEltLabelSeq

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


import           Potato.Flow.Layers
import           Potato.Flow.Math
import           Potato.Flow.SEltMethods
import           Potato.Flow.SElts
import           Potato.Flow.Types

import           Control.Exception       (assert)
import           Data.Aeson
import qualified Data.IntMap.Strict      as IM
import           Data.List.Ordered       (isSorted)
import           Data.Maybe
import qualified Data.Sequence           as Seq


data PFState = PFState {
  -- TODO someday change this to bimap so that we can get rid of _pfo_layerPosMap
  _pFState_layers      :: Seq REltId
  , _pFState_directory :: REltIdMap SEltLabel
  , _pFState_canvas    :: SCanvas
} deriving (Eq, Show, Generic)

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
pFState_selectionIsValid PFState {..} lps = validElts && validScope && sorted where
  validElts = all isJust . toList $ fmap ((IM.!?) _pFState_directory) _pFState_layers
  validScope = selectionHasScopingProperty scopeFn _pFState_layers lps
  sorted = isSorted lps
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

pFState_getLayerPosMap :: PFState -> LayerPosMap
pFState_getLayerPosMap pfs = Seq.foldrWithIndex (\lp rid acc -> IM.insert rid lp acc) IM.empty (_pFState_layers pfs)

emptyPFState :: PFState
emptyPFState = PFState Seq.empty IM.empty (SCanvas (LBox 0 0))

sPotatoFlow_to_pFState :: SPotatoFlow -> PFState
sPotatoFlow_to_pFState SPotatoFlow {..} = r where
  elts = _sPotatoFlow_sEltTree
  dir = foldr (\(rid, e) acc -> IM.insert rid e acc) IM.empty elts
  layers = Seq.fromList (map fst elts)
  r = PFState layers dir _sPotatoFlow_sCanvas

pFState_to_sPotatoFlow :: PFState -> SPotatoFlow
--pFState_to_sPotatoFlow pfs@PFState {..} = trace ("SAVING: " <> debugPrintPFState pfs) $r where
pFState_to_sPotatoFlow PFState {..} = r where
  selttree = toList . fmap (\rid -> (rid, _pFState_directory IM.! rid)) $ _pFState_layers
  r = SPotatoFlow _pFState_canvas selttree

pFState_toCanvasCoordinates :: PFState -> XY -> XY
pFState_toCanvasCoordinates PFState {..} (V2 x y) = V2 (x-sx) (y-sy) where
  LBox (V2 sx sy) _ = _sCanvas_box _pFState_canvas

-- expects LayerPos to be valid in PFState
pfState_layerPos_to_superSEltLabel :: PFState -> LayerPos -> SuperSEltLabel
pfState_layerPos_to_superSEltLabel PFState {..} lp = (rid, lp, seltl) where
  rid = Seq.index _pFState_layers lp
  seltl = (IM.!) _pFState_directory rid

-- i.e. select all
pFState_to_superSEltLabelSeq :: PFState -> Seq SuperSEltLabel
pFState_to_superSEltLabelSeq PFState {..} = Seq.mapWithIndex (\lp rid -> (rid, lp, fromJust $ IM.lookup rid _pFState_directory)) $ _pFState_layers

-- CHANGE [SuperOwl] -> PFState -> (PFState, SEltLabelChanges)
do_newElts :: [SuperSEltLabel] -> PFState -> (PFState, SEltLabelChanges)
do_newElts seltls PFState {..} = (r, fmap Just changes) where
  poss = map (\(x,y,_) -> (y,x)) seltls
  els = map (\(x,_,z) -> (x,z)) seltls
  changes = IM.fromList els
  newLayers = insertEltList_indexAfterInsertion poss _pFState_layers
  newDir = changes `IM.union` _pFState_directory
  r = PFState newLayers newDir _pFState_canvas

-- CHANGE [SuperOwl] -> PFState -> (PFState, SEltLabelChanges)
undo_newElts :: [SuperSEltLabel] -> PFState -> (PFState, SEltLabelChanges)
undo_newElts seltls PFState {..} = (r, changes) where
  poss = map (\(_,y,_) -> y) seltls
  els = map (\(x,_,z) -> (x,z)) seltls
  newLayers = removeEltList poss _pFState_layers
  newDir = _pFState_directory `IM.difference` IM.fromList els
  r = PFState newLayers newDir _pFState_canvas
  changes = IM.fromList $ map (\(x,y)->(x,Nothing)) els

-- CHANGE [SuperOwl] -> PFState -> (PFState, SEltLabelChanges)
do_deleteElts :: [SuperSEltLabel] -> PFState -> (PFState, SEltLabelChanges)
do_deleteElts = undo_newElts

-- CHANGE [SuperOwl] -> PFState -> (PFState, SEltLabelChanges)
undo_deleteElts :: [SuperSEltLabel] -> PFState -> (PFState, SEltLabelChanges)
undo_deleteElts = do_newElts

--
-- CHANGE
-- | (list of parents (assert no repeats), target (placed after or as first child if top owl (no parent)))
--do_move :: ([REltId], Maybe REltId) -> PFState  -> (PFState, SEltLabelChanges)
-- TODO assert selection has all children
do_move :: ([LayerPos], LayerPos) -> PFState -> (PFState, SEltLabelChanges)
do_move (lps, dst) pfs@PFState {..} = assert (pFState_selectionIsValid pfs lps) (r, changes) where
  -- TODO something like this
  --lps' = addChildren lps pfs
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
  -- TODO something like this
  --lps' = addChildren lps pfs
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

-- | check if the SCanvas is valid or not
-- for now, canvas offset must always be 0, I forget why it's even an option to offset the SCanvas, probably potatoes.
isValidCanvas :: SCanvas -> Bool
isValidCanvas (SCanvas (LBox p (V2 w h))) = p == 0 && w > 0 && h > 0

do_resizeCanvas :: DeltaLBox -> PFState -> PFState
do_resizeCanvas d pfs = assert (isValidCanvas newCanvas) $ pfs { _pFState_canvas = newCanvas } where
  newCanvas = SCanvas $ plusDelta (_sCanvas_box (_pFState_canvas pfs)) d

undo_resizeCanvas :: DeltaLBox -> PFState -> PFState
undo_resizeCanvas d pfs = assert (isValidCanvas newCanvas) $ pfs { _pFState_canvas = newCanvas } where
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
