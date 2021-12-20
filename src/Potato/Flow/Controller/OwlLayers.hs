{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Potato.Flow.Controller.OwlLayers where

import           Relude

import           Potato.Flow.Controller.Types
import           Potato.Flow.Types
import Potato.Flow.SElts
import           Potato.Flow.Owl
import           Potato.Flow.OwlState

import           Control.Lens                 (over, _2)
import Data.Foldable (foldl)
import           Data.Default
import qualified Data.IntMap                  as IM
import           Data.Sequence                ((><), (|>))
import qualified Data.Sequence                as Seq
import qualified Data.Text as T

data LockHiddenState = LHS_True | LHS_False | LHS_True_InheritTrue | LHS_False_InheritTrue deriving (Eq, Show)

lockHiddenStateToBool :: LockHiddenState -> Bool
lockHiddenStateToBool = \case
  LHS_False -> False
  _ -> True

toggleLockHiddenState :: LockHiddenState -> LockHiddenState
toggleLockHiddenState = \case
  LHS_True -> LHS_False
  LHS_False -> LHS_True
  LHS_True_InheritTrue -> LHS_False_InheritTrue
  LHS_False_InheritTrue -> LHS_True_InheritTrue

setLockHiddenStateInChildren :: LockHiddenState -> Bool -> LockHiddenState
setLockHiddenStateInChildren parentstate = \case
  False -> case parentstate of
    LHS_False -> LHS_False
    _         -> LHS_False_InheritTrue
  True -> case parentstate of
    LHS_False -> LHS_True
    _         -> LHS_True_InheritTrue

-- ancestor state got set, update the child
updateLockHiddenStateInChildren :: LockHiddenState -> LockHiddenState -> LockHiddenState
updateLockHiddenStateInChildren parentstate = \case
  LHS_False -> case parentstate of
    LHS_True  -> LHS_False_InheritTrue
    LHS_False -> LHS_False
    _         -> invalid
  LHS_True -> case parentstate of
    LHS_True  -> LHS_True_InheritTrue
    LHS_False -> LHS_True
    _         -> invalid
  LHS_True_InheritTrue -> case parentstate of
    LHS_False -> LHS_True
    LHS_True  -> LHS_True_InheritTrue
    _         -> invalid
  LHS_False_InheritTrue -> case parentstate of
    LHS_False -> LHS_False
    LHS_True  -> LHS_False_InheritTrue
    _         -> invalid
  where
    invalid = error "toggling of LHS_XXX_InheritTrue elements disallowed"

-- TODO be careful with hidden cost of Eq SuperOwl
-- this stores info just for what is displayed, Seq LayerEntry is uniquely generated from LayerMetaMap and PFState
data LayerEntry = LayerEntry {
  _layerEntry_lockState      :: LockHiddenState
  , _layerEntry_hideState      :: LockHiddenState
  , _layerEntry_isCollapsed    :: Bool -- this parameter is ignored if not a folder, Maybe Bool instead?
  , _layerEntry_superOwl :: SuperOwl
} deriving (Eq, Show)

layerEntry_depth :: LayerEntry -> Int
layerEntry_depth LayerEntry {..} = _owlEltMeta_depth . _superOwl_meta $ _layerEntry_superOwl

layerEntry_display :: LayerEntry -> Text
layerEntry_display LayerEntry {..} = isOwl_name _layerEntry_superOwl

layerEntry_isFolder :: LayerEntry -> Bool
layerEntry_isFolder LayerEntry {..} = mommyOwl_hasKiddos _layerEntry_superOwl

layerEntry_rEltId :: LayerEntry -> REltId
layerEntry_rEltId LayerEntry {..} = _superOwl_id _layerEntry_superOwl

-- index type into Seq LayerEntry
type LayerEntryPos = Int
type LayerEntries = Seq LayerEntry

layerEntriesToPrettyText :: LayerEntries -> Text
layerEntriesToPrettyText lentries = foldr foldrfn "" lentries where
  foldrfn le@LayerEntry {..} acc = r where
    collapseText = if layerEntry_isFolder le
      then if _layerEntry_isCollapsed
        then ">"
        else "v"
      else " "
    hideText = case _layerEntry_hideState of
      LHS_True              -> "▓"
      LHS_False             -> " "
      LHS_True_InheritTrue  -> "▓"
      LHS_False_InheritTrue -> "▒"
    lockText = case _layerEntry_lockState of
      LHS_True              -> "▓"
      LHS_False             -> " "
      LHS_True_InheritTrue  -> "▓"
      LHS_False_InheritTrue -> "▒"
    sowl = _layerEntry_superOwl
    r = T.replicate (layerEntry_depth le) " " <> collapseText <> hideText <> lockText <> " " <> isOwl_name sowl <> "\n" <> acc

data LayersState = LayersState {
    -- mapping from REltId to element meta data
    _layersState_meta :: LayerMetaMap
    -- sequence of visible folders
    , _layersState_entries :: LayerEntries
    , _layersState_scrollPos :: Int
  } deriving (Show, Eq)

data LockHideCollapseOp = LHCO_ToggleLock | LHCO_ToggleHide | LHCO_ToggleCollapse deriving (Show)

alterWithDefault :: (Eq a, Default a) => (a -> a) -> REltId -> REltIdMap a -> REltIdMap a
alterWithDefault f k m = IM.alter f' k m where
  apply x = if fx == def then Nothing else Just fx where
    fx = f x
  f' = \case
    Nothing -> apply def
    Just x -> apply x

lookupWithDefault :: (Default a) => REltId -> REltIdMap a -> a
lookupWithDefault rid ridm = case IM.lookup rid ridm of
  Nothing -> def
  Just x  -> x





-- TODO test
-- | assumes LayersState is after hide state of given lepos has just been toggled
changesFromToggleHide :: OwlPFState -> LayersState -> LayerEntryPos -> SuperOwlChanges
changesFromToggleHide OwlPFState {..} LayersState {..} lepos = r where
  le = Seq.index _layersState_entries lepos
  sowl = _layerEntry_superOwl le
  lerid = _superOwl_id sowl
  lm = lookupWithDefault lerid _layersState_meta
  isHidden = _layerMeta_isHidden lm

  -- find all children that weren't already hidden
  children = owliteratechildrenat _owlPFState_owlTree lerid
  isunhidden sowl' = not . _layerMeta_isHidden $ lookupWithDefault (_superOwl_id sowl') _layersState_meta
  unhiddenChildren = toList . fmap (\sowl' -> (_superOwl_id sowl', sowl')) $ Seq.filter isunhidden children

  r = if isHidden
    then IM.fromList $ (lerid, Nothing) : (fmap (over _2 (const Nothing)) unhiddenChildren)
    else IM.fromList $ (lerid,Just sowl) : (fmap (over _2 Just) unhiddenChildren)

-- iterates over LayerEntryPos, skipping all children of entries where skipfn evaluates to true
doChildrenRecursive :: (LayerEntry -> Bool) -> (LayerEntry -> LayerEntry) -> Seq LayerEntry -> Seq LayerEntry
doChildrenRecursive skipfn entryfn = snd . mapAccumL mapaccumlfn maxBound where
  mapaccumlfn skipdepth le = (newskipdepth, newle) where
    depth = layerEntry_depth le
    newskipdepth
      -- skip, so keep skipping
      | depth >= skipdepth = skipdepth
      -- skip all children
      -- note, no need to check for collapsed state because we are iterating over LayerEntry which do not include children of collapsed entries
      | skipfn le = depth + 1
      -- either we exited a skipped folder or aren't skipping, reset skip counter (since we skip subfolders of skipped entries, maximal skip stack depth is 1 so reset is OK)
      | otherwise = maxBound
    newle = if depth >= skipdepth
      then le -- no changes to skipped elts
      else entryfn le


toggleLayerEntry :: OwlPFState -> LayersState -> LayerEntryPos -> LockHideCollapseOp -> LayersState
toggleLayerEntry OwlPFState {..} LayersState {..} lepos op = r where
  le = Seq.index _layersState_entries lepos
  lerid = layerEntry_rEltId le
  ledepth = layerEntry_depth le
  childFrom nextLayerEntry = layerEntry_depth nextLayerEntry /= ledepth
  -- visible children of le
  childles = Seq.takeWhileL childFrom . Seq.drop (lepos+1) $ _layersState_entries
  -- everything before le
  frontOfLe = Seq.take lepos _layersState_entries
  -- everything after childles
  backOfChildles = Seq.drop (lepos + 1 + Seq.length childles) _layersState_entries

  -- simple helper function for setting lock/hidden state
  togglefn fn setlmfn setlefn = (LayersState newlmm newlentries 0) where
    newlhsstate = toggleLockHiddenState $ fn le
    newlmm = alterWithDefault (\lm' -> setlmfn lm' (lockHiddenStateToBool newlhsstate)) lerid _layersState_meta
    entryfn childle = setlefn childle $ updateLockHiddenStateInChildren newlhsstate (fn childle)
    newchildles = doChildrenRecursive (lockHiddenStateToBool . fn) entryfn childles
    newle = setlefn le newlhsstate
    newlentries = (frontOfLe |> newle) >< newchildles >< backOfChildles

  r = case op of
    LHCO_ToggleCollapse -> (LayersState newlmm newlentries 0) where
      newcollapse = not $ _layerEntry_isCollapsed le
      newlmm = alterWithDefault (\le' -> le' { _layerMeta_isCollapsed = newcollapse }) lerid _layersState_meta
      newle = le { _layerEntry_isCollapsed = newcollapse }
      newchildles = buildLayerEntriesRecursive _owlPFState_owlTree _layersState_meta Seq.empty (Just newle)
      newlentries = if newcollapse
        then (frontOfLe |> newle) >< backOfChildles
        else (frontOfLe |> newle) >< newchildles >< backOfChildles

    LHCO_ToggleLock -> togglefn _layerEntry_lockState (\lm' x -> lm' { _layerMeta_isLocked = x }) (\le' x -> le' { _layerEntry_lockState = x })
    LHCO_ToggleHide -> togglefn _layerEntry_hideState (\lm' x -> lm' { _layerMeta_isHidden = x }) (\le' x -> le' { _layerEntry_hideState = x })


makeLayersStateFromOwlPFState :: OwlPFState -> LayerMetaMap -> LayersState
makeLayersStateFromOwlPFState pfs lmm = LayersState {
    _layersState_meta = lmm
    , _layersState_entries = generateLayersNew (_owlPFState_owlTree pfs) lmm
    , _layersState_scrollPos = 0
  }

updateLayers :: OwlPFState -> SuperOwlChanges -> LayersState -> LayersState
updateLayers pfs changes LayersState {..} = r where
  -- update _layersState_meta
  (deletestuff, maybenewstuff) = IM.partition isNothing changes

  maybenewstuffcollapsed = (fmap (const (def {_layerMeta_isCollapsed = True})) maybenewstuff)

  newlmm = IM.difference (IM.union _layersState_meta maybenewstuffcollapsed) deletestuff


  -- keep deleted elts so that folder state is preserved after undos/redos
  --newlmm = IM.union _layersState_meta (fmap (const def) maybenewstuff)

  -- TODO incremental rather than regenerate...
  newlentries = generateLayersNew (_owlPFState_owlTree pfs) newlmm

  r = LayersState newlmm newlentries _layersState_scrollPos

buildLayerEntriesRecursive :: OwlTree -> LayerMetaMap -> Seq LayerEntry -> Maybe LayerEntry -> Seq LayerEntry
buildLayerEntriesRecursive ot lmm acc mparent = r where
  foldlfn acclentries rid = newacclentries where
    sowl = owlTree_mustFindSuperOwl ot rid
    lm = lookupWithDefault rid lmm
    -- add self
    lentry = case mparent of
      Nothing -> LayerEntry {
        _layerEntry_lockState =  if _layerMeta_isLocked lm then LHS_True else LHS_False
        , _layerEntry_hideState = if _layerMeta_isHidden lm then LHS_True else LHS_False
        , _layerEntry_isCollapsed = _layerMeta_isCollapsed lm
        , _layerEntry_superOwl = sowl
      }
      Just parent -> LayerEntry {
        _layerEntry_lockState = setLockHiddenStateInChildren (_layerEntry_lockState parent) $ _layerMeta_isLocked lm
        , _layerEntry_hideState = setLockHiddenStateInChildren (_layerEntry_hideState parent) $ _layerMeta_isHidden lm
        , _layerEntry_isCollapsed = _layerMeta_isCollapsed lm
        , _layerEntry_superOwl = sowl
      }
    newacclentries' = acclentries |> lentry
    -- recursively add children
    newacclentries = if _layerMeta_isCollapsed lm
      then newacclentries'
      else buildLayerEntriesRecursive ot lmm newacclentries' (Just lentry)
  r = foldl foldlfn acc $ fromMaybe Seq.empty $ case mparent of
    Nothing -> mommyOwl_kiddos ot
    Just lentry -> mommyOwl_kiddos (_layerEntry_superOwl lentry)

generateLayersNew :: OwlTree -> LayerMetaMap -> Seq LayerEntry
generateLayersNew ot lmm = buildLayerEntriesRecursive ot lmm Seq.empty Nothing
