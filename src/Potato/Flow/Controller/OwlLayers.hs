
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- TODO move to Potato.Flow.Controller
module Potato.Flow.Controller.OwlLayers where

import           Relude

import           Potato.Flow.Controller.Types
import           Potato.Flow.Types
import Potato.Flow.Serialization.Snake
import           Potato.Flow.OwlItem
import Potato.Flow.Owl
import Potato.Flow.OwlState
import Potato.Flow.DebugHelpers

import           Control.Lens                 (over, _2)
import Data.Foldable (foldl)
import           Data.Default
import qualified Data.IntMap                  as IM
import qualified Data.IntSet as IS
import           Data.Sequence                ((><), (|>))
import qualified Data.Sequence                as Seq
import  Data.Tree (Tree)
import qualified Data.Tree as Tree
import qualified Data.Tree.Zipper as Tree
import qualified Data.Text as T
import qualified Text.Show

data LockHiddenState = 
  LHS_True -- locked/hidden 
  | LHS_False -- not locked/hidden
  | LHS_True_InheritTrue -- locked/hidden and has locked/hidden parent
  | LHS_False_InheritTrue -- not locked/hidden and has locked/hidden parent 
  deriving (Eq, Show)

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
    LHS_True_InheritTrue -> LHS_False_InheritTrue
    LHS_False_InheritTrue -> LHS_False_InheritTrue
  LHS_True -> case parentstate of
    LHS_True  -> LHS_True_InheritTrue
    LHS_False -> LHS_True
    LHS_True_InheritTrue -> LHS_True_InheritTrue
    LHS_False_InheritTrue -> LHS_True_InheritTrue
  LHS_True_InheritTrue -> case parentstate of
    LHS_True  -> LHS_True_InheritTrue
    LHS_False -> LHS_True
    LHS_True_InheritTrue -> LHS_True_InheritTrue
    LHS_False_InheritTrue -> LHS_True_InheritTrue
  LHS_False_InheritTrue -> case parentstate of
    LHS_True  -> LHS_False_InheritTrue
    LHS_False -> LHS_False
    LHS_True_InheritTrue -> LHS_False_InheritTrue
    LHS_False_InheritTrue -> LHS_False_InheritTrue

-- TODO be careful with hidden cost of Eq SuperOwl
-- this stores info just for what is displayed, Seq LayerEntry is uniquely generated from LayerMetaMap and PFState
data LayerEntry = LayerEntry {
  _layerEntry_lockState      :: LockHiddenState
  , _layerEntry_hideState      :: LockHiddenState
  , _layerEntry_isCollapsed    :: Bool -- this parameter is ignored if not a folder, Maybe Bool instead?
  , _layerEntry_superOwl :: SuperOwl
} deriving (Eq)

instance Show LayerEntry where
  show LayerEntry {..} = "LayerEntry (lhc: "
    <> show _layerEntry_lockState
    <> "," <> show _layerEntry_hideState
    <> "," <> show _layerEntry_isCollapsed
    <> "):\n" <> (T.unpack $ potatoShow _layerEntry_superOwl)

layerEntry_depth :: LayerEntry -> Int
layerEntry_depth LayerEntry {..} = _owlItemMeta_depth . _superOwl_meta $ _layerEntry_superOwl

layerEntry_display :: LayerEntry -> Text
layerEntry_display LayerEntry {..} = hasOwlItem_name _layerEntry_superOwl

layerEntry_isFolder :: LayerEntry -> Bool
layerEntry_isFolder LayerEntry {..} = mommyOwl_hasKiddos _layerEntry_superOwl

layerEntry_rEltId :: LayerEntry -> REltId
layerEntry_rEltId LayerEntry {..} = _superOwl_id _layerEntry_superOwl

-- TODO DELETE just use Int
type LayerEntryPos = Int

-- DEPRECATED
type LayerEntries = Seq LayerEntry

type LayerEntryTree = Tree LayerEntry

layerEntryTree_index :: LayerEntryTree -> LayerEntryPos -> Maybe LayerEntry
layerEntryTree_index nles lepos = fst $ foldl f (Nothing, 0) nles where
  f (mle, acc) le = if acc == lepos
    then (Just le, acc + 1)
    else (mle, acc + 1)

layerEntryTree_toLayerEntries :: LayerEntryTree -> LayerEntries
layerEntryTree_toLayerEntries nles = r where
  foldlfn acc le = acc |> le
  r = foldl foldlfn Seq.empty nles

maybeApplyNTimes :: Int -> (a -> Maybe a) -> a -> Maybe a
maybeApplyNTimes n f val = foldl (\s e -> e s) (Just val) [f | x <- [1..n]] where
  foldlfn acc x = case acc of
    Nothing -> Nothing
    Just x' -> f x'



layerEntries_toLayerEntryTree :: LayerEntries -> LayerEntryTree
layerEntries_toLayerEntryTree les = foldl foldlfn Tree.empty les where
  foldlfn z le = r where
    lastDepth = layerEntry_depth Tree.label z
    depth = layerEntry_depth le
    r = if depth == lastDepth+1
      -- child case
      then Tree.insert (Node le []) (Tree.children z')
      else if depth < lastDepth
        -- go back up case
        then case maybeApplyNTimes (depth - lastDepth) Tree.parent z of
          Nothing -> error "not enough parents"
          Just z' -> Tree.insert (Node le []) (Tree.nextSpace z')
        else error "layerEntries_toLayerEntryTree: depth > lastDepth+1"
    



maybeLeft :: Either a b -> Maybe a
maybeLeft (Left a) = Just a
maybeLeft _        = Nothing


indexToZipper :: a -> Int -> Maybe (Tree.TreePos Tree.Full a)
indexToZipper tree i = maybeLeft $ parentGo (Tree.fromTree tree) i where

  childrenGo :: Maybe (Tree.TreePos Tree.Full a) -> Int -> Either (Tree.TreePos Tree.Full a) Int
  childrenGo mz n = case mz of
    -- if n is 0, then parentGo will handle the base case
    Nothing -> Right n
    Just z -> case parentGo z n of
      -- done
      Left z' -> Left z'
      -- go to next child
      Right n' -> childrenGo (Tree.next z') (n-n')


  parentGo :: Tree.TreePos Tree.Full a -> Int -> Either (Tree.TreePos Tree.Full a) Int
  parentGo z n' = case n' of
    -- we're done
    0 -> Left z
    -- walk over chlidren
    n -> childrenGo (firstChild z) (n-1)


layerEntriesToPrettyText :: LayerEntries -> Text
layerEntriesToPrettyText lentries = foldr foldrfn "" lentries where
  foldrfn le@LayerEntry {..} acc = r where
    collapseText = if layerEntry_isFolder le
      then if _layerEntry_isCollapsed
        then ">"
        else "v"
      else "●"
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
    r = T.replicate (layerEntry_depth le) " " <> collapseText <> hideText <> lockText <> " " <> hasOwlItem_name sowl <> "\n" <> acc

layerEntryTreeToPrettyText :: LayerEntryTree -> Text
layerEntryTreeToPrettyText = layerEntriesToPrettyText . layerEntryTree_toLayerEntries 


data LayersState = LayersState {
    -- mapping from REltId to element meta data
    _layersState_meta :: LayerMetaMap
    -- tree of visible folders in layers
    , _layersState_entries :: LayerEntryTree
    -- TODO DELETE
    , _layersState_scrollPos :: Int
  } deriving (Show, Eq)

instance PotatoShow LayersState where
  potatoShow LayersState{..} = r where
    r = "LayersState: "
      <> show _layersState_meta
      <> "\nLayerEntries:\n"
      <> layerEntriesToPrettyText _layersState_entries

--instance Show LayersState where
--  show ls = T.unpack (potatoShow ls)


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


-- | assumes `LayersState` has updated hide state of `lepos`
changesFromToggleHide :: OwlPFState -> LayersState -> LayerEntryPos -> SuperOwlChanges
changesFromToggleHide OwlPFState {..} LayersState {..} lepos = r where
  le = layerEntryTree_index _layersState_entries lepos
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


updateChildrenRecursively :: (LayerEntry -> LockHiddenState) -> (LayerEntry -> LayerEntry) -> LayerEntryTree -> LayerEntryTree


toggleLayerEntry :: OwlPFState -> LayersState -> LayerEntryPos -> LockHideCollapseOp -> LayersState
toggleLayerEntry OwlPFState {..} LayersState {..} lepos op = r where

  le = layerEntryTree_index _layersState_entries lepos
  lezip = indexToZipper _layersState_entries lepos
  lerid = layerEntry_rEltId le

  -- simple helper function for setting lock/hidden state
  -- TODO do it incrementally instead of regenerating the tree
  togglefn fn setlmfn setlefn = (LayersState newlmm newlentries 0) where
    newlhsstate = toggleLockHiddenState $ fn le
    newlmm = alterWithDefault (\lm' -> setlmfn lm' (lockHiddenStateToBool newlhsstate)) lerid _layersState_meta
    newlentries = buildLayerEntryTree _owlPFState_owlTree newlmm


  r = case op of
    LHCO_ToggleCollapse -> (LayersState newlmm newlentries 0) where
      newcollapse = not $ _layerEntry_isCollapsed le
      newlmm = alterWithDefault (\le' -> le' { _layerMeta_isCollapsed = newcollapse }) lerid _layersState_meta
      newle = le { _layerEntry_isCollapsed = newcollapse }
      newchildles = buildLayerEntriesRecursive _owlPFState_owlTree _layersState_meta Seq.empty (Just newle)
      newlentries = Tree.toTree $ if newcollapse
        then Tree.modifyTree (\(Tree.Node label _) -> (Tree.Node label [])) lezip 
        else Tree.modifyTree (\(Tree.Node label _) -> (Tree.Node label newchildles)) lezip 
    LHCO_ToggleLock -> togglefn _layerEntry_lockState (\lm' x -> lm' { _layerMeta_isLocked = x }) (\le' x -> le' { _layerEntry_lockState = x })
    LHCO_ToggleHide -> togglefn _layerEntry_hideState (\lm' x -> lm' { _layerMeta_isHidden = x }) (\le' x -> le' { _layerEntry_hideState = x })

expandAllCollapsedParents :: Selection -> OwlPFState -> LayersState -> LayersState
expandAllCollapsedParents selection pfs ls = r where
  ops = owlParliamentSet_findParents (hasOwlTree_owlTree pfs) . superOwlParliament_toOwlParliamentSet $  selection
  oldlmm = _layersState_meta ls
  iscollapsedfilterfn rid = case IM.lookup rid oldlmm of
    Just lm -> _layerMeta_isCollapsed lm
    Nothing -> defaultFolderCollapseState
  collapsedParents = IS.filter iscollapsedfilterfn ops
  alterfn mlm = case mlm of
    Nothing -> Just (def { _layerMeta_isCollapsed = False })
    Just x -> Just (x { _layerMeta_isCollapsed = False })
  newlmm = IS.foldr (IM.alter alterfn) oldlmm collapsedParents
  r = if IS.null collapsedParents
    then ls
    -- can we do this more efficiently?
    else (makeLayersStateFromOwlPFState pfs newlmm) { _layersState_scrollPos = _layersState_scrollPos ls }

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


buildLayerEntryTree :: OwlTree -> LayerMetaMap -> LayerEntryTree
buildLayerEntryTree ot lmm = layerEntries_toLayerEntryTree $ buildLayerEntriesRecursive ot lmm Seq.empty Nothing


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
--generateLayersNew ot lmm = buildLayerEntriesRecursive ot lmm Seq.empty Nothing
generateLayersNew = buildLayerEntryTreeot lmm


layerMetaMap_isInheritHiddenOrLocked :: OwlTree -> REltId -> LayerMetaMap -> Bool
layerMetaMap_isInheritHiddenOrLocked ot rid lmm = case IM.lookup rid lmm of
  -- these may both be false, but it may inherit from a parent where these are true therefore we still need to walk up the tree if these are both false
  Just lm | _layerMeta_isLocked lm || _layerMeta_isHidden lm -> True
  _ -> case IM.lookup rid (_owlTree_mapping ot) of
    Nothing -> False
    Just (oem,_) -> layerMetaMap_isInheritHiddenOrLocked ot (_owlItemMeta_parent oem) lmm

layerMetaMap_isInheritHidden :: OwlTree -> REltId -> LayerMetaMap -> Bool
layerMetaMap_isInheritHidden ot rid lmm = case IM.lookup rid lmm of
  Just lm | _layerMeta_isHidden lm -> True
  _ -> case IM.lookup rid (_owlTree_mapping ot) of
    Nothing -> False
    Just (oem,_) -> layerMetaMap_isInheritHidden ot (_owlItemMeta_parent oem) lmm
