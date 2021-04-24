{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Owl where

import           Relude

import           Potato.Flow.Types
import           Potato.Flow.SElts

import           Control.Exception (assert)
import qualified Data.IntMap       as IM
import           Data.Sequence     ((|>), (><))
import qualified Data.Sequence     as Seq
import qualified Data.Set as Set
import Data.Foldable (foldl)
import Data.Maybe (fromJust)
import qualified Data.Text as T


errorMsg_owlTree_lookupFail :: OwlTree -> REltId -> Text
errorMsg_owlTree_lookupFail OwlTree {..} rid = errorMsg_owlMapping_lookupFail _owlTree_mapping rid

errorMsg_owlMapping_lookupFail :: OwlMapping -> REltId -> Text
errorMsg_owlMapping_lookupFail om rid = "expected to find REltId " <> show rid <> " in OwlMapping"

class MommyOwl o where
  mommyOwl_kiddos :: o -> Maybe (Seq REltId)

-- TODO Consider moving OwlInfo into meta?
data OwlInfo = OwlInfo { _owlInfo_name :: Text } deriving (Show, Generic)

instance NFData OwlInfo

-- TODO rename to just Owl
--data OwlElt = OwlEltFolder OwlInfo (Seq OwlElt) | OwlEltSElt OwlInfo SElt deriving (Show, Generic)
data OwlElt = OwlEltFolder OwlInfo (Seq REltId) | OwlEltSElt OwlInfo SElt deriving (Show, Generic)

instance NFData OwlElt

instance MommyOwl OwlElt where
  mommyOwl_kiddos (OwlEltFolder  _ kiddos) = Just kiddos
  mommyOwl_kiddos _ = Nothing

type OwlMapping = REltIdMap (OwlEltMeta, OwlElt)

owlElt_name :: OwlElt -> Text
owlElt_name (OwlEltFolder (OwlInfo name) _) = name
owlElt_name (OwlEltSElt (OwlInfo name) _) = name

-- TODO decide if we want some relative position index or if we just want to use true index (and recompute on move/add/delete)
type SemiPos = Int

-- UNTESTED
-- TODO change this to do a binary search (once you have decided SemiPos is what you want and not actual position)
-- returns index of SemiPos in a Seq
-- if there is no exact match, returns position to the "right" of semipos
locateFromSemiPos :: (a -> SemiPos) -> Seq a -> SemiPos -> Int
locateFromSemiPos f s sp = Seq.length $ Seq.takeWhileL (\a -> f a < sp) s

-- UNTESTED
-- TODO make an owlTree method?
owlMappingSemiPosLookup :: OwlMapping -> REltId -> SemiPos
owlMappingSemiPosLookup om rid = case IM.lookup rid om of
  Nothing -> error $ "expected to find rid " <> show rid
  Just (oem,_) -> _owlEltMeta_relPosition oem

-- TODO DELETE
-- UNTESTED
locateOwlFromSemiPos :: OwlMapping -> Seq REltId -> SemiPos -> Int
locateOwlFromSemiPos om s sp = locateFromSemiPos (owlMappingSemiPosLookup om) s sp

-- UNTESTED
locateLeftSiblingIdFromSemiPos :: OwlMapping -> Seq REltId -> SemiPos -> Maybe REltId
locateLeftSiblingIdFromSemiPos om s sp = case locateFromSemiPos (owlMappingSemiPosLookup om) s sp of
  0 -> Nothing
  x -> Just $ x - 1

-- UNTESTED
-- in this case, we remove only if there is an exact match
removeAtSemiPos :: (a -> SemiPos) -> Seq a -> SemiPos -> Seq a
removeAtSemiPos f s sp = r where
  (front, back) = Seq.breakl (\a -> f a == sp) s
  r = front >< Seq.drop 1 back

-- UNTESTED
-- TODO make an owlTree method?
removeSuperOwlFromSeq :: OwlMapping -> Seq REltId -> SuperOwl -> Seq REltId
removeSuperOwlFromSeq om s so = assert (Seq.length s == Seq.length r + 1) r where
  sp = _owlEltMeta_relPosition . _superOwl_meta $ so
  r = removeAtSemiPos (owlMappingSemiPosLookup om) s sp

isDescendentOf :: (HasCallStack) => OwlMapping -> REltId -> REltId -> Bool
isDescendentOf om parent child
  | child == noOwl = False
  | otherwise = r where
    parent' = case IM.lookup child om of
      Just (oem,_) -> _owlEltMeta_parent oem
      Nothing -> error $ errorMsg_owlMapping_lookupFail om child
    r = case parent' of
      x | x == noOwl -> False
      x | x == parent -> True
      x -> isDescendentOf om x parent

-- TODO do we need this, maybe just change to OwlSpot?
data OwlEltMeta = OwlEltMeta {
  _owlEltMeta_parent :: REltId -- or should we do Maybe REltId?
  , _owlEltMeta_depth :: Int
  , _owlEltMeta_relPosition :: SemiPos -- TODO maybe change to leftSibling?
} deriving (Show, Generic)

instance NFData OwlEltMeta

owlEltMeta_prettyPrintForDebugging :: OwlEltMeta -> Text
owlEltMeta_prettyPrintForDebugging OwlEltMeta {..} = "(meta: " <> show _owlEltMeta_parent <> " " <> show _owlEltMeta_depth <> " " <> show _owlEltMeta_relPosition <> ")"

-- |
-- throws if OwlEltMeta is invalid in OwlTree
owlEltMeta_toOwlSpot :: OwlTree -> OwlEltMeta -> OwlSpot
owlEltMeta_toOwlSpot od@OwlTree {..} OwlEltMeta {..} = r where
  msiblings = case _owlEltMeta_parent of
    x | x == noOwl -> return _owlTree_topOwls
    x -> do
      (_, oelt) <- IM.lookup x _owlTree_mapping
      mommyOwl_kiddos oelt

  siblings = fromJust msiblings
  r = OwlSpot {
    _owlSpot_parent = _owlEltMeta_parent
    , _owlSpot_leftSibling = locateLeftSiblingIdFromSemiPos _owlTree_mapping siblings _owlEltMeta_relPosition
  }

-- a simpler version of OwlEltMeta used for inserting new Owls
data OwlSpot = OwlSpot {
  _owlSpot_parent :: REltId
  , _owlSpot_leftSibling :: Maybe REltId
} deriving (Show, Generic)

instance NFData OwlSpot

data SuperOwl = SuperOwl {
  _superOwl_id :: REltId
  , _superOwl_meta :: OwlEltMeta
  , _superOwl_elt :: OwlElt
} deriving (Show, Generic)

instance NFData SuperOwl

type SuperOwlChanges = REltIdMap (Maybe SuperOwl)

instance MommyOwl SuperOwl where
  mommyOwl_kiddos sowl = mommyOwl_kiddos (_superOwl_elt sowl)

superOwl_prettyPrintForDebugging :: SuperOwl -> Text
superOwl_prettyPrintForDebugging SuperOwl{..} = show _superOwl_id <> " " <> owlEltMeta_prettyPrintForDebugging _superOwl_meta <> " " <> elt where
  elt = case _superOwl_elt of
    OwlEltFolder oi kiddos -> "folder: " <> (_owlInfo_name oi)
    OwlEltSElt oi selt -> "elt: " <> (_owlInfo_name oi) -- TODO elt type

--superOwl_id :: Lens' SuperOwl REltId
superOwl_id :: Functor f => (REltId -> f REltId) -> SuperOwl -> f SuperOwl
superOwl_id f sowl = fmap (\rid -> sowl { _superOwl_id = rid }) (f (_superOwl_id sowl))

-- TODO rest of lenses

superOwl_isTopOwl :: SuperOwl -> Bool
superOwl_isTopOwl SuperOwl {..} = _owlEltMeta_depth _superOwl_meta == 0

-- | same as superOwl_isTopOwl except checks all conditions, intended to be used in asserts
superOwl_isTopOwlSurely :: SuperOwl -> Bool
superOwl_isTopOwlSurely SuperOwl {..}  = _owlEltMeta_depth _superOwl_meta == 0 &&_owlEltMeta_parent _superOwl_meta == noOwl

noOwl :: REltId
noOwl = -1


-- you can prob delete this?
-- if parent is selected, then kiddos must not be directly included in the parliament
newtype OwlParliament = OwlParliament { unOwlParliament :: Seq REltId } deriving (Show, Generic)

instance NFData OwlParliament

-- if parent is selected, then kiddos must not be directly included in the parliament
-- same as OwlParialment but contains more information
newtype SuperOwlParliament = SuperOwlParliament { unSuperOwlParliament :: Seq SuperOwl } deriving (Show, Generic)

instance NFData SuperOwlParliament

owlParliament_toSuperOwlParliament :: OwlTree -> OwlParliament -> SuperOwlParliament
owlParliament_toSuperOwlParliament od@OwlTree{..} op = SuperOwlParliament $ fmap f (unOwlParliament op) where
  f rid = case IM.lookup rid _owlTree_mapping of
    Nothing -> error $ errorMsg_owlTree_lookupFail od rid
    Just (oem,oe) -> SuperOwl rid oem oe

-- check if a mommy owl is selected, that no descendant of that mommy owl is selected
superOwlParliament_isValid :: OwlMapping -> SuperOwlParliament -> Bool
superOwlParliament_isValid om (SuperOwlParliament owls) = r where
  kiddosFirst = Seq.sortBy (\a b -> flip compare (_owlEltMeta_depth (_superOwl_meta a)) (_owlEltMeta_depth (_superOwl_meta b))) owls
  acc0 = (Set.empty, Set.fromList . toList . fmap _superOwl_id $ owls, True)
  foldlfn (visited, mommies', passing) sowl = (nextVisited, mommies, pass && passing) where

    -- remove self from list of mommies
    -- TODO you  don't actually need to check two elts at the same level, you can be smarter about removing mommies at each level
    mommies = Set.delete (_superOwl_id sowl) mommies'

    checkMommyRec rid toVisit = case rid of
      -- made it to the top
      x | x == noOwl -> (toVisit, True)
      _ -> case Set.member rid visited of
        -- we've been here before, must be OK
        True -> (toVisit, True)
        False -> case Set.member rid mommies of
          -- one of our mommies, not OK
          True -> (toVisit, False)
          False -> case IM.lookup rid om of
            Nothing -> error $ errorMsg_owlMapping_lookupFail om rid
            -- add self to list of mommies to visit and recurse
            Just (oem,_) -> checkMommyRec (_owlEltMeta_parent oem) (Set.insert rid toVisit)
    (toVisit, pass) = checkMommyRec (_owlEltMeta_parent (_superOwl_meta sowl)) Set.empty
    nextVisited = if pass
      then Set.union visited toVisit
      else visited

  (_,_,r) = foldl foldlfn acc0 kiddosFirst

superOwlParliament_toSEltTree :: OwlTree -> SuperOwlParliament -> SEltTree
superOwlParliament_toSEltTree od@OwlTree {..} (SuperOwlParliament sowls) = toList $ join r where
  makeSElt :: REltId -> SuperOwl -> (REltId, Seq (REltId, SEltLabel))
  makeSElt maxid sowl = case _superOwl_elt sowl of
    OwlEltSElt oi selt -> (maxid, Seq.singleton $ (_superOwl_id sowl, SEltLabel (_owlInfo_name oi) selt))
    OwlEltFolder oi kiddos -> let
        kiddoS = (unSuperOwlParliament . owlParliament_toSuperOwlParliament od . OwlParliament $ kiddos)
        (newmaxid, childSElts) = mapAccumL makeSElt (maxid+1) kiddoS
      in
        (newmaxid, Seq.singleton (_superOwl_id sowl, SEltLabel (_owlInfo_name oi) SEltFolderStart)
        >< (join childSElts)
        >< Seq.singleton (maxid+1, SEltLabel (_owlInfo_name oi <> "(end)") SEltFolderEnd))
  (_, r) = mapAccumL makeSElt (owlTree_maxId od) sowls

data OwlTree = OwlTree {
  _owlTree_mapping :: OwlMapping
  , _owlTree_topOwls :: Seq REltId
} deriving (Show, Generic)

instance NFData OwlTree

instance MommyOwl OwlTree where
  mommyOwl_kiddos o = Just $ _owlTree_topOwls o

owlTree_prettyPrint :: OwlTree -> Text
owlTree_prettyPrint od@OwlTree {..} = r where
  foldlfn acc rid = let
      sowl = owlTree_mustFindSuperOwl rid od
      selfEntry' = T.replicate (_owlEltMeta_depth . _superOwl_meta $ sowl) " " <> superOwl_prettyPrintForDebugging sowl
      selfEntry = selfEntry' <> "\n"
    in acc <> case mommyOwl_kiddos sowl of
      Nothing -> selfEntry
      Just kiddos -> selfEntry <> printKiddos kiddos
  printKiddos :: Seq REltId -> Text
  printKiddos kiddos = foldl foldlfn "" kiddos
  r = printKiddos (fromJust $ mommyOwl_kiddos od)

owlTree_validate :: OwlTree -> Bool
owlTree_validate OwlTree {..} = r where
  -- TODO
  r = undefined

owlTree_maxId :: OwlTree -> REltId
owlTree_maxId s = maybe 0 fst (IM.lookupMax (_owlTree_mapping s))

-- reorganize the children of the given parent
-- i.e. update their relPosition in the directory
internal_owlTree_reorgKiddos :: OwlTree -> REltId -> OwlTree
internal_owlTree_reorgKiddos od prid = od { _owlTree_mapping = om } where
  childrenToUpdate = case prid of
    x | x == noOwl -> _owlTree_topOwls od
    _ -> case IM.lookup prid (_owlTree_mapping od) of
      Just (_, OwlEltFolder _ children) -> children
      Just _ -> Seq.empty
      Nothing -> error $ errorMsg_owlTree_lookupFail od prid
  setRelPos i (oem, oe) = (oem { _owlEltMeta_relPosition = i }, oe)
  om = Seq.foldlWithIndex (\om' i x -> IM.adjust (setRelPos i) x om') (_owlTree_mapping od) childrenToUpdate

emptyOwlTree :: OwlTree
emptyOwlTree = OwlTree {
    _owlTree_mapping = IM.empty
    , _owlTree_topOwls = Seq.empty
  }

owlTree_findSuperOwl :: REltId -> OwlTree -> Maybe SuperOwl
owlTree_findSuperOwl rid OwlTree {..} = do
  (meta, elt) <- IM.lookup rid _owlTree_mapping
  return $ SuperOwl rid meta elt

owlTree_mustFindSuperOwl :: HasCallStack => REltId -> OwlTree -> SuperOwl
owlTree_mustFindSuperOwl rid od = fromJust $ owlTree_findSuperOwl rid od

owlTree_topSuperOwls :: OwlTree -> Seq SuperOwl
owlTree_topSuperOwls od = r where
  sowls = fmap (flip owlTree_mustFindSuperOwl od) (_owlTree_topOwls od)
  areOwlsInFactSuper = all superOwl_isTopOwl sowls
  r = assert areOwlsInFactSuper sowls

owlTree_foldAt' :: (a -> SuperOwl -> a) -> a -> OwlTree -> SuperOwl -> a
owlTree_foldAt' f acc od sowl = case _superOwl_elt sowl of
  OwlEltFolder _ children -> foldl (\acc' rid' -> owlTree_foldAt' f acc' od (owlTree_mustFindSuperOwl rid' od)) (f acc sowl) children
  _ -> f acc sowl

owlTree_foldAt :: (a -> SuperOwl -> a) -> a -> OwlTree -> REltId -> a
owlTree_foldAt f acc od rid = owlTree_foldAt' f acc od (owlTree_mustFindSuperOwl rid od)

owlTree_fold :: (a -> SuperOwl -> a) -> a -> OwlTree -> a
owlTree_fold f acc0 od = foldl (\acc rid -> owlTree_foldAt f acc od rid) acc0 $ _owlTree_topOwls od

owlTree_owlCount :: OwlTree -> Int
owlTree_owlCount od = owlTree_fold (\acc _ -> acc+1) 0 od

-- | iterates an element and all its children
owliterateat :: OwlTree -> REltId -> Seq SuperOwl
owliterateat od rid = owlTree_foldAt (|>) Seq.empty od rid where

-- | iterates everything in the directory
owliterateall :: OwlTree -> Seq SuperOwl
owliterateall od = owlTree_fold (|>) Seq.empty od

-- | select everything in the OwlTree
owlTree_toSuperOwlParliament :: OwlTree -> SuperOwlParliament
owlTree_toSuperOwlParliament od@OwlTree {..} = r where
  r = owlParliament_toSuperOwlParliament od . OwlParliament $ _owlTree_topOwls

owlTree_removeREltId :: REltId -> OwlTree -> OwlTree
owlTree_removeREltId rid od = owlTree_removeSuperOwl (owlTree_mustFindSuperOwl rid od) od

owlTree_removeSuperOwl :: SuperOwl -> OwlTree -> OwlTree
owlTree_removeSuperOwl sowl@SuperOwl{..} od@OwlTree{..} = r where
  -- remove the element itself
  newMapping'' = IM.delete _superOwl_id _owlTree_mapping

  -- remove all children recursively
  removeEltWithoutAdjustMommyFn rid mapping = case IM.lookup rid mapping of
    Nothing -> error $ errorMsg_owlMapping_lookupFail mapping rid
    Just (_, OwlEltFolder _ children) -> foldr removeEltWithoutAdjustMommyFn (IM.delete rid mapping) children
    Just _ -> IM.delete rid mapping
  newMapping' = case _superOwl_elt of
    OwlEltFolder _ children -> foldr removeEltWithoutAdjustMommyFn newMapping'' children
    _ -> newMapping''

  removeChildFn parent = case parent of
    (oem, OwlEltFolder oi children) -> (oem, OwlEltFolder oi (removeSuperOwlFromSeq _owlTree_mapping children sowl))
    _ -> error "expected parent to be a folder"

  -- remove from children of the element's mommy if needed
  newMapping = case _owlEltMeta_parent _superOwl_meta of
    x | x == noOwl -> newMapping'
    rid -> IM.adjust removeChildFn rid newMapping'

  -- remove from top owls if needed
  newTopOwls = if superOwl_isTopOwl sowl
    then removeSuperOwlFromSeq _owlTree_mapping _owlTree_topOwls sowl
    else _owlTree_topOwls

  r = OwlTree {
      _owlTree_mapping = newMapping
      , _owlTree_topOwls = newTopOwls
    }

owlTree_moveOwlParliament :: OwlParliament -> OwlSpot -> OwlTree -> OwlTree
owlTree_moveOwlParliament op spot@OwlSpot{..} od@OwlTree{..} = assert isValid r where

  sop@(SuperOwlParliament sowls) = owlParliament_toSuperOwlParliament od op

  -- check that no owl is a parent of where we want to move to
  isValid = not $ all (isDescendentOf _owlTree_mapping _owlSpot_parent) (fmap _superOwl_id sowls)

  removedOd = foldl (\acc sowl -> owlTree_removeSuperOwl sowl acc) od sowls

  selttree = superOwlParliament_toSEltTree od sop

  r = owlTree_addSEltTree spot selttree removedOd

owlTree_addSEltTree :: OwlSpot -> SEltTree -> OwlTree -> OwlTree
owlTree_addSEltTree spot selttree od = r where
  -- we do it the potato way

  -- reindex the selttree
  startid = owlTree_maxId od + 1
  reindexed = fmap (\(rid,seltl) -> (rid + startid, seltl)) selttree

  -- convert to OwlDirectory
  otherod = owlTree_fromSEltTree reindexed

  -- now union the two directories
  newod = od { _owlTree_mapping = _owlTree_mapping od `IM.union` _owlTree_mapping otherod }

  makeOwl rid = _superOwl_elt $ owlTree_mustFindSuperOwl rid otherod

  -- TODO change to mapAccumL so you can return added elements
  -- and set the children accordingly (will correct metas from previous step)
  r = foldr (\rid acc -> fst $ internal_owlTree_addOwlElt True spot rid (makeOwl rid) acc) newod (_owlTree_topOwls otherod)

internal_owlTree_addOwlElt :: Bool -> OwlSpot -> REltId -> OwlElt -> OwlTree -> (OwlTree, SuperOwl)
internal_owlTree_addOwlElt allowFoldersAndExisting OwlSpot{..} rid oelt od@OwlTree{..} = assert (allowFoldersAndExisting || pass) r where

  -- if we're adding a folder, ensure it has no children
  pass = case oelt of
    OwlEltFolder _ children -> Seq.null children
    _ -> True

  meta = OwlEltMeta {
      _owlEltMeta_parent = _owlSpot_parent
      , _owlEltMeta_depth = case _owlSpot_parent of
        x | x == noOwl -> 0
        _ -> case IM.lookup _owlSpot_parent _owlTree_mapping of
          Nothing -> error $ errorMsg_owlMapping_lookupFail _owlTree_mapping _owlSpot_parent
          Just (x,_) -> _owlEltMeta_depth x + 1

      -- this will get set correctly when we call internal_owlTree_reorgKiddos later
      , _owlEltMeta_relPosition = undefined
    }

  newsowl = SuperOwl rid meta oelt

  newMapping' = if allowFoldersAndExisting
    then IM.insert rid (meta, oelt) _owlTree_mapping
    else IM.insertWithKey (\k _ ov -> error ("key " <> show k <> " already exists with value " <> show ov)) rid (meta, oelt) _owlTree_mapping

  modifyKiddos kiddos = Seq.insertAt position rid kiddos where
    position = case _owlSpot_leftSibling of
      Nothing -> 0
      Just rid -> case Seq.elemIndexL rid kiddos of
        Nothing -> error $ "expected to find leftmost sibling " <> show rid <> " in " <> show kiddos
        Just x -> x + 1

  adjustfn (oem,oe) = case oe of
    OwlEltFolder oi kiddos -> (oem, OwlEltFolder oi (modifyKiddos kiddos))
    _ -> error $ "expected OwlEltFolder"

  newMapping = case _owlSpot_parent of
    x | x == noOwl -> newMapping'
    _ -> IM.adjust adjustfn _owlSpot_parent newMapping'

  newTopOwls = case _owlSpot_parent of
    x | x == noOwl -> modifyKiddos _owlTree_topOwls
    _ -> _owlTree_topOwls

  r' = OwlTree {
      _owlTree_mapping = newMapping
      , _owlTree_topOwls = newTopOwls
    }

  r = (internal_owlTree_reorgKiddos r' _owlSpot_parent, newsowl)

-- TODO modify this to return SuperOwl that was added

owlTree_addOwlElt :: OwlSpot -> REltId -> OwlElt -> OwlTree -> (OwlTree, SuperOwl)
owlTree_addOwlElt = internal_owlTree_addOwlElt False

-- | use to convert old style layers to Owl
internal_addUntilFolderEndRecursive ::
  REltIdMap SEltLabel
  -> Seq REltId
  -> Int -- ^ current layer position we are adding
  -> REltId -- ^ parent
  -> Int -- ^ depth
  -> REltIdMap (OwlEltMeta, OwlElt) -- ^ accumulated directory
  -> Seq REltId -- ^ accumulated children at current level
  -> (Int, REltIdMap (OwlEltMeta, OwlElt), Seq REltId) -- ^ (next lp, accumulated directory, children of current level)
internal_addUntilFolderEndRecursive oldDir oldLayers lp parent depth accDir accSiblings = let
    recurfn = internal_addUntilFolderEndRecursive oldDir oldLayers
    -- the elt we want to add
    rid = Seq.index oldLayers lp
    SEltLabel name selt =  oldDir IM.! rid
    selfMeta = OwlEltMeta parent depth (Seq.length accSiblings)
    newSiblings = accSiblings |> rid
  in if lp >= Seq.length oldLayers
    -- this means we've reached the end of layers, nothing to do
    then (lp+1, accDir, accSiblings)
    -- normal case
    else case selt of
      SEltFolderStart -> r where
          (lp', accDir', accSiblings') = recurfn (lp+1) rid (depth+1) accDir Seq.empty
          selfOwl = OwlEltFolder (OwlInfo name) accSiblings'
          r = recurfn lp' parent depth (IM.insert rid (selfMeta, selfOwl) accDir') newSiblings
      -- we're done! throw out this elt
      SEltFolderEnd -> (lp+1, accDir, accSiblings)
      -- nothing special, keep going
      _ -> recurfn (lp+1) parent depth (IM.insert rid (selfMeta, OwlEltSElt (OwlInfo name) selt) accDir) newSiblings

owlTree_fromSEltTree :: SEltTree -> OwlTree
owlTree_fromSEltTree selttree = r where
  seltmap = IM.fromList selttree
  layers = fmap fst selttree
  r = owlTree_fromOldState seltmap (Seq.fromList layers)

owlTree_fromOldState :: REltIdMap SEltLabel -> Seq REltId -> OwlTree
owlTree_fromOldState oldDir oldLayers = r where
  (_, newDir, topOwls) = internal_addUntilFolderEndRecursive oldDir oldLayers 0 noOwl 0 IM.empty Seq.empty
  r = OwlTree {
      _owlTree_mapping = newDir
      , _owlTree_topOwls = topOwls
    }

owlTree_toSEltTree :: OwlTree -> SEltTree
owlTree_toSEltTree od@OwlTree{..} = superOwlParliament_toSEltTree od (owlTree_toSuperOwlParliament od)
