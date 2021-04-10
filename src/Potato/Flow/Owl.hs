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


errorMsg_owlDirectory_lookupFail :: OwlDirectory -> REltId -> Text
errorMsg_owlDirectory_lookupFail OwlDirectory {..} rid = errorMsg_owlMapping_lookupFail _owlDirectory_mapping rid

errorMsg_owlMapping_lookupFail :: OwlMapping -> REltId -> Text
errorMsg_owlMapping_lookupFail om rid = "expected to find REltId " <> show rid <> " in OwlMapping"

class MommyOwl o where
  mommyOwl_kiddos :: o -> Maybe (Seq REltId)

-- TODO Consider moving OwlInfo into meta?
data OwlInfo = OwlInfo { _owlInfo_name :: Text } deriving (Show, Generic)


-- TODO rename to just Owl
--data OwlElt = OwlEltFolder OwlInfo (Seq OwlElt) | OwlEltSElt OwlInfo SElt deriving (Show, Generic)
data OwlElt = OwlEltFolder OwlInfo (Seq REltId) | OwlEltSElt OwlInfo SElt deriving (Show, Generic)

instance MommyOwl OwlElt where
  mommyOwl_kiddos (OwlEltFolder  _ kiddos) = Just kiddos
  mommyOwl_kiddos _ = Nothing

type OwlMapping = REltIdMap (OwlEltMeta, OwlElt)

owlElt_name :: OwlElt -> Text
owlElt_name (OwlEltFolder (OwlInfo name) _) = name
owlElt_name (OwlEltSElt (OwlInfo name) _) = name

-- TODO decide if we want some relative position index or if we just want to use true index (and recompute on move/add/delete)
type SemiPos = Int

-- TODO test
-- TODO change this to do a binary search (once you have decided SemiPos is what you want and not actual position)
locateFromSemiPos :: (a -> SemiPos) -> Seq a -> SemiPos -> Int
locateFromSemiPos f s sp = Seq.length $ Seq.takeWhileL (\a -> f a < sp) s

-- TODO test
-- TODO make an owlDirectory method?
owlMappingSemiPosLookup :: OwlMapping -> REltId -> SemiPos
owlMappingSemiPosLookup om rid = case IM.lookup rid om of
  Nothing -> error $ "expected to find rid " <> show rid
  Just (oem,_) -> _owlEltMeta_relPosition oem

-- TODO test
locateOwlFromSemiPos :: OwlMapping -> Seq REltId -> SemiPos -> Int
locateOwlFromSemiPos om s sp = locateFromSemiPos (owlMappingSemiPosLookup om) s sp

-- TODO test
-- in this case, we remove only if there is an exact match
removeAtSemiPos :: (a -> SemiPos) -> Seq a -> SemiPos -> Seq a
removeAtSemiPos f s sp = r where
  (front, back) = Seq.breakl (\a -> f a == sp) s
  r = front >< Seq.drop 1 back

-- TODO test
-- TODO make an owlDirectory method?
removeSuperOwlFromSeq :: OwlMapping -> Seq REltId -> SuperOwl -> Seq REltId
removeSuperOwlFromSeq om s so = assert (Seq.length s == Seq.length r + 1) r where
  sp = _owlEltMeta_relPosition . _superOwl_meta $ so
  r = removeAtSemiPos (owlMappingSemiPosLookup om) s sp

-- TODO make an owlDirectoryMethod?
isDescendentOf :: OwlMapping -> REltId -> REltId -> Bool
isDescendentOf om child parent = r where
  parent' = case IM.lookup child om of
    Just (oem,_) -> _owlEltMeta_parent oem
    Nothing -> error $ errorMsg_owlMapping_lookupFail om child
  r = case parent' of
    x | x == noOwl -> False
    x | x == parent -> True
    x -> isDescendentOf om x parent

data OwlEltMeta = OwlEltMeta {
  _owlEltMeta_parent :: REltId -- or should we do Maybe REltId?
  , _owlEltMeta_depth :: Int
  , _owlEltMeta_relPosition :: SemiPos
} deriving (Show, Generic)

owlEltMeta_prettyPrintForDebugging :: OwlEltMeta -> Text
owlEltMeta_prettyPrintForDebugging OwlEltMeta {..} = "(meta: " <> show _owlEltMeta_parent <> " " <> show _owlEltMeta_depth <> " " <> show _owlEltMeta_relPosition <> ")"

-- a simpler version of OwlEltMeta used for inserting new Owls
data OwlSpot = OwlSpot {
  _owlSpot_parent :: REltId
  , _owlSpot_leftSibling :: Maybe REltId
} deriving (Show, Generic)

data SuperOwl = SuperOwl {
  _superOwl_id :: REltId
  , _superOwl_meta :: OwlEltMeta
  , _superOwl_elt :: OwlElt
} deriving (Show, Generic)


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
newtype OwlParliament = OwlParliament { unOwlParliament :: Seq REltId }

-- if parent is selected, then kiddos must not be directly included in the parliament
-- same as OwlParialment but contains more information
newtype SuperOwlParliament = SuperOwlParliament { unSuperOwlParliament :: Seq SuperOwl }

owlParliament_toSuperOwlParliament :: OwlDirectory -> OwlParliament -> SuperOwlParliament
owlParliament_toSuperOwlParliament od@OwlDirectory{..} op = SuperOwlParliament $ fmap f (unOwlParliament op) where
  f rid = case IM.lookup rid _owlDirectory_mapping of
    Nothing -> error $ errorMsg_owlDirectory_lookupFail od rid
    Just (oem,oe) -> SuperOwl rid oem oe

-- check if a mommy owl is selected, that no descendant of that mommy owl is selecetd
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

superOwlParliament_toSEltTree :: OwlDirectory -> SuperOwlParliament -> SEltTree
superOwlParliament_toSEltTree od@OwlDirectory {..} (SuperOwlParliament sowls) = toList $ join r where
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
  (_, r) = mapAccumL makeSElt (owlDirectory_maxId od) sowls

-- TODO
-- convert a selection into a SuperOwlParliament
-- TODO figure out selection conditions?
makeSuperOwlParliament :: OwlDirectory -> [REltId] -> SuperOwlParliament
makeSuperOwlParliament od@OwlDirectory{..} selection = undefined

-- TODO rename to OwlTree
data OwlDirectory = OwlDirectory {
  -- TODO rename to mapping
  _owlDirectory_mapping :: OwlMapping
  , _owlDirectory_topOwls :: Seq REltId
} deriving (Show)

instance MommyOwl OwlDirectory where
  mommyOwl_kiddos o = Just $ _owlDirectory_topOwls o

owlDirectory_prettyPrint :: OwlDirectory -> Text
owlDirectory_prettyPrint od@OwlDirectory {..} = r where
  foldlfn acc rid = let
      sowl = owlDirectory_mustFindSuperOwl rid od
      selfEntry' = T.replicate (_owlEltMeta_depth . _superOwl_meta $ sowl) " " <> superOwl_prettyPrintForDebugging sowl
      selfEntry = selfEntry' <> "\n"
    in acc <> case mommyOwl_kiddos sowl of
      Nothing -> selfEntry
      Just kiddos -> selfEntry <> printKiddos kiddos
  printKiddos :: Seq REltId -> Text
  printKiddos kiddos = foldl foldlfn "" kiddos
  r = printKiddos (fromJust $ mommyOwl_kiddos od)

owlDirectory_validate :: OwlDirectory -> Bool
owlDirectory_validate OwlDirectory {..} = r where
  -- TODO
  r = undefined

owlDirectory_maxId :: OwlDirectory -> REltId
owlDirectory_maxId s = maybe 0 fst (IM.lookupMax (_owlDirectory_mapping s))

-- reorganize the children of the given parent
-- i.e. update their relPosition in the directory
internal_owlDirectory_reorgKiddos :: OwlDirectory -> REltId -> OwlDirectory
internal_owlDirectory_reorgKiddos od prid = od { _owlDirectory_mapping = om } where
  childrenToUpdate = case prid of
    x | x == noOwl -> _owlDirectory_topOwls od
    _ -> case IM.lookup prid (_owlDirectory_mapping od) of
      Just (_, OwlEltFolder _ children) -> children
      Just _ -> Seq.empty
      Nothing -> error $ errorMsg_owlDirectory_lookupFail od prid
  setRelPos i (oem, oe) = (oem { _owlEltMeta_relPosition = i }, oe)
  om = Seq.foldlWithIndex (\om' i x -> IM.adjust (setRelPos i) x om') (_owlDirectory_mapping od) childrenToUpdate

emptyDirectory :: OwlDirectory
emptyDirectory = OwlDirectory {
    _owlDirectory_mapping = IM.empty
    , _owlDirectory_topOwls = Seq.empty
  }

owlDirectory_findSuperOwl :: REltId -> OwlDirectory -> Maybe SuperOwl
owlDirectory_findSuperOwl rid OwlDirectory {..} = do
  (meta, elt) <- IM.lookup rid _owlDirectory_mapping
  return $ SuperOwl rid meta elt

owlDirectory_mustFindSuperOwl :: HasCallStack => REltId -> OwlDirectory -> SuperOwl
owlDirectory_mustFindSuperOwl rid od = fromJust $ owlDirectory_findSuperOwl rid od

owlDirectory_topSuperOwls :: OwlDirectory -> Seq SuperOwl
owlDirectory_topSuperOwls od = r where
  sowls = fmap (flip owlDirectory_mustFindSuperOwl od) (_owlDirectory_topOwls od)
  areOwlsInFactSuper = all superOwl_isTopOwl sowls
  r = assert areOwlsInFactSuper sowls

owlDirectory_foldAt' :: (a -> SuperOwl -> a) -> a -> OwlDirectory -> SuperOwl -> a
owlDirectory_foldAt' f acc od sowl = case _superOwl_elt sowl of
  OwlEltFolder _ children -> foldl (\acc' rid' -> owlDirectory_foldAt' f acc' od (owlDirectory_mustFindSuperOwl rid' od)) (f acc sowl) children
  _ -> f acc sowl

owlDirectory_foldAt :: (a -> SuperOwl -> a) -> a -> OwlDirectory -> REltId -> a
owlDirectory_foldAt f acc od rid = owlDirectory_foldAt' f acc od (owlDirectory_mustFindSuperOwl rid od)

-- TODO instance Foldable OwlDirectory derp
owlDirectory_fold :: (a -> SuperOwl -> a) -> a -> OwlDirectory -> a
owlDirectory_fold f acc0 od = foldl (\acc rid -> owlDirectory_foldAt f acc od rid) acc0 $ _owlDirectory_topOwls od

-- TODO just Foldable.length
owlDirectory_owlCount :: OwlDirectory -> Int
owlDirectory_owlCount od = owlDirectory_fold (\acc _ -> acc+1) 0 od

-- | iterates an element and all its children
owliterateat :: OwlDirectory -> REltId -> Seq SuperOwl
owliterateat od rid = owlDirectory_foldAt (|>) Seq.empty od rid where

-- | iterates everything in the directory
owliterateall :: OwlDirectory -> Seq SuperOwl
owliterateall od = owlDirectory_fold (|>) Seq.empty od

-- | select everything in the OwlDirectory
owlDirectory_toSuperOwlParliament :: OwlDirectory -> SuperOwlParliament
owlDirectory_toSuperOwlParliament od@OwlDirectory {..} = r where
  r = owlParliament_toSuperOwlParliament od . OwlParliament $ _owlDirectory_topOwls

owlDirectory_removeSuperOwl :: SuperOwl -> OwlDirectory -> OwlDirectory
owlDirectory_removeSuperOwl sowl@SuperOwl{..} od@OwlDirectory{..} = r where
  -- remove the element itself
  newMapping'' = IM.delete _superOwl_id _owlDirectory_mapping

  -- remove all children recursively
  removeEltWithoutAdjustMommyFn rid mapping = case IM.lookup rid mapping of
    Nothing -> error $ errorMsg_owlMapping_lookupFail mapping rid
    Just (_, OwlEltFolder _ children) -> foldr removeEltWithoutAdjustMommyFn (IM.delete rid mapping) children
    Just _ -> IM.delete rid mapping
  newMapping' = case _superOwl_elt of
    OwlEltFolder _ children -> foldr removeEltWithoutAdjustMommyFn newMapping'' children
    _ -> newMapping''

  removeChildFn parent = case parent of
    (oem, OwlEltFolder oi children) -> (oem, OwlEltFolder oi (removeSuperOwlFromSeq _owlDirectory_mapping children sowl))
    _ -> error "expected parent to be a folder"

  -- remove from children of the element's mommy if needed
  newMapping = case _owlEltMeta_parent _superOwl_meta of
    x | x == noOwl -> newMapping' -- TODO this is wrong? what am I doing here?
    rid -> IM.adjust removeChildFn rid newMapping'

  -- remove from top owls if needed
  newTopOwls = if superOwl_isTopOwl sowl
    then removeSuperOwlFromSeq _owlDirectory_mapping _owlDirectory_topOwls sowl
    else _owlDirectory_topOwls

  r = OwlDirectory {
      _owlDirectory_mapping = newMapping
      , _owlDirectory_topOwls = newTopOwls
    }


-- TODO probably want to move an OwlParliament?
-- TODO
owlDirectory_moveOwlParliament :: OwlParliament -> OwlSpot -> OwlDirectory -> OwlDirectory
owlDirectory_moveOwlParliament op spot@OwlSpot{..} od@OwlDirectory{..} = assert isValid r where

  sop@(SuperOwlParliament sowls) = owlParliament_toSuperOwlParliament od op

  -- check that no owl is a parent of where we want to move to
  isValid = not $ all (isDescendentOf _owlDirectory_mapping _owlSpot_parent) (fmap _superOwl_id sowls)

  removedOd = foldl (\acc sowl -> owlDirectory_removeSuperOwl sowl acc) od sowls

  selttree = superOwlParliament_toSEltTree od sop

  r = owlDirectory_addSEltTree spot selttree removedOd

-- TODO
owlDirectory_addSEltTree :: OwlSpot -> SEltTree -> OwlDirectory -> OwlDirectory
owlDirectory_addSEltTree OwlSpot{..} selttree od@OwlDirectory{..} = r where

  r = undefined

owlDirectory_addOwlElt :: OwlSpot -> REltId -> OwlElt -> OwlDirectory -> OwlDirectory
owlDirectory_addOwlElt OwlSpot{..} rid oelt od@OwlDirectory{..} = assert pass r where

  -- if we're adding a folder, ensure it has no children
  pass = case oelt of
    OwlEltFolder _ children -> Seq.null children
    _ -> True

  meta = OwlEltMeta {
      _owlEltMeta_parent = _owlSpot_parent
      , _owlEltMeta_depth = case _owlSpot_parent of
        x | x == noOwl -> 0
        _ -> case IM.lookup _owlSpot_parent _owlDirectory_mapping of
          Nothing -> error $ errorMsg_owlMapping_lookupFail _owlDirectory_mapping _owlSpot_parent
          Just (x,_) -> _owlEltMeta_depth x + 1

      -- this will get set correctly when we call internal_owlDirectory_reorgKiddos later
      , _owlEltMeta_relPosition = undefined
    }

  newMapping' = IM.insertWithKey (\k _ ov -> error ("key " <> show k <> " already exists with value " <> show ov)) rid (meta, oelt) _owlDirectory_mapping
  --newDirectoryNoAssert = IM.insert rid (meta, oelt) _owlDirectory_mapping

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
    x | x == noOwl -> modifyKiddos _owlDirectory_topOwls
    _ -> _owlDirectory_topOwls

  r' = OwlDirectory {
      _owlDirectory_mapping = newMapping
      , _owlDirectory_topOwls = newTopOwls
    }

  r = internal_owlDirectory_reorgKiddos r' _owlSpot_parent

-- | use to convert old style layers to Owl
internal_addUntilFolderEndRecursive ::
  REltIdMap SEltLabel
  -> Seq REltId
  -> LayerPos -- ^ current layer position we are adding
  -> REltId -- ^ parent
  -> Int -- ^ depth
  -> REltIdMap (OwlEltMeta, OwlElt) -- ^ accumulated directory
  -> Seq REltId -- ^ accumulated children at current level
  -> (LayerPos, REltIdMap (OwlEltMeta, OwlElt), Seq REltId) -- ^ (next lp, accumulated directory, children of current level)
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


owlDirectory_fromOldState :: REltIdMap SEltLabel -> Seq REltId -> OwlDirectory
owlDirectory_fromOldState oldDir oldLayers = r where
  (_, newDir, topOwls) = internal_addUntilFolderEndRecursive oldDir oldLayers 0 noOwl 0 IM.empty Seq.empty
  r = OwlDirectory {
      _owlDirectory_mapping = newDir
      , _owlDirectory_topOwls = topOwls
    }

-- TODO convert to SuperOwlParliament and then call superOwlParliament_toSEltTree
owlDirectory_toSEltTree :: OwlDirectory -> SEltTree
owlDirectory_toSEltTree od@OwlDirectory{..} = superOwlParliament_toSEltTree od (owlDirectory_toSuperOwlParliament od )
