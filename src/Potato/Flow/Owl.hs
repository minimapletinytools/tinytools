{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Owl where

import           Relude

import           Potato.Flow.Types
import           Potato.Flow.SElts

import           Control.Exception (assert)
import qualified Data.IntMap       as IM
import           Data.Sequence     ((|>), (><))
import qualified Data.Sequence     as Seq
import Data.Foldable (foldl)
import Data.Maybe (fromJust)
import qualified Data.Text as T


-- TODO Consider moving OwlInfo into meta?
data OwlInfo = OwlInfo { _owlInfo_name :: Text } deriving (Show, Generic)

--data OwlElt = OwlEltFolder OwlInfo (Seq OwlElt) | OwlEltSElt OwlInfo SElt deriving (Show, Generic)
data OwlElt = OwlEltFolder OwlInfo (Seq REltId) | OwlEltSElt OwlInfo SElt deriving (Show, Generic)

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
isChildOf :: OwlMapping -> REltId -> REltId -> Bool
isChildOf om child parent = r where
  parent' = case IM.lookup child om of
    Just (oem,_) -> _owlEltMeta_parent oem
    Nothing -> error $ "expected to find " <> show child
  r = case parent' of
    x | x == noOwl -> False
    x | x == parent -> True
    x -> isChildOf om x parent

data OwlEltMeta = OwlEltMeta {
  _owlEltMeta_parent :: REltId -- or should we do Maybe REltId?
  , _owlEltMeta_depth :: Int
  , _owlEltMeta_relPosition :: SemiPos
} deriving (Show, Generic)

-- a simpler version of OwlEltMeta used for inserting new Owls
data OwlSpot = OwlSpot {
  _owlSpot_parent :: REltId
  -- TODO is this what we want?
  , _owlSpot_leftSibling :: REltId
} deriving (Show, Generic)

data SuperOwl = SuperOwl {
  _superOwl_id :: REltId
  , _superOwl_meta :: OwlEltMeta
  , _superOwl_elt :: OwlElt
} deriving (Show, Generic)

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

-- if parent is selected, then kiddos must not be directly included in the parliament
newtype OwlParliament = OwlParliament { unOwlParliament :: Seq REltId }
newtype SuperOwlParliament = SuperOwlParliament { unSuperOwlParliament :: Seq SuperOwl }

-- TODO
owlSuperParliament_isValid :: SuperOwlParliament -> Bool
owlSuperParliament_isValid (SuperOwlParliament owls) = undefined

-- ???
--makeSuperOwlParliament :: [REltId] -> SuperOwlParliament

-- TODO rename to OwlTree
data OwlDirectory = OwlDirectory {
  -- TODO rename to mapping
  _owlDirectory_mapping :: OwlMapping
  , _owlDirectory_topOwls :: Seq REltId
} deriving (Show)


owlDirectory_maxId :: OwlDirectory -> REltId
owlDirectory_maxId s = maybe 0 fst (IM.lookupMax (_owlDirectory_mapping s))

-- reorganize the children of the given parent
-- i.e. update their relPosition in the directory
reorgChildren :: OwlDirectory -> REltId -> OwlDirectory
reorgChildren od prid = od { _owlDirectory_mapping = om } where
  childrenToUpdate = case prid of
    x | x == noOwl -> _owlDirectory_topOwls od
    _ -> case IM.lookup prid (_owlDirectory_mapping od) of
      Just (_, OwlEltFolder _ children) -> children
      Just _ -> Seq.empty
      Nothing -> error $ "expected to find parent with REltId " <> show prid
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

owlDirectory_mustFindSuperOwl :: REltId -> OwlDirectory -> SuperOwl
owlDirectory_mustFindSuperOwl rid od = fromJust $ owlDirectory_findSuperOwl rid od
-- inlining... hope this will tell me where caller came from when fromJust fails.. I don't think it works...
{-# INLINE owlDirectory_mustFindSuperOwl #-}

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

owlDirectory_fold :: (a -> SuperOwl -> a) -> a -> OwlDirectory -> a
owlDirectory_fold f acc0 od = foldl (\acc rid -> owlDirectory_foldAt f acc od rid) acc0 $ _owlDirectory_topOwls od

owlDirectory_owlCount :: OwlDirectory -> Int
owlDirectory_owlCount od = owlDirectory_fold (\acc _ -> acc+1) 0 od

owlDirectory_prettyPrint :: OwlDirectory -> [Text]
owlDirectory_prettyPrint od = reverse $ owlDirectory_fold f [] od where
  f acc (SuperOwl rid OwlEltMeta {..} oelt) = r:acc where
    -- TODO make helper for this
    name = case oelt of
      OwlEltFolder (OwlInfo name) _ -> name
      OwlEltSElt (OwlInfo name) _ -> name
    depth = _owlEltMeta_depth
    r = T.replicate depth "  " <> show rid <> " " <> name


-- | iterates an element and all its children
owliterateat :: OwlDirectory -> REltId -> Seq SuperOwl
owliterateat od rid = owlDirectory_foldAt (|>) Seq.empty od rid where

-- | iterates everything in the directory
owliterateall :: OwlDirectory -> Seq SuperOwl
owliterateall od = owlDirectory_fold (|>) Seq.empty od

-- TODO test
owlDirectory_removeSuperOwl :: SuperOwl -> OwlDirectory -> OwlDirectory
owlDirectory_removeSuperOwl sowl@SuperOwl{..} od@OwlDirectory{..} = r where
  relPosToRemove = _owlEltMeta_relPosition _superOwl_meta
  removeChildFn parent = case parent of
    (oem, OwlEltFolder oi children) -> (oem, OwlEltFolder oi (removeSuperOwlFromSeq _owlDirectory_mapping children sowl))
    _ -> error "expected parent to be a folder"

  -- TODO need to remove children from directory too
  newMapping' = IM.delete _superOwl_id _owlDirectory_mapping

  newMapping = case _superOwl_id of
    x | x == noOwl -> newMapping'
    rid -> IM.adjust removeChildFn rid newMapping'
  newTopOwls = if superOwl_isTopOwl sowl
    then removeSuperOwlFromSeq _owlDirectory_mapping _owlDirectory_topOwls sowl
    else _owlDirectory_topOwls
  r = OwlDirectory {
      _owlDirectory_mapping = newMapping
      , _owlDirectory_topOwls = newTopOwls
    }


-- TODO probably want to move an OwlParliament?
-- TODO
owlDirectory_moveSuperOwl :: SuperOwl -> OwlSpot -> OwlDirectory -> OwlDirectory
owlDirectory_moveSuperOwl sowl@SuperOwl{..} OwlSpot{..} od@OwlDirectory{..} = assert isValid r where
  rid = _superOwl_id
  isValid = not $ isChildOf _owlDirectory_mapping _owlSpot_parent rid
  -- TODO call remove then call add for now, optimize later...
  r = undefined

-- TODO figure out how you want to handle children...
-- maybe use serialized form and redo ids each time you add?
owlDirectory_addSuperOwl :: OwlSpot -> REltId -> OwlElt -> OwlDirectory -> OwlDirectory
owlDirectory_addSuperOwl OwlSpot{..} rid oelt OwlDirectory{..} = r where

  meta = undefined -- TODO
  newDirectory = IM.insertWithKey (\k _ ov -> error ("key " <> show k <> " already exists with value " <> show ov)) rid (meta, oelt) _owlDirectory_mapping
  --newDirectoryNoAssert = IM.insert rid (meta, oelt) _owlDirectory_mapping

  r = OwlDirectory {
      _owlDirectory_mapping = newDirectory
      -- TODO insert at provided position
      -- if sibling is Nothing then insert at 0 position otherwise find the sibling owl
      -- then figure out sibling owl index (binary search on rel pos) OR just use actual position you know..
      , _owlDirectory_topOwls = _owlDirectory_topOwls
    }

-- | use to convert old style layers to Owl
addUntilFolderEndRecursive ::
  REltIdMap SEltLabel
  -> Seq REltId
  -> LayerPos -- ^ current layer position we are adding
  -> REltId -- ^ parent
  -> Int -- ^ depth
  -> REltIdMap (OwlEltMeta, OwlElt) -- ^ accumulated directory
  -> Seq REltId -- ^ accumulated children at current level
  -> (LayerPos, REltIdMap (OwlEltMeta, OwlElt), Seq REltId) -- ^ (next lp, accumulated directory, children of current level)
addUntilFolderEndRecursive oldDir oldLayers lp parent depth accDir accSiblings = let
    recurfn = addUntilFolderEndRecursive oldDir oldLayers
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
  (_, newDir, topOwls) = addUntilFolderEndRecursive oldDir oldLayers 0 noOwl 0 IM.empty Seq.empty
  r = OwlDirectory {
      _owlDirectory_mapping = newDir
      , _owlDirectory_topOwls = topOwls
    }

-- TODO test
owlDirectory_toOldState :: OwlDirectory -> SEltTree
owlDirectory_toOldState od@OwlDirectory{..} = toList $ join r where
  makeSElt maxid rid = case IM.lookup rid _owlDirectory_mapping of
    Nothing -> error $ "expected to find owl with id " <> show rid
    Just (_, OwlEltSElt oi selt) -> (maxid, Seq.singleton $ (rid, SEltLabel (_owlInfo_name oi) selt))
    Just (_, OwlEltFolder oi children) -> let
        (newmaxid, childSElts) = mapAccumL makeSElt (maxid+1) children
      in
        (newmaxid, Seq.singleton (rid, SEltLabel (_owlInfo_name oi) SEltFolderStart)
        >< (join childSElts)
        >< Seq.singleton (maxid+1, SEltLabel (_owlInfo_name oi <> "(end)") SEltFolderEnd))
  (_, r) = mapAccumL makeSElt (owlDirectory_maxId od) _owlDirectory_topOwls
