{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Owl where

import           Relude

import           Potato.Flow.Types
import           Potato.Flow.SElts

import           Control.Exception (assert)
import qualified Data.IntMap       as IM
import           Data.Sequence     ((|>))
import qualified Data.Sequence     as Seq
import Data.Maybe (fromJust)


data OwlInfo = OwlInfo { _owlInfo_name :: Text } deriving (Show, Generic)

data OwlElt = OwlEltFolder OwlInfo (Seq REltId) | OwlEltSElt OwlInfo SElt deriving (Show, Generic)

-- TODO no owl prefix prob
type SemiPos = Int

data OwlEltMeta = OwlEltMeta {
  _owlEltMeta_parent :: REltId
  , _owlEltMeta_depth :: Int
  , _owlEltMeta_relPosition :: SemiPos
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

data OwlDirectory = OwlDirectory {
  _owlDirectory_directory :: REltIdMap (OwlEltMeta, OwlElt)
  , _owlDirectory_topOwls :: Seq REltId
} deriving (Show)

emptyDirectory :: OwlDirectory
emptyDirectory = OwlDirectory {
    _owlDirectory_directory = IM.empty
    , _owlDirectory_topOwls = Seq.empty
  }

owlDirectory_findSuperOwl :: REltId -> OwlDirectory -> Maybe SuperOwl
owlDirectory_findSuperOwl rid OwlDirectory {..} = do
  (meta, elt) <- IM.lookup rid _owlDirectory_directory
  return $ SuperOwl rid meta elt

owlDirectory_mustFindSuperOwl :: REltId -> OwlDirectory -> SuperOwl
owlDirectory_mustFindSuperOwl rid od = fromJust $ owlDirectory_findSuperOwl rid od
-- inlining... hope this will tell me where caller came from when fromJust fails
{-# INLINE owlDirectory_mustFindSuperOwl #-}

owlDirectory_topSuperOwls :: OwlDirectory -> Seq SuperOwl
owlDirectory_topSuperOwls od = r where
  sowls = fmap (flip owlDirectory_mustFindSuperOwl od) (_owlDirectory_topOwls od)
  areOwlsInFactSuper = all superOwl_isTopOwl sowls
  r = assert areOwlsInFactSuper sowls

-- TODO
-- | iterates an element and all its children
owliterateat :: OwlDirectory -> REltId -> Seq SuperOwl
owliterateat = undefined

-- | iterates everything inthe directory
owliterateall :: OwlDirectory -> Seq SuperOwl
owliterateall od = join $ fmap (owliterateat od) (_owlDirectory_topOwls od)

-- TODO need rel position to insert at I guess
owlDirectory_addSuperOwl :: OwlDirectory -> SuperOwl -> OwlDirectory
owlDirectory_addSuperOwl OwlDirectory{..} sowl@SuperOwl {..} = assert (superOwl_isTopOwl sowl) r where
  newDirectory = IM.insertWithKey (\k _ ov -> error ("key " <> show k <> " already exists with value " <> show ov)) _superOwl_id (_superOwl_meta, _superOwl_elt) _owlDirectory_directory
  --newDirectoryNoAssert = IM.insert _superOwl_id (_superOwl_meta, _superOwl_elt) _owlDirectory_directory
  r = OwlDirectory {
      _owlDirectory_directory = newDirectory
      -- TODO insert at provided position
      , _owlDirectory_topOwls = _owlDirectory_topOwls |> _superOwl_id
    }

-- TODO
{-
addUntilFolderEndRecursive ::
  REltIdMap SEltLabel
  -> Seq REltId
  -> LayerPos -- ^ current layer position we are adding
  -> [LayerEntry] -- ^ accumulator
  -> (LayerPos, [LayerEntry]) -- ^ (next lp, accumulator)
addUntilFolderEndRecursive directory layers lp added = let
    rid = Seq.index _pFState_layers lp
    seltl =  _pFState_directory IM.! rid
    sseltl = (rid, lp, seltl)
    selfEntry = eltfn sseltl parent
    combined = if skip then added else selfEntry:added
  in if lp >= Seq.length _pFState_layers
    -- this means we've reached the end of layers, nothing to do
    then (lp+1, added)
    -- normal case
    else case seltl of
      SEltLabel _ SEltFolderStart -> if not skip && skipfn (lookupWithDefault rid lmm)
          -- recurse through children (skipping) and the continue where it left off
          then uncurry (addUntilFolderEndRecursive pfs lmm skipfn eltfn skip parent) $ addUntilFolderEndRecursive pfs lmm skipfn eltfn True (Just selfEntry) (lp+1) combined
          -- recurse through children (possibly skipping), and then continue where it left off
          else uncurry (addUntilFolderEndRecursive pfs lmm skipfn eltfn skip parent) $ addUntilFolderEndRecursive pfs lmm skipfn eltfn skip (Just selfEntry) (lp+1) combined
      -- we're done!
      SEltLabel _ SEltFolderEnd -> (lp+1, added)
      -- nothing special, keep going
      _ ->addUntilFolderEndRecursive pfs lmm skipfn eltfn skip parent (lp+1) combined


owlDirectory_fromOldState :: REltIdMap SEltLabel -> Seq REltId -> OwlDirectory
owlDirectory_fromOldState directory layers = r where
  r = undefined
-}

--type SuperOwl = (REltId, OwlEltMeta, OwlElt)

--type OwlDirectory = REltIdMap (OwlEltMeta, OwlElt)

--emptyDirectory :: OwlDirectory
--emptyDirectory = IM.singleton (topOwlId, OwlEltFolder (OwlInfo "top owl") Seq.empty)


-- | get the top owl
-- every OwlDirectory is expected to have a topOwl, fails if no topOwl
--topOwl :: OwlDirectory ->
-- top owl
-- you can either have dummy REltId 0 node in tree OR you can carry around a list of all top level owls with the directory
-- prob way easier to do it the first way...


toSElt :: OwlElt -> SElt
toSElt = undefined

--getChildren :: OwlElt -> Seq
