-- variosu methods for creating Llamas

{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Methods.LlamaWorks where


import           Relude

import           Potato.Flow.Math
import           Potato.Flow.Owl
import Potato.Flow.OwlItem
import Potato.Flow.Attachments
import Potato.Flow.OwlWorkspace
import Potato.Flow.OwlState
import Potato.Flow.Llama
import           Potato.Flow.SElts
import           Potato.Flow.Types

import           Control.Exception (assert)

import qualified Data.Text         as T
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Sequence as Seq



makeAddFolderLlama :: OwlPFState -> (OwlSpot, Text) -> Llama
makeAddFolderLlama pfs (spot, name) = makePFCLlama $ OwlPFCNewElts [(owlPFState_nextId pfs, spot, OwlItem (OwlInfo name) (OwlSubItemFolder Seq.empty))]

pfc_removeElt_to_deleteElts :: OwlPFState -> OwlParliament -> OwlPFCmd
pfc_removeElt_to_deleteElts pfs owlp = assert valid r where
  od = _owlPFState_owlTree pfs
  valid = superOwlParliament_isValid od $ owlParliament_toSuperOwlParliament od owlp
  sop = owlParliament_toSuperOwlParliament od owlp
  sowlswithchildren = superOwlParliament_convertToSeqWithChildren od sop
  r = OwlPFCDeleteElts $ toList (fmap (\SuperOwl {..} -> (_superOwl_id, owlTree_owlItemMeta_toOwlSpot od _superOwl_meta, _superOwl_elt)) sowlswithchildren)

makeLlamaToSetAttachedLinesToCurrentPosition :: OwlPFState -> AttachmentMap -> REltId -> [Llama]
makeLlamaToSetAttachedLinesToCurrentPosition pfs am target = case IM.lookup target am of
    Nothing       -> []
    Just attached -> fmap makeLlama . IS.toList $ attached
  where
    makeLlama :: REltId -> Llama
    makeLlama rid = case _superOwl_elt (hasOwlTree_mustFindSuperOwl pfs rid) of
        OwlItem _ (OwlSubItemLine sline) -> r where
          startAttachment = _sAutoLine_attachStart sline
          endAttachment = _sAutoLine_attachEnd sline
          affectstart = fmap _attachment_target startAttachment == Just target
          affectend = fmap _attachment_target endAttachment == Just target
          newstartpos = case maybeLookupAttachment False pfs startAttachment of
            Nothing -> error $ "expected to find attachment " <> show startAttachment
            Just x -> x
          newendpos = case maybeLookupAttachment False pfs endAttachment of
            Nothing -> error $ "expected to find attachment " <> show endAttachment
            Just x -> x
          newsline = sline {
              -- disconnect from target if it was deleted
              -- NOTE strictly speaking necessary! Not sure which way is better in multi-user mode
              _sAutoLine_attachStart = if affectstart then Nothing else _sAutoLine_attachStart sline
              , _sAutoLine_attachEnd = if affectend  then Nothing else _sAutoLine_attachEnd sline

              -- place endpoints in new place
              , _sAutoLine_start = if affectstart then newstartpos else _sAutoLine_start sline
              , _sAutoLine_end = if affectend then newendpos else _sAutoLine_end sline

            }
          r = makeSetLlama (rid, SEltLine newsline)
        _ -> error $ "found non-line element in attachment list"

removeEltAndUpdateAttachments_to_llama :: OwlPFState -> AttachmentMap -> OwlParliament -> Llama
removeEltAndUpdateAttachments_to_llama pfs am op@(OwlParliament rids) = r where
  removellama = makePFCLlama $  pfc_removeElt_to_deleteElts pfs op
  resetattachllamas = join $ fmap (makeLlamaToSetAttachedLinesToCurrentPosition pfs am) (toList rids)
  -- seems more correct to detach lines first and then delete the target so that undo operation is more sensible
  r = makeCompositionLlama $ resetattachllamas <> [removellama]