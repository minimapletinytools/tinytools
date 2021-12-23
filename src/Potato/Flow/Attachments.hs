
module Potato.Flow.Attachments (
  attachLocationsFromLBox
  , owlElt_availableAttachments
  , getAvailableAttachments
) where

import           Relude

import           Potato.Flow.Math
import           Potato.Flow.Controller.Types
import           Potato.Flow.BroadPhase
import           Potato.Flow.OwlState
import           Potato.Flow.Owl
import Potato.Flow.SElts
import           Potato.Flow.Types

-- TODO put me elsewhere
attachLocationsFromLBox :: Bool -> LBox -> [(AttachmentLocation, XY)]
attachLocationsFromLBox offsetBorder (LBox (V2 x y) (V2 w h)) = if offsetBorder
  then
    [ (AL_TOP, V2 ((x+w) `div` 2) y)
    , (AL_BOT, V2 ((x+w) `div` 2) (y+h))
    , (AL_LEFT, V2 x ((y+h) `div` 2))
    , (AL_RIGHT, V2 (x+w) ((y+h) `div` 2))
    ]
  else
    [ (AL_TOP, V2 ((x+w) `div` 2) (y-1))
    , (AL_BOT, V2 ((x+w) `div` 2) (y+h+1))
    , (AL_LEFT, V2 (x-1) ((y+h) `div` 2))
    , (AL_RIGHT, V2 (x+w-1) ((y+h) `div` 2))
    ]

owlElt_availableAttachments :: Bool -> OwlElt -> [(AttachmentLocation, XY)]
owlElt_availableAttachments offsetBorder = \case
  OwlEltFolder _ _ -> []
  OwlEltSElt _ selt -> case selt of
    SEltBox sbox -> attachLocationsFromLBox offsetBorder (_sBox_box sbox)
    _ -> []

-- TODO TEST
getAvailableAttachments :: Bool -> OwlPFState -> BroadPhaseState -> LBox -> [(Attachment, XY)]
getAvailableAttachments offsetBorder pfs bps screenRegion = r where
  culled = broadPhase_cull screenRegion (_broadPhaseState_bPTree bps)
  -- you could silently fail here by ignoring maybes but that would definitely be an indication of a bug so we fail here instead (you could do a better job about dumping debug info though)
  sowls = fmap (hasOwlTree_mustFindSuperOwl pfs) culled
  fmapfn sowl = fmap (\(a,p) -> (Attachment (_superOwl_id sowl) a, p)) $ owlElt_availableAttachments offsetBorder (_superOwl_elt sowl)
  r = join $ fmap fmapfn sowls
