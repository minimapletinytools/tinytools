
module Potato.Flow.Attachments (
  attachLocationFromLBox
  , attachLocationsFromLBox
  , owlElt_availableAttachments
  , getAvailableAttachments
  , isOverAttachment
  , getAttachmentPosition
  , attachmentRenderChar

) where

import           Relude

import           Potato.Flow.Math
import           Potato.Flow.BroadPhase
import           Potato.Flow.OwlState
import           Potato.Flow.Owl
import Potato.Flow.SElts


attachLocationFromLBox :: Bool -> LBox -> AttachmentLocation -> XY
attachLocationFromLBox True (LBox (V2 x y) (V2 w h)) = \case
  AL_TOP -> V2 (x+w `div` 2) (y-1)
  AL_BOT -> V2 (x+w `div` 2) (y+h)
  AL_LEFT -> V2 (x-1) (y+h `div` 2)
  AL_RIGHT -> V2 (x+w) (y+h `div` 2)
attachLocationFromLBox False (LBox (V2 x y) (V2 w h)) = \case
  AL_TOP -> V2 (x+w `div` 2) y
  AL_BOT -> V2 (x+w `div` 2) (y+h-1)
  AL_LEFT -> V2 x (y+h `div` 2 )
  AL_RIGHT -> V2 (x+w-1) (y+h `div` 2 )

attachLocationsFromLBox :: Bool -> LBox -> [(AttachmentLocation, XY)]
attachLocationsFromLBox offsetBorder lbx = fmap (\a -> (a,attachLocationFromLBox offsetBorder lbx a)) [AL_TOP, AL_BOT, AL_LEFT, AL_RIGHT]

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
  -- TODO sort sowls
  fmapfn sowl = fmap (\(a,p) -> (Attachment (_superOwl_id sowl) a, p)) $ owlElt_availableAttachments offsetBorder (_superOwl_elt sowl)
  r = join $ fmap fmapfn sowls

isOverAttachment :: XY -> [(Attachment, XY)] -> Maybe (Attachment, XY)
isOverAttachment pos attachments = find (\(a,x) -> x == pos) attachments

getAttachmentPosition :: Bool -> OwlPFState -> Attachment -> XY
getAttachmentPosition offsetBorder pfs a = r where
  target = hasOwlTree_mustFindSuperOwl pfs (_attachment_target a)
  r = case hasOwlElt_owlElt target of
    OwlEltSElt _ selt -> case selt of
      SEltBox sbox -> attachLocationFromLBox offsetBorder (_sBox_box sbox) (_attachment_location a)
      _ -> error "expected SEltBox"
    _ -> error "expecteed OwlEltSelt"


attachmentRenderChar :: Attachment -> PChar
attachmentRenderChar att = case _attachment_location att of
  AL_TOP -> '⇈'
  AL_BOT -> '⇊'
  AL_LEFT -> '⇇'
  AL_RIGHT -> '⇉'
