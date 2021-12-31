
module Potato.Flow.Attachments (
  attachLocationFromLBox
  , attachLocationsFromLBox
  , owlElt_availableAttachments
  , isOverAttachment
  , attachmentRenderChar

) where

import           Relude

import           Potato.Flow.Math
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

isOverAttachment :: XY -> [(Attachment, XY)] -> Maybe (Attachment, XY)
isOverAttachment pos attachments = find (\(a,x) -> x == pos) attachments


attachmentRenderChar :: Attachment -> PChar
attachmentRenderChar att = case _attachment_location att of
  AL_TOP -> '⇈'
  AL_BOT -> '⇊'
  AL_LEFT -> '⇇'
  AL_RIGHT -> '⇉'
