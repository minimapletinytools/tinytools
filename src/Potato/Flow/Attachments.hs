
module Potato.Flow.Attachments (
  attachLocationFromLBox
  , attachLocationsFromLBox
  , owlItem_availableAttachments
  , isOverAttachment
  , attachmentRenderChar

) where

import           Relude

import           Potato.Flow.Math
import           Potato.Flow.OwlItem
import Potato.Flow.Owl
import Potato.Flow.SElts


attachLocationFromLBox :: Bool -> LBox -> AttachmentLocation -> XY
attachLocationFromLBox True (LBox (V2 x y) (V2 w h)) = \case
  AL_Top -> V2 (x+w `div` 2) (y-1)
  AL_Bot -> V2 (x+w `div` 2) (y+h)
  AL_Left -> V2 (x-1) (y+h `div` 2)
  AL_Right -> V2 (x+w) (y+h `div` 2)
  -- or maybe in the middle is better?
  AL_Any -> V2 x y
attachLocationFromLBox False (LBox (V2 x y) (V2 w h)) = \case
  AL_Top -> V2 (x+w `div` 2) y
  AL_Bot -> V2 (x+w `div` 2) (y+h-1)
  AL_Left -> V2 x (y+h `div` 2 )
  AL_Right -> V2 (x+w-1) (y+h `div` 2 )
  -- or maybe in the middle is better?
  AL_Any -> V2 x y

attachLocationsFromLBox :: Bool -> LBox -> [(AttachmentLocation, XY)]
attachLocationsFromLBox offsetBorder lbx = fmap (\a -> (a,attachLocationFromLBox offsetBorder lbx a)) [AL_Top, AL_Bot, AL_Left, AL_Right]

owlItem_availableAttachments :: Bool -> OwlItem -> [(AttachmentLocation, XY)]
owlItem_availableAttachments offsetBorder o = case _owlItem_subItem o of
  OwlSubItemBox sbox -> attachLocationsFromLBox offsetBorder (_sBox_box sbox)
  _ -> []
  
isOverAttachment :: XY -> [(Attachment, XY)] -> Maybe (Attachment, XY)
isOverAttachment pos attachments = find (\(a,x) -> x == pos) attachments


attachmentRenderChar :: Attachment -> PChar
attachmentRenderChar att = case _attachment_location att of
  AL_Top -> '⇈'
  AL_Bot -> '⇊'
  AL_Left -> '⇇'
  AL_Right -> '⇉'
  AL_Any -> ' ' -- should never be rendered
