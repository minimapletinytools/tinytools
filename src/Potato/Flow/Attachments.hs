
module Potato.Flow.Attachments (
  BoxWithAttachmentLocation
  , attachLocationFromLBox_conjugateCartRotationReflection
  , attachLocationFromLBox
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
import Potato.Flow.Methods.LineTypes

import Data.Ratio

type BoxWithAttachmentLocation = (LBox, AttachmentLocation, AttachmentOffsetRatio)

-- uh not sure if this is actually conjugation...
attachLocationFromLBox_conjugateCartRotationReflection :: CartRotationReflection -> Bool -> BoxWithAttachmentLocation -> XY
attachLocationFromLBox_conjugateCartRotationReflection crr offsetBorder (box, al, af) = r where
  box' = cartRotationReflection_invert_apply crr box
  r' = attachLocationFromLBox offsetBorder (box', cartRotationReflection_invert_apply crr al, cartRotationReflection_invert_apply crr af)
  r = cartRotationReflection_apply crr r'

-- TODO use af param
attachLocationFromLBox :: Bool -> BoxWithAttachmentLocation -> XY
attachLocationFromLBox offset (LBox (V2 x y) (V2 w h), al, af)
  | offset = case al of
    AL_Top -> V2 (x + w * n `div` d) (y-1)
    AL_Bot -> V2 (x+(w-1) * n `div` d) (y+h)
    AL_Left -> V2 (x-1) (y+(h-1) * n `div` d)
    AL_Right -> V2 (x+w) (y+h * n `div` d)
    -- or maybe in the middle is better?
    AL_Any -> V2 x y
  | otherwise = case al of
    AL_Top -> V2 (x+w * n `div` d) y
    AL_Bot -> V2 (x+(w-1) * n `div` d) (y+h-1)
    AL_Left -> V2 x (y+(h-1) * n `div` d )
    AL_Right -> V2 (x+w-1) (y+h * n `div` d )
    -- or maybe in the middle is better?
    AL_Any -> V2 x y
  where
    n = numerator af
    d = denominator af

attachLocationsFromLBox :: Bool -> LBox -> [(AttachmentLocation, XY)]
attachLocationsFromLBox offsetBorder lbx = fmap (\a -> (a,attachLocationFromLBox offsetBorder (lbx, a, attachment_offset_rel_default))) [AL_Top, AL_Bot, AL_Left, AL_Right]

owlItem_availableAttachments :: Bool -> Bool -> OwlItem -> [(AttachmentLocation, XY)]
owlItem_availableAttachments includeNoBorder offsetBorder o = case _owlItem_subItem o of
  OwlSubItemBox sbox | not includeNoBorder && not (sBoxType_hasBorder (_sBox_boxType sbox)) -> []
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
