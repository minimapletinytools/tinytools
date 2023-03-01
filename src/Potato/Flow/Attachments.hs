{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Attachments (
  AvailableAttachment
  , BoxWithAttachmentLocation
  , attachLocationFromLBox_conjugateCartRotationReflection
  , attachLocationFromLBox
  , availableAttachLocationsFromLBox
  , owlItem_availableAttachments
  , owlItem_availableAttachmentsAtDefaultLocation
  , isOverAttachment
  , projectAttachment
  , attachmentRenderChar

) where

import           Relude

import           Potato.Flow.Math
import           Potato.Flow.OwlItem
import Potato.Flow.Owl
import Potato.Flow.SElts
import Potato.Flow.Methods.LineTypes

import Data.List (maximumBy)
import Data.Ratio
import Data.Tuple.Extra
import Control.Exception (assert)


data CartSegment = CartSegment {
    _cartSegment_isVertical :: Bool
    , _cartSegment_common :: Int
    , _cartSegment_leftOrTop :: Int
    , _cartSegment_rightOrBot :: Int
  } deriving (Eq, Show)
-- represents possible place to attach
data AvailableAttachment = AvailableAttachment_CartSegment CartSegment AttachmentLocation deriving (Show, Eq)

type BoxWithAttachmentLocation = (LBox, AttachmentLocation, AttachmentOffsetRatio)

-- uh not sure if this is actually conjugation...
attachLocationFromLBox_conjugateCartRotationReflection :: CartRotationReflection -> Bool -> BoxWithAttachmentLocation -> XY
attachLocationFromLBox_conjugateCartRotationReflection crr offsetBorder (box, al, af) = r where
  box' = cartRotationReflection_invert_apply crr box
  r' = attachLocationFromLBox offsetBorder (box', cartRotationReflection_invert_apply crr al, cartRotationReflection_invert_apply crr af)
  r = cartRotationReflection_apply crr r'

-- NOTE assumes LBox is canonical
attachLocationFromLBox :: Bool -> BoxWithAttachmentLocation -> XY
attachLocationFromLBox offset (LBox (V2 x y) (V2 w h), al, af)
  | offset = case al of
    AL_Top -> V2 (x + w * n `div` d) (y-1)
    -- TODO you need to get rid of the -1 here
    AL_Bot -> V2 (x+(w-1) * n `div` d) (y+h)
    -- TODO you need to get rid of the -1 here
    AL_Left -> V2 (x-1) (y+(h-1) * n `div` d)
    AL_Right -> V2 (x+w) (y+h * n `div` d)
    -- or maybe in the middle is better?
    AL_Any -> V2 x y
  | otherwise = case al of
    AL_Top -> V2 (x+w * n `div` d) y
    -- TODO you need to get rid of the -1 here
    AL_Bot -> V2 (x+(w-1) * n `div` d) (y+h-1)
    -- TODO you need to get rid of the -1 here
    AL_Left -> V2 x (y+(h-1) * n `div` d )
    AL_Right -> V2 (x+w-1) (y+h * n `div` d )
    -- or maybe in the middle is better?
    AL_Any -> V2 x y
  where
    n = numerator af
    d = denominator af


defaultAttachLocationsFromLBox :: Bool -> LBox -> [(AttachmentLocation, XY)]
defaultAttachLocationsFromLBox offsetBorder lbx = fmap (\a -> (a, attachLocationFromLBox offsetBorder (lbx, a, attachment_offset_rel_default))) [AL_Top, AL_Bot, AL_Left, AL_Right]

-- NOTE assumes LBox is canonical
availableAttachLocationFromLBox :: Bool -> (LBox, AttachmentLocation) -> AvailableAttachment
availableAttachLocationFromLBox offset (LBox (V2 x y) (V2 w h), al)
  | offset = flip AvailableAttachment_CartSegment al $ case al of
    AL_Top -> CartSegment False (y-1) x (x+w)
    AL_Bot -> CartSegment False (y+h) x (x+w)
    AL_Left -> CartSegment True (x-1) y (y+h)
    AL_Right -> CartSegment True (x+w) y (y+h)
    AL_Any -> assert False $ CartSegment False x y y
  | otherwise = flip AvailableAttachment_CartSegment al $ case al of
    AL_Top -> CartSegment False y x (x+w)
    AL_Bot -> CartSegment False (y+h-1) x (x+w)
    AL_Left -> CartSegment True x y (y+h)
    AL_Right -> CartSegment True (x+w-1) y (y+h)
    AL_Any -> assert False $ CartSegment False x y y

availableAttachLocationsFromLBox :: Bool -> LBox -> [AvailableAttachment]
availableAttachLocationsFromLBox offsetBorder lbx = fmap (\a -> (availableAttachLocationFromLBox offsetBorder (lbx, a))) [AL_Top, AL_Bot, AL_Left, AL_Right]

owlItem_availableAttachmentsAtDefaultLocation :: Bool -> Bool -> OwlItem -> [(AttachmentLocation, XY)]
owlItem_availableAttachmentsAtDefaultLocation includeNoBorder offsetBorder o = case _owlItem_subItem o of
  OwlSubItemBox sbox | not includeNoBorder && not (sBoxType_hasBorder (_sBox_boxType sbox)) -> []
  OwlSubItemBox sbox -> defaultAttachLocationsFromLBox offsetBorder (_sBox_box sbox)
  _ -> []

owlItem_availableAttachments :: Bool -> Bool -> OwlItem -> [AvailableAttachment]
owlItem_availableAttachments includeNoBorder offsetBorder o = case _owlItem_subItem o of
  OwlSubItemBox sbox | not includeNoBorder && not (sBoxType_hasBorder (_sBox_boxType sbox)) -> []
  OwlSubItemBox sbox -> availableAttachLocationsFromLBox offsetBorder (_sBox_box sbox)
  _ -> []

isOverAttachment :: XY -> [(Attachment, XY)] -> Maybe (Attachment, XY)
isOverAttachment pos attachments = find (\(a,x) -> x == pos) attachments

-- TODO
projectAttachment :: XY -> LBox -> Maybe (AttachmentLocation, XY)
projectAttachment pos lbox = r where
  als = availableAttachLocationsFromLBox False lbox

  -- TODO
  -- returns (projection distance, project position)
  projdfn :: AvailableAttachment -> (Int, XY, AvailableAttachment)
  projdfn = undefined

  rslts = fmap projdfn als
  (d, pos, AvailableAttachment_CartSegment _ al) = maximumBy (\a b -> compare (fst3 a) (fst3 b)) rslts

  r = if d < 2
    then Nothing
    else Just $ (al, pos)

attachmentRenderChar :: Attachment -> PChar
attachmentRenderChar att = case _attachment_location att of
  AL_Top -> '⇈'
  AL_Bot -> '⇊'
  AL_Left -> '⇇'
  AL_Right -> '⇉'
  AL_Any -> ' ' -- should never be rendered
