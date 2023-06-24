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
import Potato.Flow.SElts
import Potato.Flow.Methods.LineTypes

import Data.List (minimumBy)
import Data.Ratio
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

-- TODO there is a bug in cartRotationReflection_apply/cartRotationReflection_invert_apply where we don't actually apply the rotation but somehow this only works with that bug... Maybe the rotations cancel out?
-- uh not sure if this is actually conjugation...
attachLocationFromLBox_conjugateCartRotationReflection :: CartRotationReflection -> Bool -> BoxWithAttachmentLocation -> XY
attachLocationFromLBox_conjugateCartRotationReflection crr offsetBorder (box, al, af) = r where
  r' = attachLocationFromLBox offsetBorder (cartRotationReflection_invert_apply crr box, cartRotationReflection_invert_apply crr al, cartRotationReflection_invert_apply crr af)
  r = cartRotationReflection_apply crr r'

-- NOTE assumes LBox is canonical
attachLocationFromLBox :: Bool -> BoxWithAttachmentLocation -> XY
attachLocationFromLBox True (lbx, al, af) = attachLocationFromLBox False (lBox_expand lbx (1,1,1,1), al, af)
attachLocationFromLBox False (LBox (V2 x y) (V2 w h), al, af) = case al of
  AL_Top -> V2 (x+w * n `div` d) y
  AL_Bot -> V2 (x+(w-1) * dn `div` d) (y+h-1)
  AL_Left -> V2 x (y+(h-1) * dn `div` d )
  AL_Right -> V2 (x+w-1) (y+h * n `div` d )
  -- or maybe in the middle is better?
  AL_Any -> V2 x y
  where
    n = numerator af
    d = denominator af
    dn = d-n


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
isOverAttachment pos attachments = find (\(_,x) -> x == pos) attachments


projectAttachment :: AttachmentLocation -> XY -> REltId -> LBox -> Maybe (Attachment, XY)
projectAttachment preval (V2 x y) rid lbox = r where
  als = availableAttachLocationsFromLBox False lbox

  -- returns (projection distance, (projection ratio, projection position)
  projdfn :: AvailableAttachment -> (Int, (AttachmentOffsetRatio, XY), AvailableAttachment)
  projdfn aa@(AvailableAttachment_CartSegment (CartSegment {..}) al) = r2 where
    projcomp = if _cartSegment_isVertical then x else y
    (orthd, orthcomp) = (abs (projcomp - _cartSegment_common), _cartSegment_common)
    slidecomp = if _cartSegment_isVertical then y else x
    (parad, paracomp) = if slidecomp < _cartSegment_leftOrTop
      then (_cartSegment_leftOrTop - slidecomp, _cartSegment_leftOrTop)
      else if slidecomp > _cartSegment_rightOrBot
        then (slidecomp - _cartSegment_rightOrBot, _cartSegment_rightOrBot)
        else (0, slidecomp)

    pos2@(V2 px py) = if _cartSegment_isVertical then V2 orthcomp paracomp else V2 paracomp orthcomp
    segl = _cartSegment_rightOrBot - _cartSegment_leftOrTop
    ratio2 = case al of
      AL_Top -> (px - _cartSegment_leftOrTop) % segl
      AL_Bot -> (_cartSegment_rightOrBot - px) % segl
      AL_Left -> (_cartSegment_rightOrBot - py) % segl
      AL_Right -> (py - _cartSegment_leftOrTop) % segl
      AL_Any -> error "unexpected"

    r2 = (parad+orthd, (ratio2, pos2), aa)

  rslts = fmap projdfn als
  cmpfn (d1, _, AvailableAttachment_CartSegment _ al1) (d2, _, AvailableAttachment_CartSegment _ al2) = compare d1 d2 <> compare (al2 == preval) (al1 == preval)
  (d, (ratio1, pos1), AvailableAttachment_CartSegment _ alfinal) = minimumBy cmpfn rslts

  attachment = Attachment {
      _attachment_target = rid
      , _attachment_location = alfinal
      , _attachment_offset_rel = ratio1
    }

  r = if d > 2
    then Nothing
    else Just $ (attachment, pos1)



attachmentRenderChar :: Attachment -> PChar
attachmentRenderChar att = case _attachment_location att of
  AL_Top -> '⇈'
  AL_Bot -> '⇊'
  AL_Left -> '⇇'
  AL_Right -> '⇉'
  AL_Any -> ' ' -- should never be rendered
