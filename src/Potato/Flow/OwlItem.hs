module Potato.Flow.OwlItem where



import Relude

import Potato.Flow.SElts
import Potato.Flow.Types
import Potato.Flow.Methods.LineTypes
import Potato.Flow.DebugHelpers

data OwlInfo = OwlInfo {
    _owlInfo_name :: Text
  } deriving (Show, Eq, Generic)

instance NFData OwlInfo

data OwlSubItem =
  OwlSubItemNone
  | OwlSubItemFolder (Seq REltId)
  | OwlSubItemBox SBox
  -- TODO maybe cache should also include line labels?
  | OwlSubItemLine SAutoLine (Maybe LineAnchorsForRender)
  | OwlSubItemTextArea STextArea
  deriving (Generic, Show, Eq)

instance NFData OwlSubItem

owlSubItem_equivalent :: OwlSubItem -> OwlSubItem -> Bool
owlSubItem_equivalent (OwlSubItemLine slinea _) (OwlSubItemLine slineb _) = slinea == slineb
owlSubItem_equivalent a b = a == b

owlSubItem_clearCache :: OwlSubItem -> OwlSubItem
owlSubItem_clearCache = \case
  OwlSubItemLine x _ -> OwlSubItemLine x Nothing
  x -> x

data OwlItem = OwlItem {
  _owlItem_info :: OwlInfo
  , _owlItem_subItem :: OwlSubItem
} deriving (Show, Eq, Generic)

instance NFData OwlItem

instance PotatoShow OwlItem where
  potatoShow = \case
    OwlItem oinfo (OwlSubItemFolder kiddos) -> "folder: " <> (_owlInfo_name oinfo) <> ": " <> show kiddos
    OwlItem oinfo subitem -> "elt: " <> (_owlInfo_name oinfo) <> ": " <> case subitem of
        OwlSubItemNone -> "none"
        OwlSubItemBox sbox -> show sbox
        OwlSubItemLine sline cache -> show sline <> " " <> show cache
        OwlSubItemTextArea stextarea -> show stextarea

owlItem_clearCache :: OwlItem -> OwlItem
owlItem_clearCache (OwlItem oinfo osubitem) = OwlItem oinfo (owlSubItem_clearCache osubitem)


class MommyOwl o where
  mommyOwl_kiddos :: o -> Maybe (Seq REltId)
  mommyOwl_hasKiddos :: o -> Bool
  mommyOwl_hasKiddos = isJust . mommyOwl_kiddos

class HasOwlItem o where
  hasOwlItem_owlItem :: o -> OwlItem
  hasOwlItem_name :: o -> Text
  hasOwlItem_name = hasOwlItem_name . hasOwlItem_owlItem
  hasOwlItem_isFolder :: o -> Bool
  hasOwlItem_isFolder = hasOwlItem_isFolder . hasOwlItem_owlItem
  hasOwlItem_attachments :: o -> [Attachment]
  hasOwlItem_attachments = hasOwlItem_attachments . hasOwlItem_owlItem
  hasOwlItem_toSElt_hack :: o -> SElt
  hasOwlItem_toSElt_hack = hasOwlItem_toSElt_hack . hasOwlItem_owlItem
  hasOwlItem_toSEltLabel_hack :: o -> SEltLabel
  hasOwlItem_toSEltLabel_hack = hasOwlItem_toSEltLabel_hack . hasOwlItem_owlItem
  hasOwlItem_toOwlSubItem :: o -> OwlSubItem
  hasOwlItem_toOwlSubItem = _owlItem_subItem . hasOwlItem_owlItem


owlItem_name :: OwlItem -> Text
owlItem_name = _owlInfo_name . _owlItem_info

owlItem_setName :: OwlItem -> Text -> OwlItem
owlItem_setName (OwlItem oi x) n = OwlItem (oi { _owlInfo_name = n}) x

instance MommyOwl OwlItem where
  mommyOwl_kiddos o = case _owlItem_subItem o of
    OwlSubItemFolder kiddos -> Just kiddos
    _ -> Nothing



owlSubItem_to_sElt_hack :: OwlSubItem -> SElt
owlSubItem_to_sElt_hack = \case
  OwlSubItemFolder _ -> SEltFolderStart
  OwlSubItemBox sbox -> SEltBox sbox
  OwlSubItemLine sline _ -> SEltLine sline
  OwlSubItemTextArea stextarea -> SEltTextArea stextarea
  OwlSubItemNone -> SEltNone

instance HasOwlItem OwlItem where
  hasOwlItem_owlItem = id
  hasOwlItem_name = owlItem_name
  hasOwlItem_isFolder o = case _owlItem_subItem o of
    OwlSubItemFolder _ -> True
    _ -> False
  hasOwlItem_attachments o = case _owlItem_subItem o of
    OwlSubItemLine sline _ -> catMaybes [_sAutoLine_attachStart sline, _sAutoLine_attachEnd sline]
    _ -> []
  hasOwlItem_toSElt_hack = owlSubItem_to_sElt_hack . _owlItem_subItem
  hasOwlItem_toSEltLabel_hack o = SEltLabel (hasOwlItem_name o) (hasOwlItem_toSElt_hack o)

-- DELETE use hasOwlItem variant instead
owlItem_toSElt_hack :: OwlItem -> SElt
owlItem_toSElt_hack = hasOwlItem_toSElt_hack

sElt_to_owlSubItem :: SElt -> OwlSubItem
sElt_to_owlSubItem s = case s of
  SEltBox x -> OwlSubItemBox x
  SEltLine x -> OwlSubItemLine x Nothing
  SEltTextArea x -> OwlSubItemTextArea x
  SEltNone -> OwlSubItemNone
  _ -> error $ "cannot convert " <> show s
