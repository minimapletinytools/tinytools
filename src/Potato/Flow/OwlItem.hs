module Potato.Flow.OwlItem where



import Relude

import Potato.Flow.SElts
import Potato.Flow.Types


-- TODO move me somewhere
data SAutoLineCache = SAutoLineCache

{-
data OwlSubItem =
  OwlSubItemFolder (Seq REltId)
  | OwlSubItemBox SBox
  | OwlSubItemLine SAutoLine (Maybe SAutoLineCache)
  | OwlSubItemTextArea STextArea

data OwlItem = OwlItem OwlInfo OwlSubItem
-}


data OwlInfo = OwlInfo {
    _owlInfo_name :: Text
  } deriving (Show, Eq, Generic)

instance NFData OwlInfo


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


-- TODO rename to OwlItem
-- TODO OwlItemSElt -> OwlItemElt OwlInfo OwlItem
-- TODO add OwlItemFolder settings (or make it part of owlinfo?)
data OwlItem = OwlItemFolder OwlInfo (Seq REltId) | OwlItemSElt OwlInfo SElt deriving (Show, Eq, Generic)

instance NFData OwlItem

owlItem_name :: OwlItem -> Text
owlItem_name (OwlItemFolder (OwlInfo name) _) = name
owlItem_name (OwlItemSElt (OwlInfo name) _) = name

instance MommyOwl OwlItem where
  mommyOwl_kiddos (OwlItemFolder _ kiddos) = Just kiddos
  mommyOwl_kiddos _ = Nothing

-- temp conversions
owlItem_toSElt_hack :: OwlItem -> SElt
owlItem_toSElt_hack = \case
  OwlItemSElt _ selt -> selt
  _ -> SEltFolderStart


instance HasOwlItem OwlItem where
  hasOwlItem_owlItem = id
  hasOwlItem_name (OwlItemFolder (OwlInfo name) _) = name
  hasOwlItem_name (OwlItemSElt (OwlInfo name) _) = name
  hasOwlItem_isFolder (OwlItemFolder _ _) = True
  hasOwlItem_isFolder _ = False
  hasOwlItem_attachments = \case
    OwlItemFolder _ _ -> []
    OwlItemSElt _ selt -> case selt of
      SEltLine sline -> catMaybes [_sAutoLine_attachStart sline, _sAutoLine_attachEnd sline]
      _ -> []
  hasOwlItem_toSElt_hack = \case
    OwlItemSElt _ selt -> selt
    _ -> SEltFolderStart
  hasOwlItem_toSEltLabel_hack o = case o of
    OwlItemSElt _ selt -> SEltLabel (hasOwlItem_name o) selt
    _ -> SEltLabel (hasOwlItem_name o) SEltFolderStart
