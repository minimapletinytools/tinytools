module Potato.Flow.OwlItem where



import Relude

import Potato.Flow.SElts
import Potato.Flow.Types


-- TODO move me somewhere
data SAutoLineCache = SAutoLineCache

data OwlSubItem =
  OwlSubItemFolder (Seq REltId)
  | OwlSubItemBox SBox
  | OwlSubItemLine SAutoLine (Maybe SAutoLineCache)
  | OwlSubItemTextArea STextArea

data OwlItem = OwlItem OwlInfo OwlSubItem



data OwlInfo = OwlInfo {
    _owlInfo_name :: Text
  } deriving (Show, Eq, Generic)

instance NFData OwlInfo


class MommyOwl o where
  mommyOwl_kiddos :: o -> Maybe (Seq REltId)
  mommyOwl_hasKiddos :: o -> Bool
  mommyOwl_hasKiddos = isJust . mommyOwl_kiddos

class HasOwlElt o where
  hasOwlElt_owlElt :: o -> OwlElt
  hasOwlElt_name :: o -> Text
  hasOwlElt_name = hasOwlElt_name . hasOwlElt_owlElt
  hasOwlElt_isFolder :: o -> Bool
  hasOwlElt_isFolder = hasOwlElt_isFolder . hasOwlElt_owlElt
  hasOwlElt_attachments :: o -> [Attachment]
  hasOwlElt_attachments = hasOwlElt_attachments . hasOwlElt_owlElt
  hasOwlElt_toSElt_hack :: o -> SElt
  hasOwlElt_toSElt_hack = hasOwlElt_toSElt_hack . hasOwlElt_owlElt
  hasOwlElt_toSEltLabel_hack :: o -> SEltLabel
  hasOwlElt_toSEltLabel_hack = hasOwlElt_toSEltLabel_hack . hasOwlElt_owlElt


-- TODO rename to OwlItem
-- TODO OwlItemSElt -> OwlItemElt OwlInfo OwlElt
-- TODO add OwlEltFolder settings (or make it part of owlinfo?)
data OwlElt = OwlEltFolder OwlInfo (Seq REltId) | OwlEltSElt OwlInfo SElt deriving (Show, Eq, Generic)

instance NFData OwlElt

owlElt_name :: OwlElt -> Text
owlElt_name (OwlEltFolder (OwlInfo name) _) = name
owlElt_name (OwlEltSElt (OwlInfo name) _) = name

instance MommyOwl OwlElt where
  mommyOwl_kiddos (OwlEltFolder _ kiddos) = Just kiddos
  mommyOwl_kiddos _ = Nothing

-- temp conversions
owlElt_toSElt_hack :: OwlElt -> SElt
owlElt_toSElt_hack = \case
  OwlEltSElt _ selt -> selt
  _ -> SEltFolderStart


instance HasOwlElt OwlElt where
  hasOwlElt_owlElt = id
  hasOwlElt_name (OwlEltFolder (OwlInfo name) _) = name
  hasOwlElt_name (OwlEltSElt (OwlInfo name) _) = name
  hasOwlElt_isFolder (OwlEltFolder _ _) = True
  hasOwlElt_isFolder _ = False
  hasOwlElt_attachments = \case
    OwlEltFolder _ _ -> []
    OwlEltSElt _ selt -> case selt of
      SEltLine sline -> catMaybes [_sAutoLine_attachStart sline, _sAutoLine_attachEnd sline]
      _ -> []
  hasOwlElt_toSElt_hack = \case
    OwlEltSElt _ selt -> selt
    _ -> SEltFolderStart
  hasOwlElt_toSEltLabel_hack o = case o of
    OwlEltSElt _ selt -> SEltLabel (hasOwlElt_name o) selt
    _ -> SEltLabel (hasOwlElt_name o) SEltFolderStart
