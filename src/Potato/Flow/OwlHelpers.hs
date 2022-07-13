module Potato.Flow.OwlHelpers where


import Relude

import Potato.Flow.SElts
import Potato.Flow.Owl
import Potato.Flow.OwlItem
import Potato.Flow.Llama

superOwl_mustGetSLine :: SuperOwl -> SAutoLine
superOwl_mustGetSLine sowl = case _owlItem_subItem$ _superOwl_elt sowl of
  OwlSubItemLine sline _  -> sline
  x -> error $ "expected SAutoLine, got " <> show x <> " instead"

data SetLineStyleEnd = SetLineStyleEnd_Start | SetLineStyleEnd_End | SetLineStyleEnd_Both

setLineStyleEnd_setStart :: SetLineStyleEnd -> Bool
setLineStyleEnd_setStart SetLineStyleEnd_End = False
setLineStyleEnd_setStart _ = True

setLineStyleEnd_setEnd :: SetLineStyleEnd -> Bool
setLineStyleEnd_setEnd SetLineStyleEnd_Start = False
setLineStyleEnd_setEnd _ = True


-- TODO move into Llama/Helpers.hs or something
makeLlamaForLineStyle :: SuperOwl -> SetLineStyleEnd -> LineStyle -> Llama
makeLlamaForLineStyle sowl end newstyle = r where
  rid = _superOwl_id sowl
  sline = superOwl_mustGetSLine sowl
  newsline = sline {
      _sAutoLine_lineStyle = if setLineStyleEnd_setStart end then newstyle else _sAutoLine_lineStyle sline
      , _sAutoLine_lineStyleEnd = if setLineStyleEnd_setEnd end then _sAutoLine_lineStyleEnd sline else newstyle
    }
  r = makeSetLlama (rid, SEltLine sline)
