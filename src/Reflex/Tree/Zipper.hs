module Reflex.Tree.Zipper (

) where

import           Relude

import           Reflex
import           Reflex.Tree

-- TODO finish http://hackage.haskell.org/package/rosezipper-0.2/docs/src/Data-Tree-Zipper.html#TreePos

-- note the reflex timeline var is replaced with r so we can continue using the type var t from Data.Tree.Zipper source as was
data TreePos r t a  = Loc
  { _content :: t a        -- ^ The currently selected tree.

  -- TODO these need to be dynamically connected to the parent `Dynamic t [Tree t a]` that owns this
  -- not sure if TreePos even works here
  , _before  :: Forest r a
  , _after   :: Forest r a
  , _parents :: [(Forest r a, a, Forest r a)]
  }
