{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell    #-}

module Potato.Flow.Cmd (
  PFCmdTag(..)
  , PFCmd

) where

import           Relude

import           Potato.Flow.Math
import           Potato.Flow.Types

import           Data.Constraint.Extras.TH
import qualified Data.Dependent.Sum        as DS
import           Data.GADT.Compare.TH
import           Data.GADT.Show.TH
import qualified Text.Show

data PFCmdTag a where
  -- LayerPos indices are as if all elements already exist in the map, must be in order
  PFCNewElts :: PFCmdTag [SuperSEltLabel]
  -- LayerPos indices are the current indices of elements to be removed, must be in order
  PFCDeleteElts :: PFCmdTag [SuperSEltLabel]
  -- all LayerPos indices are before move, must be in order
  PFCMove :: PFCmdTag ([LayerPos], LayerPos)
  --PFCDuplicate :: PFCmdTag [REltId]
  PFCManipulate :: PFCmdTag ControllersWithId
  PFCResizeCanvas :: PFCmdTag DeltaLBox

instance Text.Show.Show (PFCmdTag a) where
  show PFCNewElts      = "PFCNewElts"
  show PFCDeleteElts   = "PFCDeleteElts"
  show PFCManipulate   = "PFCManipulate"
  show PFCResizeCanvas = "PFCResize"

type PFCmd = DS.DSum PFCmdTag Identity

instance NFData PFCmd where
  rnf (PFCNewElts DS.:=> Identity a)      = rnf a
  rnf (PFCDeleteElts DS.:=> Identity a)   = rnf a
  rnf (PFCMove DS.:=> Identity a)         = rnf a
  rnf (PFCManipulate DS.:=> Identity a)   = rnf a
  rnf (PFCResizeCanvas DS.:=> Identity a) = rnf a
  rnf _                                   = ()

deriveGEq      ''PFCmdTag
deriveGCompare ''PFCmdTag
deriveGShow ''PFCmdTag
deriveArgDict ''PFCmdTag
