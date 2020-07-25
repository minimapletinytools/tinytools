{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell    #-}

module Potato.Flow.New.Cmd (
  PFCmdTag(..)
  , PFCmd

) where

import           Relude

import           Potato.Flow.Math
import           Potato.Flow.Reflex.Types

import           Data.Constraint.Extras.TH
import qualified Data.Dependent.Sum        as DS
import           Data.GADT.Compare.TH
import           Data.GADT.Show.TH
import qualified Text.Show

data PFCmdTag a where
  -- LayerPos indices are as if all elements already exist in the map
  PFCNewElts :: PFCmdTag [SuperSEltLabel]
  -- LayerPos indices are the current indices of elements to be removed
  PFCDeleteElts :: PFCmdTag [SuperSEltLabel]
  -- target index is before removal
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

deriveGEq      ''PFCmdTag
deriveGCompare ''PFCmdTag
deriveGShow ''PFCmdTag
deriveArgDict ''PFCmdTag
