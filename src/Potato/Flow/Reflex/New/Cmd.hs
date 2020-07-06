{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE RecursiveDo        #-}
{-# LANGUAGE TemplateHaskell    #-}

module Potato.Flow.Reflex.New.Cmd (
  PFCmdTag(..)
  , PFCmd

) where

import           Relude

import           Reflex
import           Reflex.Data.ActionStack

import           Potato.Flow.Math
import           Potato.Flow.Reflex.Types

import qualified Data.Dependent.Map       as DM
import qualified Data.Dependent.Sum       as DS
import qualified Text.Show

data PFCmdTag t a where
  -- LayerPos indices are as if all elements already exist in the map
  PFCNewElts :: PFCmdTag t (NonEmpty SuperSEltLabel)
  -- LayerPos indices are the current indices of elements to be removed
  PFCDeleteElts :: PFCmdTag t (NonEmpty SuperSEltLabel)
  --PFCMove :: PFCmdTag t (NonEmpty LayerPos, LayerPos)
  --PFCPaste :: PFCmdTag t (LayerPos, [REltId, SEltLabel])
  --PFCDuplicate :: PFCmdTag t [REltId]
  PFCManipulate :: PFCmdTag t (ControllersWithId)
  PFCResizeCanvas :: PFCmdTag t DeltaLBox

instance Text.Show.Show (PFCmdTag t a) where
  show PFCNewElts      = "PFCNewElts"
  show PFCDeleteElts   = "PFCDeleteElts"
  show PFCManipulate   = "PFCManipulate"
  show PFCResizeCanvas = "PFCResize"

type PFCmd t = DS.DSum (PFCmdTag t) Identity
