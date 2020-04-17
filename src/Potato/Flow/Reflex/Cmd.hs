{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE RecursiveDo        #-}

module Potato.Flow.Reflex.Cmd (
  PFCmdTag(..)
  , PFCmd
  , PFCmdMap
  , PFCmdStack
  , selectDo
  , selectUndo
) where

import           Relude

import           Reflex
import           Reflex.Data.ActionStack
import           Reflex.Potato.Helpers

import           Potato.Flow.Reflex.Layers
import           Potato.Flow.Reflex.Manipulators
import           Potato.Flow.Reflex.RElts
import           Potato.Flow.SElts


import qualified Data.Dependent.Map              as DM
import qualified Data.Dependent.Sum              as DS
import qualified Data.GADT.Compare


data PFCmdTag t a where
  PFCNewElts :: PFCmdTag t (NonEmpty (REltLabel t)) -- TODO needs LayerPos
  PFCDeleteElt :: PFCmdTag t (LayerPos, REltLabel t)
  --PFCReorder :: PFCmdTag t (REltId, LayerPos)
  --PFCPaste :: PFCmdTag t ([SElt t], LayerPos)
  --PFCDuplicate :: PFCmdTag t [REltId]
  PFCManipulate :: PFCmdTag t (PFMControlCmd t)
  deriving anyclass Data.GADT.Compare.GEq
  deriving anyclass DM.GCompare

type PFCmd t = DS.DSum (PFCmdTag t) Identity
type PFCmdMap t = DM.DMap (PFCmdTag t) Identity

type PFCmdStack t = ActionStack t (PFCmd t)

selectDo :: forall t a. (Reflex t)
  => PFCmdStack t
  -> PFCmdTag t a
  -> Event t a
selectDo = actionStack_makeDoSelector

selectUndo :: forall t a. (Reflex t)
  => PFCmdStack t
  -> PFCmdTag t a
  -> Event t a
selectUndo = actionStack_makeUndoSelector
