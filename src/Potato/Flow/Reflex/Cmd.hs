{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE DeriveAnyClass    #-}

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


import qualified Data.GADT.Compare
import qualified Data.Dependent.Map              as DM
import qualified Data.Dependent.Sum              as DS


data PFCmdTag t a where
  PFCNewElts :: PFCmdTag t (NonEmpty (REltId, RElt t)) -- TODO needs LayerPos
  PFCDeleteElt :: PFCmdTag t REltId
  PFCReorder :: PFCmdTag t (REltId, LayerPos)
  PFCPaste :: PFCmdTag t ([RElt t], LayerPos) -- change to SElt if you want to paste from other workspaces
  PFCDuplicate :: PFCmdTag t [REltId]
  PFCManipulate :: PFCmdTag t (ManipulatorCmd t)
  deriving anyclass Data.GADT.Compare.GEq
  deriving anyclass DM.GCompare

type PFCmd t = DS.DSum (PFCmdTag t) Identity
type PFCmdMap t = DM.DMap (PFCmdTag t) Identity

type PFCmdStack t = ActionStack t (PFCmd t)

selectDo :: forall t a. (Reflex t)
  => PFCmdStack t
  -> PFCmdTag t a
  -> Event t a
selectDo as tag = select (fanDSum $ _actionStack_do as) tag

selectUndo :: forall t a. (Reflex t)
  => PFCmdStack t
  -> PFCmdTag t a
  -> Event t a
selectUndo as tag = select (fanDSum $ _actionStack_undo as) tag
