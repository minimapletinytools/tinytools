{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE RecursiveDo        #-}
{-# LANGUAGE TemplateHaskell    #-}

module Potato.Flow.Reflex.Cmd (
  PFCmdTag(..)
  , PFCmd
  , PFCmdMap
  , PFCmdStack
  , selectDo
  , selectUndo

  --, TESTTag(..)
) where

import           Relude

import           Reflex
import           Reflex.Data.ActionStack

import           Potato.Flow.Math
import           Potato.Flow.Types

import qualified Data.Dependent.Map       as DM
import qualified Data.Dependent.Sum       as DS
import qualified Data.GADT.Compare
import           Data.GADT.Compare.TH
import qualified Text.Show


data PFCmdTag t a where
  -- LayerPos indices are as if all elements already exist in the map
  PFCNewElts :: PFCmdTag t (NonEmpty SuperSEltLabel)
  -- LayerPos indices are the current indices of elements to be removed
  PFCDeleteElts :: PFCmdTag t (NonEmpty SuperSEltLabel)
  --PFCMove :: PFCmdTag t (LayerPos, NonEmpty LayerPos)
  --PFCPaste :: PFCmdTag t (LayerPos, [REltId, SEltLabel])
  --PFCDuplicate :: PFCmdTag t [REltId]
  PFCManipulate :: PFCmdTag t (ControllersWithId)
  PFCResizeCanvas :: PFCmdTag t DeltaLBox

instance Text.Show.Show (PFCmdTag t a) where
  show PFCNewElts      = "PFCNewElts"
  show PFCDeleteElts   = "PFCDeleteElts"
  show PFCManipulate   = "PFCManipulate"
  show PFCResizeCanvas = "PFCResize"

instance Data.GADT.Compare.GEq (PFCmdTag t) where
  geq PFCNewElts PFCNewElts           = do return Data.GADT.Compare.Refl
  geq PFCDeleteElts PFCDeleteElts     = do return Data.GADT.Compare.Refl
  geq PFCManipulate PFCManipulate     = do return Data.GADT.Compare.Refl
  geq PFCResizeCanvas PFCResizeCanvas = do return Data.GADT.Compare.Refl
  -- TODO make sure this is correct
  geq _ _                             = Nothing

instance Data.GADT.Compare.GCompare (PFCmdTag t) where
  gcompare PFCNewElts PFCNewElts = runGComparing $ (do return Data.GADT.Compare.GEQ)
  gcompare PFCNewElts _ = Data.GADT.Compare.GLT
  gcompare _ PFCNewElts = Data.GADT.Compare.GGT
  gcompare PFCDeleteElts PFCDeleteElts = runGComparing $ (do return Data.GADT.Compare.GEQ)
  gcompare PFCDeleteElts _ = Data.GADT.Compare.GLT
  gcompare _ PFCDeleteElts = Data.GADT.Compare.GGT
  gcompare PFCManipulate PFCManipulate = runGComparing $ (do return Data.GADT.Compare.GEQ)
  gcompare PFCManipulate _ = Data.GADT.Compare.GLT
  gcompare _ PFCManipulate = Data.GADT.Compare.GGT
  gcompare PFCResizeCanvas PFCResizeCanvas = runGComparing $ (do return Data.GADT.Compare.GEQ)
  gcompare PFCResizeCanvas _ = Data.GADT.Compare.GLT
  gcompare _ PFCResizeCanvas = Data.GADT.Compare.GGT

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
