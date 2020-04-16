{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE RecursiveDo        #-}

module Potato.Flow.Reflex.Manipulators.Tags (
  MBox
  , MLine
  , MTag(..)
) where

import           Relude

import qualified Data.Dependent.Map as DM
import qualified Data.GADT.Compare

data MBox
data MLine

data MTag a where
  MTagBox :: MTag MBox
  MTagLine :: MTag MLine
  deriving anyclass Data.GADT.Compare.GEq
  deriving anyclass DM.GCompare
