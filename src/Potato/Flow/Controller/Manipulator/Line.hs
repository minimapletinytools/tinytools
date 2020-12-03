{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Manipulator.Line (

) where

import           Relude

import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.Manipulator
import           Potato.Flow.Entry
import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.Types

import           Data.Dependent.Sum                 (DSum ((:=>)))
import qualified Data.IntMap                        as IM
