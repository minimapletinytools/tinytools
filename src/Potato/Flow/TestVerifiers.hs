{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Potato.Flow.TestVerifiers (
  VerifyOutput
  , PFVerify
  , GoatVerify

) where

import           Relude

import           Data.Default
import qualified Data.IntMap   as IM
import qualified Data.Sequence as Seq
import           Potato.Flow


type VerifyOutput = Either Text Text -- left is failure, right is success, Text is optional message

type PFVerify = PFWorkspace -> VerifyOutput

type GoatVerify = GoatState -> VerifyOutput
