{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Preview where

import           Relude

import Potato.Flow.Llama


data Shepard = Shepard Int deriving (Eq, Show, Generic)

instance NFData Shepard

-- TODO use this to identify preview chains in the future
-- TODO also use to identify handlers
data Shift = Shift Int deriving (Eq, Show, Generic)

instance NFData Shift

dummyShepard :: Shepard
dummyShepard = Shepard 0

dummyShift :: Shift
dummyShift = Shift 0


-- TODO add PO_CommitAndStart
-- PO_StartAndCommit and PO_ContinueAndCommit are equivalent to doing a PO_Start or PO_Continue followed by a Preview_Commit, just for convenience
-- NOTE that PO_Start/PO_Continue will commit when another a preview comes in from the local user, the main reason you want to commit is to ensure the preview gets saved
data PreviewOperation = PO_Start | PO_Continue | PO_StartAndCommit | PO_ContinueAndCommit deriving (Eq, Show, Generic)

data Preview = 
  -- apply a preview operation
  Preview PreviewOperation Llama 
  -- commit the last operation
  | Preview_Commit
  -- cancel the preview 
  | Preview_Cancel 
  deriving (Show, Generic)

previewOperation_fromUndoFirst :: Bool -> PreviewOperation
previewOperation_fromUndoFirst undoFirst = case undoFirst of
  True -> PO_Continue
  False -> PO_Start

previewOperation_toUndoFirst :: PreviewOperation -> Bool
previewOperation_toUndoFirst po = case po of
  PO_Start -> False
  PO_Continue -> True
  PO_StartAndCommit -> False
  PO_ContinueAndCommit -> True