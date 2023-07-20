{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Preview where

import           Relude

import Potato.Flow.Llama

data PreviewOperation = Start | Continue | Commit deriving (Eq, Show, Generic)
data Preview = Preview PreviewOperation Llama | Cancel deriving (Show, Generic)
