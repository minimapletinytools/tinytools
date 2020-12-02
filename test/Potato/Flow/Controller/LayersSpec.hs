{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.LayersSpec
  ( spec
  )
where

import           Relude                        hiding (empty, fromList)

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit      (fromHUnitTest)
import           Test.HUnit

import           Potato.Flow
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.Layers

import           Potato.Flow.TestStates

import           Data.Default
import qualified Data.IntMap                   as IM
import qualified Data.Sequence                 as Seq


someState1 :: PFState
someState1 = PFState {
      _pFState_layers = Seq.fromList [0..5]
      , _pFState_directory = IM.fromList [
          (0, folderStart)
            , (1, someSEltLabel)
            , (2, someSEltLabel)
            , (3, someSEltLabel)
            , (4, someSEltLabel)
            , (5, folderEnd)
        ]
      , _pFState_canvas = someSCanvas
  }

someState1_indents :: LayerIndents
someState1_indents = Seq.fromList [0,1,1,1,1,1]

someState2 :: PFState
someState2 = PFState {
      _pFState_layers = Seq.fromList [0..11]
      , _pFState_directory = IM.fromList [
          (0, folderStart)
            , (1, folderStart)
              , (2, someSEltLabel)
              , (3, folderStart)
                , (4, someSEltLabel)
                , (5, folderEnd)
              , (6, someSEltLabel)
              , (7, folderEnd)
            , (8, someSEltLabel)
            , (9, folderStart)
              , (10, folderEnd)
            , (11, folderEnd)
        ]
      , _pFState_canvas = someSCanvas
  }

someState2_indents :: LayerIndents
someState2_indents = Seq.fromList [0,1,2,2,3,3,2,2,1,1,2,1]




spec :: Spec
spec = do
  describe "Layers" $ do
    it "generateLayers" $ do
      generateLayers someState1 `shouldBe` someState1_indents
      generateLayers someState2 `shouldBe` someState2_indents
    describe "generateLayersNew" $ do
      it "basic" $ do
        -- empty LayerMetaMap means everything is collapsed by default
        Seq.length (generateLayersNew someState1 IM.empty) `shouldBe` 1
