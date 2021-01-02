{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.SEltMethodsSpec(
  spec
) where

import           Relude       hiding (empty, fromList)

import           Test.Hspec

import           Data.Default (def)
import qualified Data.IntMap  as IM
import qualified Data.Text    as T

import           Potato.Flow


spec :: Spec
spec = do
  describe "SEltMethod" $ do
    describe "getDrawer" $ do
      describe "SBox" $ do
        it "SBoxType_NoBoxText" $ do
          let
            somesbox = def {
                _sBox_box       = LBox 0 (V2 5 5)
                , _sBox_text    = def {
                    _sBoxText_text = "m ow meow meow"
                  }
                , _sBox_boxType = SBoxType_NoBoxText
              }
            SEltDrawer {..} = getDrawer (SEltBox somesbox)
          _sEltDrawer_renderFn (V2 100 0) `shouldBe` Nothing
          _sEltDrawer_renderFn (V2 (-1) 0) `shouldBe` Nothing
          _sEltDrawer_renderFn (V2 0 0) `shouldBe` Just 'm'
          _sEltDrawer_renderFn (V2 1 0) `shouldBe` Just ' '
          _sEltDrawer_renderFn (V2 2 0) `shouldBe` Just 'o'
          -- TODO it seems EOL space characters are not rendered and this gives the fill char. Maybe this is desired behavior thought?
          --_sEltDrawer_renderFn (V2 3 0) `shouldBe` Just ' '
          _sEltDrawer_renderFn (V2 1 1) `shouldBe` Just 'e'
          _sEltDrawer_renderFn (V2 4 4) `shouldBe` Just '@'
