{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Serialization.Snake where

import           Relude

import           Potato.Flow.Types
import           Potato.Flow.SElts
import Potato.Flow.Controller.Types

import qualified Data.Aeson               as Aeson
import qualified Data.Aeson.Encode.Pretty as PrettyAeson
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as LBS
import           System.FilePath
import qualified Data.Text.Encoding as Text



-- | list of all version supported
versions :: [Int]
versions = [1]

-- | version of the current build
currentVersion :: Int
currentVersion = 1

data Snake = Snake {
  _snake_version :: Int
  -- string instead of enum type to ensure compatibility if new formats are ever added
  -- currently supports "binary" and "json"
  , _snake_format :: String
  , _snake_data :: Text
} deriving (Eq, Generic, Show)

instance Aeson.FromJSON Snake
instance Aeson.ToJSON Snake
instance Binary.Binary Snake
instance NFData Snake

data SnakeFormat = SF_Json | SF_Binary

serialize :: SnakeFormat -> (SPotatoFlow, ControllerMeta) -> LBS.ByteString
serialize f x = r where
  (inner, format) = case f of
    SF_Json -> (PrettyAeson.encodePretty x, "json")
    SF_Binary -> (Binary.encode x, "binary")
  outer = Snake {
    _snake_version = currentVersion
    , _snake_format = format
    , _snake_data = Text.decodeUtf8 (LBS.toStrict inner)
  }
  r = PrettyAeson.encodePretty outer

deserialize :: LBS.ByteString -> Either String (SPotatoFlow, ControllerMeta)
deserialize lbs = case Aeson.eitherDecode lbs of
  Left e -> Left $ "failed to decode Snake with error " <> e
  Right vso -> deserialize_internal vso


deserialize_internal :: Snake -> Either String (SPotatoFlow, ControllerMeta)
deserialize_internal Snake {..} = do
  if _snake_version /= currentVersion 
    then Left $ "version mismatch, got: " <> show _snake_version <> " expected: " <> show currentVersion
    else return ()
  case _snake_format of
    "json" -> Aeson.eitherDecodeStrict (Text.encodeUtf8 _snake_data)
    "binary" -> case Binary.decode (LBS.fromStrict $ Text.encodeUtf8 _snake_data) of
      Just x -> Right x
      Nothing -> Left "failed to decode binary"
    x -> Left $ "unrecognized fromat" <> show x


decodeFile :: FilePath -> IO (Either String (SPotatoFlow, ControllerMeta))
decodeFile fp = do
  vsobs <- LBS.readFile fp
  return $ deserialize vsobs


decodeFileMaybe :: FilePath -> IO (Maybe (SPotatoFlow, ControllerMeta))
decodeFileMaybe fp = do
  x <- decodeFile fp
  case x of
    Left _ -> return Nothing
    Right x -> return (Just x)
