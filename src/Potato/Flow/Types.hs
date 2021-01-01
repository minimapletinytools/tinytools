{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Potato.Flow.Types (
  LayerPos
  , REltId
  , REltIdMap
  , SuperSEltLabel
  , ControllersWithId
  , SEltLabelChanges
  , SEltLabelChangesWithLayerPos
  , LayerPosMap

  -- * controllers
  , CRename(..)
  , CLine(..)
  , CText(..)
  , CBoundingBox(..)
  , CTag(..)
  , CTextStyle(..)
  , CSuperStyle(..)
  , Controller

  , DeltaText
  , DeltaSuperStyle(..)
  , DeltaTextStyle(..)

  -- serialized types
  , SEltTree
  , SCanvas(..)
  , SPotatoFlow(..)
) where

import           Relude

import           Potato.Flow.Math
import           Potato.Flow.SElts

import           Control.Exception         (assert)
import           Data.Aeson
import           Data.Binary
import           Data.Constraint.Extras.TH
import           Data.Default
import qualified Data.Dependent.Sum        as DS
import           Data.GADT.Compare.TH
import           Data.GADT.Show.TH
import qualified Data.IntMap.Strict        as IM



type LayerPos = Int
type REltId = Int
type REltIdMap a = IM.IntMap a
type SuperSEltLabel = (REltId, LayerPos, SEltLabel)
type SEltLabelChanges = REltIdMap (Maybe SEltLabel)
type SEltLabelChangesWithLayerPos = REltIdMap (Maybe (LayerPos, SEltLabel))
type LayerPosMap = REltIdMap LayerPos

-- | (old text, new text)
type DeltaText = (Text,Text)
-- TODO more efficient to do this with zippers prob?
-- is there a way to make this more generic?
instance Delta Text DeltaText where
  plusDelta s (b, a) = assert (b == s) a
  minusDelta s (b, a) = assert (a == s) b

-- TODO
data DeltaSuperStyle = DeltaSuperStyle deriving (Eq, Generic, Show)
instance NFData DeltaSuperStyle

-- TODO
instance Delta SuperStyle DeltaSuperStyle where
  plusDelta ss _ = ss
  minusDelta ss _ = ss

-- TODO
data DeltaTextStyle = DeltaTextStyle deriving (Eq, Generic, Show)
instance NFData DeltaTextStyle

-- TODO
instance Delta TextStyle DeltaTextStyle where
  plusDelta ts _ = ts
  minusDelta ts _ = ts

data CRename = CRename {
  _cRename_deltaLabel :: DeltaText
} deriving (Eq, Generic, Show)

instance NFData CRename

instance Delta SEltLabel CRename where
  plusDelta (SEltLabel name selt) CRename {..} = SEltLabel (plusDelta name _cRename_deltaLabel) selt
  minusDelta (SEltLabel name selt) CRename {..} = SEltLabel (minusDelta name _cRename_deltaLabel) selt

data CLine = CLine {
  _cLine_deltaStart :: Maybe DeltaXY
  , _cLine_deltaEnd :: Maybe DeltaXY
} deriving (Eq, Generic, Show)

instance NFData CLine

instance Default CLine where
  def = CLine Nothing Nothing

instance Delta SLine CLine where
  plusDelta sline@SLine {..} CLine {..} = sline {
      _sLine_start   = case _cLine_deltaStart of
        Nothing -> _sLine_start
        Just d  -> plusDelta _sLine_start d
      , _sLine_end   =  case _cLine_deltaEnd of
        Nothing -> _sLine_end
        Just d  -> plusDelta _sLine_end d
    }
  minusDelta sline@SLine {..} CLine {..} = sline {
      _sLine_start   = case _cLine_deltaStart of
        Nothing -> _sLine_start
        Just d  -> minusDelta _sLine_start d
        , _sLine_end   =  case _cLine_deltaEnd of
          Nothing -> _sLine_end
          Just d  -> minusDelta _sLine_end d
    }

data CText = CText {
  _cText_deltaText      :: DeltaText
} deriving (Eq, Generic, Show)

instance NFData CText

instance Delta SBox CText where
  plusDelta sbox@SBox {..} ctext = sbox {
      _sBox_text   = plusDelta _sBox_text ctext
    }
  minusDelta sbox@SBox {..} ctext = sbox {
      _sBox_text   = minusDelta _sBox_text ctext
    }

instance Delta SBoxText CText where
  plusDelta sboxtext@SBoxText {..} CText {..} = sboxtext {
      _sBoxText_text   = plusDelta _sBoxText_text _cText_deltaText
    }
  minusDelta sboxtext@SBoxText {..} CText {..} = sboxtext {
      _sBoxText_text   = minusDelta _sBoxText_text _cText_deltaText
    }


-- | transforms object based on a reference point
-- used for multi-selection
data CBoundingBox = CBoundingBox {
  _cBoundingBox_deltaBox    :: DeltaLBox
} deriving (Eq, Generic, Show)

instance NFData CBoundingBox

data CSuperStyle = CSuperStyle DeltaSuperStyle deriving (Eq, Generic, Show)

instance NFData CSuperStyle

data CTextStyle = CTextStyle DeltaTextStyle deriving (Eq, Generic, Show)

instance NFData CTextStyle

-- TODO pretty sure you can delete this now
data CTag a where
  CTagRename :: CTag CRename
  CTagLine :: CTag CLine
  CTagText :: CTag CText
  CTagBoundingBox :: CTag CBoundingBox

  CTagSuperStyle :: CTag CSuperStyle
  CTagTextStyle :: CTag CTextStyle


deriveGEq      ''CTag
deriveGCompare ''CTag
deriveGShow ''CTag
deriveArgDict ''CTag

-- | Controllers represent changes to SELts
type Controller = DS.DSum CTag Identity

instance NFData Controller where
  rnf (CTagRename DS.:=> Identity a)      = rnf a
  rnf (CTagLine DS.:=> Identity a)        = rnf a
  rnf (CTagText DS.:=> Identity a)        = rnf a
  rnf (CTagBoundingBox DS.:=> Identity a) = rnf a
  rnf (CTagSuperStyle DS.:=> Identity a)  = rnf a
  rnf (CTagTextStyle DS.:=> Identity a)   = rnf a

-- | indexed my REltId
type ControllersWithId = IntMap Controller



type SEltTree = [(REltId,SEltLabel)]

data SCanvas = SCanvas {
  _sCanvas_box :: LBox
} deriving (Eq, Generic, Show)

instance FromJSON SCanvas
instance ToJSON SCanvas
instance Binary SCanvas
instance NFData SCanvas

-- TODO serialize PFState instead
data SPotatoFlow = SPotatoFlow {
  _sPotatoFlow_sCanvas    :: SCanvas
  , _sPotatoFlow_sEltTree :: SEltTree
} deriving (Eq, Generic, Show)

instance FromJSON SPotatoFlow
instance ToJSON SPotatoFlow
instance Binary SPotatoFlow
instance NFData SPotatoFlow
