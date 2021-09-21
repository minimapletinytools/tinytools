{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Potato.Flow.Types (
  REltId
  , REltIdMap
  , ControllersWithId
  , controllerWithId_isParams

  -- DELETE
  , LayerPos
  , SuperSEltLabel
  , SEltLabelChanges
  , SEltLabelChangesWithLayerPos
  , LayerPosMap

  -- * controllers
  , CRename(..)
  , CLine(..)
  , CBoxText(..)
  , CBoxType(..)
  , CBoundingBox(..)
  , CTag(..)
  , CTextStyle(..)
  , CSuperStyle(..)
  , CTextAlign(..)
  , CMaybeText(..)
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

{-
-- TODO more efficient to do this with zippers prob?
-- is there a way to make this more generic?
instance Delta Text DeltaText where
  plusDelta s (b, a) = assert (b == s) a
  minusDelta s (b, a) = assert (a == s) b
-}

instance (Show a, Eq a) => Delta a (a,a) where
  plusDelta s (b, a) = if b /= s
    then assert (b == s) a
    else assert (b == s) a
  minusDelta s (b, a) = assert (a == s) b

data DeltaTextAlign = DeltaTextAlign (TextAlign, TextAlign) deriving (Eq, Generic, Show)
instance NFData DeltaTextAlign
instance Delta TextAlign DeltaTextAlign where
  plusDelta ta (DeltaTextAlign d) = plusDelta ta d
  minusDelta ta (DeltaTextAlign d) = minusDelta ta d

data DeltaSuperStyle = DeltaSuperStyle (SuperStyle, SuperStyle) deriving (Eq, Generic, Show)
instance NFData DeltaSuperStyle
instance Delta SuperStyle DeltaSuperStyle where
  plusDelta ss (DeltaSuperStyle d) = plusDelta ss d
  minusDelta ss (DeltaSuperStyle d) = minusDelta ss d

data DeltaTextStyle = DeltaTextStyle (TextStyle, TextStyle) deriving (Eq, Generic, Show)
instance NFData DeltaTextStyle
instance Delta TextStyle DeltaTextStyle where
  plusDelta ts (DeltaTextStyle d) = plusDelta ts d
  minusDelta ts (DeltaTextStyle d) = minusDelta ts d

data DeltaMaybeText = DeltaMaybeText (Maybe Text, Maybe Text)  deriving (Eq, Generic, Show)
instance NFData DeltaMaybeText
instance Delta (Maybe Text) DeltaMaybeText where
  plusDelta mt (DeltaMaybeText d) = plusDelta mt d
  minusDelta mt (DeltaMaybeText d) = minusDelta mt d

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

instance Delta SSimpleLine CLine where
  plusDelta sline@SSimpleLine {..} CLine {..} = sline {
      _sSimpleLine_start   = case _cLine_deltaStart of
        Nothing -> _sSimpleLine_start
        Just d  -> plusDelta _sSimpleLine_start d
      , _sSimpleLine_end   =  case _cLine_deltaEnd of
        Nothing -> _sSimpleLine_end
        Just d  -> plusDelta _sSimpleLine_end d
    }
  minusDelta sline@SSimpleLine {..} CLine {..} = sline {
      _sSimpleLine_start   = case _cLine_deltaStart of
        Nothing -> _sSimpleLine_start
        Just d  -> minusDelta _sSimpleLine_start d
        , _sSimpleLine_end   =  case _cLine_deltaEnd of
          Nothing -> _sSimpleLine_end
          Just d  -> minusDelta _sSimpleLine_end d
    }

data CBoxText = CBoxText {
  _cBoxText_deltaText      :: DeltaText
} deriving (Eq, Generic, Show)

instance NFData CBoxText

instance Delta SBox CBoxText where
  plusDelta sbox@SBox {..} ctext = sbox {
      _sBox_text   = plusDelta _sBox_text ctext
    }
  minusDelta sbox@SBox {..} ctext = sbox {
      _sBox_text   = minusDelta _sBox_text ctext
    }

instance Delta SBoxText CBoxText where
  plusDelta sboxtext@SBoxText {..} CBoxText {..} = sboxtext {
      _sBoxText_text   = plusDelta _sBoxText_text _cBoxText_deltaText
    }
  minusDelta sboxtext@SBoxText {..} CBoxText {..} = sboxtext {
      _sBoxText_text   = minusDelta _sBoxText_text _cBoxText_deltaText
    }

data CBoxType = CBoxType (SBoxType, SBoxType) deriving (Eq, Generic, Show)

instance NFData CBoxType

instance Delta SBox CBoxType where
  plusDelta sbox@SBox {..} (CBoxType deltatype) = sbox {
      _sBox_boxType   = plusDelta _sBox_boxType deltatype
    }
  minusDelta sbox@SBox {..} (CBoxType deltatype) = sbox {
      _sBox_boxType   = minusDelta _sBox_boxType deltatype
    }

data CBoundingBox = CBoundingBox {
  _cBoundingBox_deltaBox    :: DeltaLBox
} deriving (Eq, Generic, Show)
instance NFData CBoundingBox

data CSuperStyle = CSuperStyle DeltaSuperStyle deriving (Eq, Generic, Show)
instance NFData CSuperStyle

data CTextStyle = CTextStyle DeltaTextStyle deriving (Eq, Generic, Show)
instance NFData CTextStyle

data CTextAlign = CTextAlign DeltaTextAlign deriving (Eq, Generic, Show)
instance NFData CTextAlign

data CMaybeText = CMaybeText DeltaMaybeText deriving (Eq, Generic, Show)
instance NFData CMaybeText

-- NOTE, in some previous (very flawed) design, these were fanned out in a Reflex event hence the `DSum CTag` thing
-- we don't do this anymore, but DSum is still a nice alternative to using an ADT so we keep it.
data CTag a where
  CTagRename :: CTag CRename
  CTagLine :: CTag CLine
  CTagBoxText :: CTag CBoxText
  CTagBoxType :: CTag CBoxType
  CTagBoxTextStyle :: CTag CTextStyle

  CTagBoxLabelAlignment :: CTag CTextAlign
  CTagBoxLabelText :: CTag CMaybeText
  
  -- TODO CTagBoxLabel
  -- CTagBoxLabelAlignment

  CTagSuperStyle :: CTag CSuperStyle
  CTagBoundingBox :: CTag CBoundingBox
  
  

deriveGEq      ''CTag
deriveGCompare ''CTag
deriveGShow ''CTag
deriveArgDict ''CTag

-- | Controllers represent changes to SElts
type Controller = DS.DSum CTag Identity

instance NFData Controller where
  rnf (CTagRename DS.:=> Identity a)       = rnf a
  rnf (CTagLine DS.:=> Identity a)         = rnf a
  rnf (CTagBoxText DS.:=> Identity a)      = rnf a
  rnf (CTagBoxType DS.:=> Identity a)      = rnf a
  rnf (CTagBoundingBox DS.:=> Identity a)  = rnf a
  rnf (CTagSuperStyle DS.:=> Identity a)   = rnf a
  rnf (CTagBoxTextStyle DS.:=> Identity a) = rnf a
  rnf (CTagBoxLabelAlignment DS.:=> Identity a) = rnf a
  rnf (CTagBoxLabelText DS.:=> Identity a) = rnf a

-- | indexed my REltId
type ControllersWithId = IntMap Controller

controller_isParams :: Controller -> Bool
controller_isParams (CTagBoxType DS.:=> Identity a)      = True
controller_isParams (CTagSuperStyle DS.:=> Identity a)   = True
controller_isParams (CTagBoxTextStyle DS.:=> Identity a) = True
controller_isParams (CTagBoxLabelAlignment DS.:=> Identity a) = True
controller_isParams _                                    = False

controllerWithId_isParams :: ControllersWithId -> Bool
controllerWithId_isParams = all controller_isParams

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
