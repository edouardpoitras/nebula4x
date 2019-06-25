{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Nebula4x.Types.Component.Laser where

import           Control.Lens
import           Control.Newtype.Generics
import           Data.Aeson
import           Data.Aeson.TH
import           GHC.Generics                   ( Generic )

import           Nebula4x.Types.Component
import           Nebula4x.Types.Component.MissleLauncher

--
-- Laser
--
type LaserTargetType = MissleTargetType

newtype LaserRange = LaserRange Double deriving (Show, Eq, Generic)

instance Newtype LaserRange

instance ToJSON LaserRange

instance FromJSON LaserRange

newtype LaserStrength = LaserStrength Double deriving (Show, Eq, Generic)

instance Newtype LaserStrength

instance ToJSON LaserStrength

instance FromJSON LaserStrength

data Laser = Laser
  { _lName :: ComponentName
  , _lCost :: ComponentCost
  , _lSize :: ComponentSize
  , _lRange :: ComponentRating
  , _lRechargeRate :: ComponentRating
  , _lDamage :: ComponentRating
  } deriving (Show, Eq, Generic)

makeLenses ''Laser

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Laser

type Lasers = [Laser]

data ShipLasers = ShipLasers
  { _slRechargeRate :: ComponentRating
  , _slRechargeStatus :: ComponentRating
  , _slLasers :: Lasers
  } deriving (Show, Eq, Generic)

makeLenses ''ShipLasers

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''ShipLasers

data LaserDesign = LaserDesign
  { _ldId :: ComponentId
  , _ldResearchCost :: ComponentResearch
  , _ldResearchProgress :: ComponentResearch
  , _ldLaser :: Laser
  } deriving (Show, Eq, Generic)

makeLenses ''LaserDesign

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''LaserDesign

--
-- Laser Focal Size
--
data LaserFocalSize = LaserFocalSize
  { _lfsName :: ComponentName
  , _lfsSize :: ComponentSize
  , _lfsDamage :: ComponentRating
  } deriving (Show, Eq, Generic)

makeLenses ''LaserFocalSize

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''LaserFocalSize

data LaserFocalSizeResearch = LaserFocalSizeResearch
  { _lfsrId :: ComponentId
  , _lfsrResearchCost :: ComponentResearch
  , _lfsrResearchProgress :: ComponentResearch
  , _lfsrLaserFocalSize :: LaserFocalSize
  } deriving (Show, Eq, Generic)

makeLenses ''LaserFocalSizeResearch

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''LaserFocalSizeResearch

--
-- Laser Wavelength
--
data LaserWavelength = LaserWavelength
  { _lwName :: ComponentName
  , _lwRangeModifier :: ComponentRating
  } deriving (Show, Eq, Generic)

makeLenses ''LaserWavelength

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''LaserWavelength

data LaserWavelengthResearch = LaserWavelengthResearch
  { _lwrId :: ComponentId
  , _lwrResearchCost :: ComponentResearch
  , _lwrResearchProgress :: ComponentResearch
  , _lwrLaserWavelength :: LaserWavelength
  } deriving (Show, Eq, Generic)

makeLenses ''LaserWavelengthResearch

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''LaserWavelengthResearch

--
-- Laser Recharge Rate
--
data LaserRechargeRate = LaserRechargeRate
  { _lrrName :: ComponentName
  , _lrrRechargeRate :: ComponentRating
  } deriving (Show, Eq, Generic)

makeLenses ''LaserRechargeRate

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''LaserRechargeRate

data LaserRechargeRateResearch = LaserRechargeRateResearch
  { _lrrrId :: ComponentId
  , _lrrrResearchCost :: ComponentResearch
  , _lrrrResearchProgress :: ComponentResearch
  , _lrrrLaserRechargeRate :: LaserRechargeRate
  } deriving (Show, Eq, Generic)

makeLenses ''LaserRechargeRateResearch

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''LaserRechargeRateResearch

--
-- Laser Reduced Size
--
data LaserReducedSize = LaserReducedSize
  { _lrsName :: ComponentName
  , _lrsReducedSize :: ComponentRating
  , _lrsRechargeMultiplier :: ComponentRating
  } deriving (Show, Eq, Generic)

makeLenses ''LaserReducedSize

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''LaserReducedSize

data LaserReducedSizeResearch = LaserReducedSizeResearch
  { _lrsrId :: ComponentId
  , _lrsrResearchCost :: ComponentResearch
  , _lrsrResearchProgress :: ComponentResearch
  , _lrsrLaserReducedSize :: LaserReducedSize
  } deriving (Show, Eq, Generic)

makeLenses ''LaserReducedSizeResearch

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''LaserReducedSizeResearch
