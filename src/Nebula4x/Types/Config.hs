{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Nebula4x.Types.Config where

import           Control.Lens
import           Data.Aeson.TH
import qualified Data.Map.Strict          as Map
import           GHC.Generics             (Generic)

import           Nebula4x.Types.Body
import           Nebula4x.Types.Component
import           Nebula4x.Types.Mineral

type MineralAmounts = Map.Map Element (Range Double)

data UniverseConfig = UniverseConfig
  { _ucRealStarSystems               :: Bool
  , _ucHomeworldMineralAccessibility :: Range Accessibility
  , _ucHomeworldMineralAmounts       :: MineralAmounts
  } deriving (Show, Eq, Generic)

makeLenses ''UniverseConfig

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''UniverseConfig

type PulseFrequency = Int

data PulseConfig = PulseConfig
  { _pcBasePulse         :: PulseFrequency
  , _pcBodyPosition      :: PulseFrequency
  , _pcShipyardWork      :: PulseFrequency
  , _pcResearchWork      :: PulseFrequency
  , _pcProductionWork    :: PulseFrequency
  , _pcMiningWork        :: PulseFrequency
  , _pcMassDriverWork    :: PulseFrequency
  , _pcMineralPacketWork :: PulseFrequency
  , _pcMissleWork        :: PulseFrequency
  , _pcShipOrder         :: PulseFrequency
  } deriving (Show, Eq, Generic)

makeLenses ''PulseConfig

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''PulseConfig

data StarConfig = StarConfig
  { _scStarMass   :: Range BodyMass
  , _scStarRadius :: Range BodyRadius
  } deriving (Show, Eq, Generic)

makeLenses ''StarConfig

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''StarConfig

data AsteroidConfig = AsteroidConfig
  { _acAsteroidsPerBelt       :: Range Int
  , _acGenerationChance       :: GenerationChance
  , _acAsteroidBelts          :: Range Int
  , _acBeltGenerationChance   :: GenerationChance
  , _acAsteroidOrbitDeviation :: Range OrbitDeviation
  } deriving (Show, Eq, Generic)

makeLenses ''AsteroidConfig

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''AsteroidConfig

data CometConfig = CometConfig
  { _ccComets           :: Range Int
  , _ccGenerationChance :: GenerationChance
  } deriving (Show, Eq, Generic)

makeLenses ''CometConfig

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''CometConfig

data StarSystemConfig = StarSystemConfig
  { _sscPlanetGenerationChance :: GenerationChance
  , _sscNPRGenerationChance    :: GenerationChance
  , _sscPlanets                :: Range Int
  } deriving (Show, Eq, Generic)

makeLenses ''StarSystemConfig

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''StarSystemConfig

data BodyConfig = BodyConfig
  { _bcType                        :: BodyType
  , _bcMass                        :: Range BodyMass
  , _bcRadius                      :: Range BodyRadius
  , _bcOrbitalDistance             :: OrbitalDistance
  , _bcMoonGenerationChance        :: GenerationChance
  , _bcMaxMoons                    :: Int
  , _bcMaxMoonMassRelativeToParent :: MassRatio
  , _bcMineralAmounts              :: MineralAmounts
  , _bcAccessibilityModifier       :: Range Double
  , _bcMineralGenerationChance     :: GenerationChance
  } deriving (Show, Eq, Generic)

makeLenses ''BodyConfig

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''BodyConfig

type BodyConfigs = Map.Map BodyType BodyConfig

data Config = Config
  { _universeConfig   :: UniverseConfig
  , _pulseConfig      :: PulseConfig
  , _asteroidConfig   :: AsteroidConfig
  , _cometConfig      :: CometConfig
  , _starConfig       :: StarConfig
  , _starSystemConfig :: StarSystemConfig
  , _bodyConfigs      :: BodyConfigs
  } deriving (Show, Eq, Generic)

makeLenses ''Config

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Config
