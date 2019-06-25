{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Nebula4x.Types.Research where

import           Control.Lens
import           Data.Aeson.TH
import qualified Data.Map.Strict               as Map
import           GHC.Generics                   ( Generic )

import           Nebula4x.Types.Component
import           Nebula4x.Types.Component.Armor
import           Nebula4x.Types.Component.Engine
import           Nebula4x.Types.Component.FuelStorage
import           Nebula4x.Types.Component.Laser
import           Nebula4x.Types.Component.MissleLauncher
import           Nebula4x.Types.Component.Sensor
import           Nebula4x.Types.Component.Shield
import           Nebula4x.Types.Component.Transport
import           Nebula4x.Types.Installment
import           Nebula4x.Types.Race
import           Nebula4x.Types.Ship

type ResearchMap a = Map.Map ComponentId a

type BodyResearchLabs = Map.Map ComponentId Int

data ResearchStatus a = ResearchStatus
  { _unlocked     :: ResearchMap a
  , _pending      :: Maybe a
  , _researchLabs :: BodyResearchLabs
  , _locked       :: ResearchMap a
  } deriving (Show, Eq, Generic)

makeLenses ''ResearchStatus

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''ResearchStatus

data Research = Research
  { _rShipDesigns               :: ResearchStatus ShipDesign
  , _rEngineDesigns             :: ResearchStatus EngineDesign
  , _rEngineTechnology          :: ResearchStatus EngineTechnologyResearch
  , _rEngineModifier            :: ResearchStatus PowerEfficiencyModifierResearch
  , _rEngineFuelConsumption     :: ResearchStatus FuelConsumptionResearch
  , _rEngineSize                :: ResearchStatus EngineSizeResearch
  , _rMissleLauncherDesigns     :: ResearchStatus MissleLauncherDesign
  , _rMissleLauncherSize        :: ResearchStatus MissleLauncherSizeResearch
  , _rMissleLauncherReloadRate  :: ResearchStatus MissleLauncherReloadRateResearch
  , _rMissleLauncherReducedSize :: ResearchStatus MissleLauncherReducedSizeResearch
  , _rLaserDesigns              :: ResearchStatus LaserDesign
  , _rLaserFocalSize            :: ResearchStatus LaserFocalSizeResearch
  , _rLaserWavelength           :: ResearchStatus LaserWavelengthResearch
  , _rLaserRechargeRate         :: ResearchStatus LaserRechargeRateResearch
  , _rLaserReducedSize          :: ResearchStatus LaserReducedSizeResearch
  , _rArmor                     :: ResearchStatus ArmorResearch
  , _rShield                    :: ResearchStatus ShieldResearch
  , _rFuelStorage               :: ResearchStatus FuelStorageResearch
  , _rCargoHandling             :: ResearchStatus CargoHandlingResearch
  , _rCargoHold                 :: ResearchStatus CargoHoldResearch
  , _rJumpGate                  :: ResearchStatus JumpGateResearch
  , _rGeologicalSensor          :: ResearchStatus SensorResearch
  , _rGravitationalSensor       :: ResearchStatus SensorResearch
  , _rMines                     :: ResearchStatus InstallmentResearch
  , _rResearchLabs              :: ResearchStatus InstallmentResearch
  , _rFuelRefineries            :: ResearchStatus InstallmentResearch
  , _rConstructionFactories     :: ResearchStatus InstallmentResearch
  , _rMassDrivers               :: ResearchStatus InstallmentResearch
  , _rCommercialShipyards       :: ResearchStatus InstallmentResearch
  , _rNavalShipyards            :: ResearchStatus InstallmentResearch
  } deriving (Show, Eq, Generic)

makeLenses ''Research

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Research

type RaceResearch = Map.Map RaceId Research
