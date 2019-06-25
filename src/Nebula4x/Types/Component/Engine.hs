{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Nebula4x.Types.Component.Engine where

import           Control.Lens
import           Data.Aeson               (FromJSON, ToJSON)
import           Data.Aeson.TH
import           GHC.Generics             (Generic)

import           Nebula4x.Types.Component

data EngineType
  = CommercialEngine
  | MilitaryEngine
  deriving (Show, Eq, Generic)

instance ToJSON EngineType

instance FromJSON EngineType

--
-- Engine
--
data Engine = Engine
  { _eName       :: ComponentName
  , _eType       :: EngineType
  , _eCost       :: ComponentCost
  , _eSize       :: ComponentSize
  , _eRating     :: ComponentRating
  , _eEfficiency :: ComponentEfficiency
  } deriving (Show, Eq, Generic)

makeLenses ''Engine

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Engine

data EngineDesign = EngineDesign
  { _edId               :: ComponentId
  , _edResearchCost     :: ComponentResearch
  , _edResearchProgress :: ComponentResearch
  , _edEngine           :: Engine
  } deriving (Show, Eq, Generic)

makeLenses ''EngineDesign

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''EngineDesign

--
-- Engine Technologies
--
data EngineTechnology = EngineTechnology
  { _etName  :: ComponentName
  , _etPower :: ComponentRating
  } deriving (Show, Eq, Generic)

makeLenses ''EngineTechnology

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''EngineTechnology

data EngineTechnologyResearch = EngineTechnologyResearch
  { _etrId               :: ComponentId
  , _etrResearchCost     :: ComponentResearch
  , _etrResearchProgress :: ComponentResearch
  , _etrEngineTechnology :: EngineTechnology
  } deriving (Show, Eq, Generic)

makeLenses ''EngineTechnologyResearch

deriveJSON
  defaultOptions {fieldLabelModifier = drop 1}
  ''EngineTechnologyResearch

--
-- Power and Efficiency Modifiers
--
data PowerEfficiencyModifier = PowerEfficiencyModifier
  { _pemName               :: ComponentName
  , _pemPowerModifier      :: ComponentModifier
  , _pemEfficiencyModifier :: ComponentModifier
  } deriving (Show, Eq, Generic)

makeLenses ''PowerEfficiencyModifier

deriveJSON
  defaultOptions {fieldLabelModifier = drop 1}
  ''PowerEfficiencyModifier

data PowerEfficiencyModifierResearch = PowerEfficiencyModifierResearch
  { _pemrId                      :: ComponentId
  , _pemrResearchCost            :: ComponentResearch
  , _pemrResearchProgress        :: ComponentResearch
  , _pemrPowerEfficiencyModifier :: PowerEfficiencyModifier
  } deriving (Show, Eq, Generic)

makeLenses ''PowerEfficiencyModifierResearch

deriveJSON
  defaultOptions {fieldLabelModifier = drop 1}
  ''PowerEfficiencyModifierResearch

--
-- Fuel Consumption
--
data FuelConsumption = FuelConsumption
  { _fcName               :: ComponentName
  , _fcEfficiencyModifier :: ComponentModifier
  } deriving (Show, Eq, Generic)

makeLenses ''FuelConsumption

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''FuelConsumption

data FuelConsumptionResearch = FuelConsumptionResearch
  { _fcrId               :: ComponentId
  , _fcrResearchCost     :: ComponentResearch
  , _fcrResearchProgress :: ComponentResearch
  , _fcrFuelConsumption  :: FuelConsumption
  } deriving (Show, Eq, Generic)

makeLenses ''FuelConsumptionResearch

deriveJSON
  defaultOptions {fieldLabelModifier = drop 1}
  ''FuelConsumptionResearch

--
-- Engine Size
--
data EngineSize = EngineSize
  { _esSize               :: ComponentSize
  , _esEfficiencyModifier :: ComponentModifier
  } deriving (Show, Eq, Generic)

makeLenses ''EngineSize

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''EngineSize

data EngineSizeResearch = EngineSizeResearch
  { _esrId               :: ComponentId
  , _esrResearchCost     :: ComponentResearch
  , _esrResearchProgress :: ComponentResearch
  , _esrEngineSize       :: EngineSize
  } deriving (Show, Eq, Generic)

makeLenses ''EngineSizeResearch

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''EngineSizeResearch
