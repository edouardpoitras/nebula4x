{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Nebula4x.Component.Engine where

import           Control.Newtype.Generics
import qualified Data.Map.Strict          as Map

import           Nebula4x.Types

engineMineralCostRatio :: MineralCost
engineMineralCostRatio = Map.fromList [(Gallicite, 1.0)]

conventionalEngineTechnology :: EngineTechnology
conventionalEngineTechnology =
  EngineTechnology (ComponentName "Conventional") (ComponentRating 0.2)

conventionalEngineTechnologyResearch :: EngineTechnologyResearch
conventionalEngineTechnologyResearch =
  EngineTechnologyResearch
    1
    (ComponentResearch 0.0)
    (ComponentResearch 0.0)
    conventionalEngineTechnology

nuclearThermalEngineTechnology :: EngineTechnology
nuclearThermalEngineTechnology =
  EngineTechnology (ComponentName "Nuclear Thermal") (ComponentRating 5)

nuclearThermalEngineTechnologyResearch :: EngineTechnologyResearch
nuclearThermalEngineTechnologyResearch =
  EngineTechnologyResearch
    2
    (ComponentResearch 2500.0)
    (ComponentResearch 0.0)
    nuclearThermalEngineTechnology

nuclearPulseEngineTechnology :: EngineTechnology
nuclearPulseEngineTechnology =
  EngineTechnology (ComponentName "Nuclear Pulse") (ComponentRating 8)

nuclearPulseEngineTechnologyResearch :: EngineTechnologyResearch
nuclearPulseEngineTechnologyResearch =
  EngineTechnologyResearch
    3
    (ComponentResearch 5000.0)
    (ComponentResearch 0.0)
    nuclearPulseEngineTechnology

ionEngineTechnology :: EngineTechnology
ionEngineTechnology =
  EngineTechnology (ComponentName "Ion") (ComponentRating 12)

ionEngineTechnologyResearch :: EngineTechnologyResearch
ionEngineTechnologyResearch =
  EngineTechnologyResearch
    4
    (ComponentResearch 10000.0)
    (ComponentResearch 0.0)
    ionEngineTechnology

magnetoPlasmaEngineTechnology :: EngineTechnology
magnetoPlasmaEngineTechnology =
  EngineTechnology (ComponentName "Magneto-Plasma") (ComponentRating 16)

magnetoPlasmaEngineTechnologyResearch :: EngineTechnologyResearch
magnetoPlasmaEngineTechnologyResearch =
  EngineTechnologyResearch
    5
    (ComponentResearch 20000.0)
    (ComponentResearch 0.0)
    magnetoPlasmaEngineTechnology

internalConfinementFusionEngineTechnology :: EngineTechnology
internalConfinementFusionEngineTechnology =
  EngineTechnology
    (ComponentName "Internal Confinement Fusion")
    (ComponentRating 20)

internalConfinementFusionEngineTechnologyResearch :: EngineTechnologyResearch
internalConfinementFusionEngineTechnologyResearch =
  EngineTechnologyResearch
    6
    (ComponentResearch 40000.0)
    (ComponentResearch 0.0)
    internalConfinementFusionEngineTechnology

magneticConfinementFusionEngineTechnology :: EngineTechnology
magneticConfinementFusionEngineTechnology =
  EngineTechnology
    (ComponentName "Magnetic Confinement Fusion")
    (ComponentRating 25)

magneticConfinementFusionEngineTechnologyResearch :: EngineTechnologyResearch
magneticConfinementFusionEngineTechnologyResearch =
  EngineTechnologyResearch
    7
    (ComponentResearch 80000.0)
    (ComponentResearch 0.0)
    magneticConfinementFusionEngineTechnology

inertialConfinementFusionEngineTechnology :: EngineTechnology
inertialConfinementFusionEngineTechnology =
  EngineTechnology
    (ComponentName "Inertial Confinement Fusion")
    (ComponentRating 32)

inertialConfinementFusionEngineTechnologyResearch :: EngineTechnologyResearch
inertialConfinementFusionEngineTechnologyResearch =
  EngineTechnologyResearch
    8
    (ComponentResearch 150000.0)
    (ComponentResearch 0.0)
    inertialConfinementFusionEngineTechnology

solidCoreAntiMatterEngineTechnology :: EngineTechnology
solidCoreAntiMatterEngineTechnology =
  EngineTechnology (ComponentName "Solid-Core Anti-Matter") (ComponentRating 40)

solidCoreAntiMatterEngineTechnologyResearch :: EngineTechnologyResearch
solidCoreAntiMatterEngineTechnologyResearch =
  EngineTechnologyResearch
    9
    (ComponentResearch 300000.0)
    (ComponentResearch 0.0)
    solidCoreAntiMatterEngineTechnology

gasCoreAntiMatterEngineTechnology :: EngineTechnology
gasCoreAntiMatterEngineTechnology =
  EngineTechnology (ComponentName "Gas-Core Anti-Matter") (ComponentRating 50)

gasCoreAntiMatterEngineTechnologyResearch :: EngineTechnologyResearch
gasCoreAntiMatterEngineTechnologyResearch =
  EngineTechnologyResearch
    10
    (ComponentResearch 600000.0)
    (ComponentResearch 0.0)
    gasCoreAntiMatterEngineTechnology

plasmaCoreAntiMatterEngineTechnology :: EngineTechnology
plasmaCoreAntiMatterEngineTechnology =
  EngineTechnology
    (ComponentName "Plasma-Core Anti-Matter")
    (ComponentRating 60)

plasmaCoreAntiMatterEngineTechnologyResearch :: EngineTechnologyResearch
plasmaCoreAntiMatterEngineTechnologyResearch =
  EngineTechnologyResearch
    11
    (ComponentResearch 1250000.0)
    (ComponentResearch 0.0)
    plasmaCoreAntiMatterEngineTechnology

beamCoreAntiMatterEngineTechnology :: EngineTechnology
beamCoreAntiMatterEngineTechnology =
  EngineTechnology (ComponentName "Beam-Core Anti-Matter") (ComponentRating 80)

beamCoreAntiMatterEngineTechnologyResearch :: EngineTechnologyResearch
beamCoreAntiMatterEngineTechnologyResearch =
  EngineTechnologyResearch
    12
    (ComponentResearch 2500000.0)
    (ComponentResearch 0.0)
    beamCoreAntiMatterEngineTechnology

photonicEngineTechnology :: EngineTechnology
photonicEngineTechnology =
  EngineTechnology (ComponentName "Photonic") (ComponentRating 100)

photonicEngineTechnologyResearch :: EngineTechnologyResearch
photonicEngineTechnologyResearch =
  EngineTechnologyResearch
    13
    (ComponentResearch 5000000.0)
    (ComponentResearch 0.0)
    photonicEngineTechnology

--
-- Power Efficiency Modifier
--
power1Efficiency1Modifier :: PowerEfficiencyModifier
power1Efficiency1Modifier =
  PowerEfficiencyModifier
    (ComponentName "Power x1.00 Fuel Consumption x1.00")
    (ComponentModifier 1.0)
    (ComponentModifier 1.0)

power1Efficiency1ModifierResearch :: PowerEfficiencyModifierResearch
power1Efficiency1ModifierResearch =
  PowerEfficiencyModifierResearch
    1
    (ComponentResearch 0.0)
    (ComponentResearch 0.0)
    power1Efficiency1Modifier

power05Efficiency018Modifier :: PowerEfficiencyModifier
power05Efficiency018Modifier =
  PowerEfficiencyModifier
    (ComponentName "Power x0.50 Fuel Consumption x0.18")
    (ComponentModifier 0.5)
    (ComponentModifier 0.18)

power05Efficiency018ModifierResearch :: PowerEfficiencyModifierResearch
power05Efficiency018ModifierResearch =
  PowerEfficiencyModifierResearch
    2
    (ComponentResearch 0.0)
    (ComponentResearch 0.0)
    power05Efficiency018Modifier

power125Efficiency175Modifier :: PowerEfficiencyModifier
power125Efficiency175Modifier =
  PowerEfficiencyModifier
    (ComponentName "Power x1.25 Fuel Consumption x1.75")
    (ComponentModifier 1.25)
    (ComponentModifier 1.75)

power125Efficiency175ModifierResearch :: PowerEfficiencyModifierResearch
power125Efficiency175ModifierResearch =
  PowerEfficiencyModifierResearch
    3
    (ComponentResearch 1000.0)
    (ComponentResearch 0.0)
    power125Efficiency175Modifier

power04Efficiency01Modifier :: PowerEfficiencyModifier
power04Efficiency01Modifier =
  PowerEfficiencyModifier
    (ComponentName "Power x0.40 Fuel Consumption x0.10")
    (ComponentModifier 0.4)
    (ComponentModifier 0.1)

power04Efficiency01ModifierResearch :: PowerEfficiencyModifierResearch
power04Efficiency01ModifierResearch =
  PowerEfficiencyModifierResearch
    4
    (ComponentResearch 1000.0)
    (ComponentResearch 0.0)
    power04Efficiency01Modifier

power15Efficiency275Modifier :: PowerEfficiencyModifier
power15Efficiency275Modifier =
  PowerEfficiencyModifier
    (ComponentName "Power x1.5 Fuel Consumption x2.75")
    (ComponentModifier 1.5)
    (ComponentModifier 2.75)

power15Efficiency275ModifierResearch :: PowerEfficiencyModifierResearch
power15Efficiency275ModifierResearch =
  PowerEfficiencyModifierResearch
    5
    (ComponentResearch 2000.0)
    (ComponentResearch 0.0)
    power15Efficiency275Modifier

power03Efficiency005Modifier :: PowerEfficiencyModifier
power03Efficiency005Modifier =
  PowerEfficiencyModifier
    (ComponentName "Power x0.3 Fuel Consumption x0.05")
    (ComponentModifier 0.3)
    (ComponentModifier 0.05)

power03Efficiency005ModifierResearch :: PowerEfficiencyModifierResearch
power03Efficiency005ModifierResearch =
  PowerEfficiencyModifierResearch
    6
    (ComponentResearch 2000.0)
    (ComponentResearch 0.0)
    power03Efficiency005Modifier

power175Efficiency4Modifier :: PowerEfficiencyModifier
power175Efficiency4Modifier =
  PowerEfficiencyModifier
    (ComponentName "Power x1.75 Fuel Consumption x4.00")
    (ComponentModifier 1.75)
    (ComponentModifier 4.0)

power175Efficiency4ModifierResearch :: PowerEfficiencyModifierResearch
power175Efficiency4ModifierResearch =
  PowerEfficiencyModifierResearch
    7
    (ComponentResearch 4000.0)
    (ComponentResearch 0.0)
    power175Efficiency4Modifier

power025Efficiency003Modifier :: PowerEfficiencyModifier
power025Efficiency003Modifier =
  PowerEfficiencyModifier
    (ComponentName "Power x0.25 Fuel Consumption x0.03")
    (ComponentModifier 0.25)
    (ComponentModifier 0.03)

power025Efficiency003ModifierResearch :: PowerEfficiencyModifierResearch
power025Efficiency003ModifierResearch =
  PowerEfficiencyModifierResearch
    8
    (ComponentResearch 4000.0)
    (ComponentResearch 0.0)
    power025Efficiency003Modifier

power2Efficiency55Modifier :: PowerEfficiencyModifier
power2Efficiency55Modifier =
  PowerEfficiencyModifier
    (ComponentName "Power x2.00 Fuel Consumption x5.50")
    (ComponentModifier 2.0)
    (ComponentModifier 5.5)

power2Efficiency55ModifierResearch :: PowerEfficiencyModifierResearch
power2Efficiency55ModifierResearch =
  PowerEfficiencyModifierResearch
    9
    (ComponentResearch 8000.0)
    (ComponentResearch 0.0)
    power2Efficiency55Modifier

power02Efficiency002Modifier :: PowerEfficiencyModifier
power02Efficiency002Modifier =
  PowerEfficiencyModifier
    (ComponentName "Power x0.20 Fuel Consumption x0.02")
    (ComponentModifier 0.2)
    (ComponentModifier 0.02)

power02Efficiency002ModifierResearch :: PowerEfficiencyModifierResearch
power02Efficiency002ModifierResearch =
  PowerEfficiencyModifierResearch
    10
    (ComponentResearch 8000.0)
    (ComponentResearch 0.0)
    power02Efficiency002Modifier

power25Efficiency10Modifier :: PowerEfficiencyModifier
power25Efficiency10Modifier =
  PowerEfficiencyModifier
    (ComponentName "Power x2.50 Fuel Consumption x10.00")
    (ComponentModifier 2.5)
    (ComponentModifier 10.0)

power25Efficiency10ModifierResearch :: PowerEfficiencyModifierResearch
power25Efficiency10ModifierResearch =
  PowerEfficiencyModifierResearch
    11
    (ComponentResearch 15000.0)
    (ComponentResearch 0.0)
    power25Efficiency10Modifier

power015Efficiency001Modifier :: PowerEfficiencyModifier
power015Efficiency001Modifier =
  PowerEfficiencyModifier
    (ComponentName "Power x0.15 Fuel Consumption x0.01")
    (ComponentModifier 0.15)
    (ComponentModifier 0.01)

power015Efficiency001ModifierResearch :: PowerEfficiencyModifierResearch
power015Efficiency001ModifierResearch =
  PowerEfficiencyModifierResearch
    12
    (ComponentResearch 15000.0)
    (ComponentResearch 0.0)
    power015Efficiency001Modifier

power3Efficiency15Modifier :: PowerEfficiencyModifier
power3Efficiency15Modifier =
  PowerEfficiencyModifier
    (ComponentName "Power x3.00 Fuel Consumption x15.00")
    (ComponentModifier 2.5)
    (ComponentModifier 10.0)

power3Efficiency15ModifierResearch :: PowerEfficiencyModifierResearch
power3Efficiency15ModifierResearch =
  PowerEfficiencyModifierResearch
    13
    (ComponentResearch 30000.0)
    (ComponentResearch 0.0)
    power3Efficiency15Modifier

power01Efficiency0001Modifier :: PowerEfficiencyModifier
power01Efficiency0001Modifier =
  PowerEfficiencyModifier
    (ComponentName "Power x0.10 Fuel Consumption x0.001")
    (ComponentModifier 0.1)
    (ComponentModifier 0.001)

power01Efficiency0001ModifierResearch :: PowerEfficiencyModifierResearch
power01Efficiency0001ModifierResearch =
  PowerEfficiencyModifierResearch
    14
    (ComponentResearch 30000.0)
    (ComponentResearch 0.0)
    power01Efficiency0001Modifier

--
-- Fuel Consumption
--
fuelConsumption1 :: FuelConsumption
fuelConsumption1 =
  FuelConsumption
    (ComponentName "1 Litre per Engine Power Hour")
    (ComponentModifier 1.0)

fuelConsumption1Research :: FuelConsumptionResearch
fuelConsumption1Research =
  FuelConsumptionResearch
    1
    (ComponentResearch 0.0)
    (ComponentResearch 0.0)
    fuelConsumption1

fuelConsumption09 :: FuelConsumption
fuelConsumption09 =
  FuelConsumption
    (ComponentName "0.9 Litre per Engine Power Hour")
    (ComponentModifier 0.9)

fuelConsumption09Research :: FuelConsumptionResearch
fuelConsumption09Research =
  FuelConsumptionResearch
    2
    (ComponentResearch 1000.0)
    (ComponentResearch 0.0)
    fuelConsumption09

fuelConsumption08 :: FuelConsumption
fuelConsumption08 =
  FuelConsumption
    (ComponentName "0.8 Litre per Engine Power Hour")
    (ComponentModifier 0.8)

fuelConsumption08Research :: FuelConsumptionResearch
fuelConsumption08Research =
  FuelConsumptionResearch
    3
    (ComponentResearch 2000.0)
    (ComponentResearch 0.0)
    fuelConsumption08

fuelConsumption07 :: FuelConsumption
fuelConsumption07 =
  FuelConsumption
    (ComponentName "0.7 Litre per Engine Power Hour")
    (ComponentModifier 0.7)

fuelConsumption07Research :: FuelConsumptionResearch
fuelConsumption07Research =
  FuelConsumptionResearch
    4
    (ComponentResearch 4000.0)
    (ComponentResearch 0.0)
    fuelConsumption07

fuelConsumption06 :: FuelConsumption
fuelConsumption06 =
  FuelConsumption
    (ComponentName "0.6 Litre per Engine Power Hour")
    (ComponentModifier 0.6)

fuelConsumption06Research :: FuelConsumptionResearch
fuelConsumption06Research =
  FuelConsumptionResearch
    5
    (ComponentResearch 8000.0)
    (ComponentResearch 0.0)
    fuelConsumption06

fuelConsumption05 :: FuelConsumption
fuelConsumption05 =
  FuelConsumption
    (ComponentName "0.5 Litre per Engine Power Hour")
    (ComponentModifier 0.5)

fuelConsumption05Research :: FuelConsumptionResearch
fuelConsumption05Research =
  FuelConsumptionResearch
    6
    (ComponentResearch 15000.0)
    (ComponentResearch 0.0)
    fuelConsumption05

fuelConsumption04 :: FuelConsumption
fuelConsumption04 =
  FuelConsumption
    (ComponentName "0.4 Litre per Engine Power Hour")
    (ComponentModifier 0.4)

fuelConsumption04Research :: FuelConsumptionResearch
fuelConsumption04Research =
  FuelConsumptionResearch
    7
    (ComponentResearch 30000.0)
    (ComponentResearch 0.0)
    fuelConsumption04

fuelConsumption03 :: FuelConsumption
fuelConsumption03 =
  FuelConsumption
    (ComponentName "0.3 Litre per Engine Power Hour")
    (ComponentModifier 0.3)

fuelConsumption03Research :: FuelConsumptionResearch
fuelConsumption03Research =
  FuelConsumptionResearch
    8
    (ComponentResearch 60000.0)
    (ComponentResearch 0.0)
    fuelConsumption03

fuelConsumption025 :: FuelConsumption
fuelConsumption025 =
  FuelConsumption
    (ComponentName "0.25 Litre per Engine Power Hour")
    (ComponentModifier 0.25)

fuelConsumption025Research :: FuelConsumptionResearch
fuelConsumption025Research =
  FuelConsumptionResearch
    9
    (ComponentResearch 120000.0)
    (ComponentResearch 0.0)
    fuelConsumption025

fuelConsumption02 :: FuelConsumption
fuelConsumption02 =
  FuelConsumption
    (ComponentName "0.2 Litre per Engine Power Hour")
    (ComponentModifier 0.2)

fuelConsumption02Research :: FuelConsumptionResearch
fuelConsumption02Research =
  FuelConsumptionResearch
    10
    (ComponentResearch 250000.0)
    (ComponentResearch 0.0)
    fuelConsumption02

fuelConsumption016 :: FuelConsumption
fuelConsumption016 =
  FuelConsumption
    (ComponentName "0.16 Litre per Engine Power Hour")
    (ComponentModifier 0.16)

fuelConsumption016Research :: FuelConsumptionResearch
fuelConsumption016Research =
  FuelConsumptionResearch
    11
    (ComponentResearch 500000.0)
    (ComponentResearch 0.0)
    fuelConsumption016

fuelConsumption0125 :: FuelConsumption
fuelConsumption0125 =
  FuelConsumption
    (ComponentName "0.125 Litre per Engine Power Hour")
    (ComponentModifier 0.125)

fuelConsumption0125Research :: FuelConsumptionResearch
fuelConsumption0125Research =
  FuelConsumptionResearch
    12
    (ComponentResearch 1000000.0)
    (ComponentResearch 0.0)
    fuelConsumption0125

fuelConsumption01 :: FuelConsumption
fuelConsumption01 =
  FuelConsumption
    (ComponentName "0.1 Litre per Engine Power Hour")
    (ComponentModifier 0.1)

fuelConsumption01Research :: FuelConsumptionResearch
fuelConsumption01Research =
  FuelConsumptionResearch
    13
    (ComponentResearch 2000000.0)
    (ComponentResearch 0.0)
    fuelConsumption01

--
-- Engine Sizes
--
engineSize20 :: EngineSize
engineSize20 = EngineSize (ComponentSize 20.0) (ComponentModifier 0.80)

engineSize20Research :: EngineSizeResearch
engineSize20Research =
  EngineSizeResearch
    20
    (ComponentResearch 0.0)
    (ComponentResearch 0.0)
    engineSize20

engineSize25 :: EngineSize
engineSize25 = EngineSize (ComponentSize 25.0) (ComponentModifier 0.75)

engineSize25Research :: EngineSizeResearch
engineSize25Research =
  EngineSizeResearch
    25
    (ComponentResearch 0.0)
    (ComponentResearch 0.0)
    engineSize25

engineSize30 :: EngineSize
engineSize30 = EngineSize (ComponentSize 30.0) (ComponentModifier 0.70)

engineSize30Research :: EngineSizeResearch
engineSize30Research =
  EngineSizeResearch
    30
    (ComponentResearch 0.0)
    (ComponentResearch 0.0)
    engineSize30

engineSize15 :: EngineSize
engineSize15 = EngineSize (ComponentSize 15.0) (ComponentModifier 0.85)

engineSize15Research :: EngineSizeResearch
engineSize15Research =
  EngineSizeResearch
    15
    (ComponentResearch 100.0)
    (ComponentResearch 0.0)
    engineSize15

engineSize35 :: EngineSize
engineSize35 = EngineSize (ComponentSize 35.0) (ComponentModifier 0.65)

engineSize35Research :: EngineSizeResearch
engineSize35Research =
  EngineSizeResearch
    35
    (ComponentResearch 100.0)
    (ComponentResearch 0.0)
    engineSize35

engineSize10 :: EngineSize
engineSize10 = EngineSize (ComponentSize 10.0) (ComponentModifier 0.90)

engineSize10Research :: EngineSizeResearch
engineSize10Research =
  EngineSizeResearch
    10
    (ComponentResearch 200.0)
    (ComponentResearch 0.0)
    engineSize10

engineSize40 :: EngineSize
engineSize40 = EngineSize (ComponentSize 40.0) (ComponentModifier 0.60)

engineSize40Research :: EngineSizeResearch
engineSize40Research =
  EngineSizeResearch
    40
    (ComponentResearch 200.0)
    (ComponentResearch 0.0)
    engineSize40

engineSize5 :: EngineSize
engineSize5 = EngineSize (ComponentSize 5.0) (ComponentModifier 0.95)

engineSize5Research :: EngineSizeResearch
engineSize5Research =
  EngineSizeResearch
    5
    (ComponentResearch 300.0)
    (ComponentResearch 0.0)
    engineSize5

engineSize45 :: EngineSize
engineSize45 = EngineSize (ComponentSize 45.0) (ComponentModifier 0.55)

engineSize45Research :: EngineSizeResearch
engineSize45Research =
  EngineSizeResearch
    45
    (ComponentResearch 300.0)
    (ComponentResearch 0.0)
    engineSize45

engineSize1 :: EngineSize
engineSize1 = EngineSize (ComponentSize 1.0) (ComponentModifier 0.99)

engineSize1Research :: EngineSizeResearch
engineSize1Research =
  EngineSizeResearch
    1
    (ComponentResearch 500.0)
    (ComponentResearch 0.0)
    engineSize1

engineSize50 :: EngineSize
engineSize50 = EngineSize (ComponentSize 50.0) (ComponentModifier 0.5)

engineSize50Research :: EngineSizeResearch
engineSize50Research =
  EngineSizeResearch
    50
    (ComponentResearch 500.0)
    (ComponentResearch 0.0)
    engineSize50

--
-- Engine Designs
--
newEngineDesign ::
     ComponentId
  -> ComponentName
  -> EngineTechnology
  -> PowerEfficiencyModifier
  -> FuelConsumption
  -> EngineSize
  -> EngineDesign
newEngineDesign cId name (EngineTechnology _ (ComponentRating enginePower)) (PowerEfficiencyModifier _ (ComponentModifier powerModifier) (ComponentModifier efficiencyModifier)) (FuelConsumption _ (ComponentModifier fuelConsumptionModifier)) (EngineSize (ComponentSize engineSize) (ComponentModifier engineSizeModifier)) =
  engineDesign
  where
    engineDesign =
      EngineDesign
        cId
        (ComponentResearch researchCost)
        (ComponentResearch 0.0)
        engine
    engine =
      Engine
        name
        engineType
        (pack cost)
        (pack engineSize)
        (pack power)
        (pack fuelPerHour)
    engineType =
      if engineSize >= 25 && powerModifier <= 0.5
        then CommercialEngine
        else MilitaryEngine
    cost = 0.1 * researchCost
    basePower = enginePower * engineSize
    power = basePower * powerModifier
    fuelPerHour =
      power * efficiencyModifier * fuelConsumptionModifier * engineSizeModifier
    researchCost =
      case engineType of
        CommercialEngine -> power * 2
        MilitaryEngine ->
          if powerModifier >= 1.0
            then power * 5
            else power * 5 + (1.0 - powerModifier) * 2 * power
