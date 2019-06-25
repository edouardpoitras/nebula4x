{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Nebula4x.Component.Transport where

import           Control.Lens
import           Control.Newtype.Generics
import qualified Data.Map.Strict          as Map

import           Nebula4x.Types

--
-- Cargo Handling Systems
--
cargoHandlingMineralCostRatio :: MineralCost
cargoHandlingMineralCostRatio =
  Map.fromList [(Duranium, 0.5), (Mercassium, 0.5)]

slowCargoHandlingSystem :: CargoHandlingComponent
slowCargoHandlingSystem =
  CargoHandlingComponent
    (ComponentName "Slow Cargo Handling System")
    (ComponentCost 5.0)
    (ComponentSize 1.0)
    (ComponentRating 2.0)

slowCargoHandlingSystemResearch :: CargoHandlingResearch
slowCargoHandlingSystemResearch =
  CargoHandlingResearch
    1
    (ComponentResearch 200)
    (ComponentResearch 0)
    slowCargoHandlingSystem

standardCargoHandlingSystem :: CargoHandlingComponent
standardCargoHandlingSystem =
  CargoHandlingComponent
    (ComponentName "Standard Cargo Handling System")
    (ComponentCost 10.0)
    (ComponentSize 2.0)
    (ComponentRating 5.0)

standardCargoHandlingSystemResearch :: CargoHandlingResearch
standardCargoHandlingSystemResearch =
  CargoHandlingResearch
    2
    (ComponentResearch 1000)
    (ComponentResearch 0)
    standardCargoHandlingSystem

improvedCargoHandlingSystem :: CargoHandlingComponent
improvedCargoHandlingSystem =
  CargoHandlingComponent
    (ComponentName "Improved Cargo Handling System")
    (ComponentCost 15.0)
    (ComponentSize 3.0)
    (ComponentRating 10.0)

improvedCargoHandlingSystemResearch :: CargoHandlingResearch
improvedCargoHandlingSystemResearch =
  CargoHandlingResearch
    3
    (ComponentResearch 10000)
    (ComponentResearch 0)
    improvedCargoHandlingSystem

advancedCargoHandlingSystem :: CargoHandlingComponent
advancedCargoHandlingSystem =
  CargoHandlingComponent
    (ComponentName "Advanced Cargo Handling System")
    (ComponentCost 25.0)
    (ComponentSize 5.0)
    (ComponentRating 20.0)

advancedCargoHandlingSystemResearch :: CargoHandlingResearch
advancedCargoHandlingSystemResearch =
  CargoHandlingResearch
    4
    (ComponentResearch 50000)
    (ComponentResearch 0)
    advancedCargoHandlingSystem

gravAssistedCargoHandlingSystem :: CargoHandlingComponent
gravAssistedCargoHandlingSystem =
  CargoHandlingComponent
    (ComponentName "Grav-Assisted Cargo Handling System")
    (ComponentCost 50.0)
    (ComponentSize 10.0)
    (ComponentRating 50.0)

gravAssistedCargoHandlingSystemResearch :: CargoHandlingResearch
gravAssistedCargoHandlingSystemResearch =
  CargoHandlingResearch
    5
    (ComponentResearch 200000)
    (ComponentResearch 0)
    gravAssistedCargoHandlingSystem

--
-- Cargo Holds
--
cargoHoldMineralCostRatio :: MineralCost
cargoHoldMineralCostRatio = Map.fromList [(Duranium, 1.0)]

smallCargoHold :: CargoHoldComponent
smallCargoHold =
  CargoHoldComponent
    (ComponentName "Small Cargo Hold")
    (ComponentCost 10.0)
    (ComponentSize 100.0)
    (ComponentRating 2000.0)

smallCargoHoldResearch :: CargoHoldResearch
smallCargoHoldResearch =
  CargoHoldResearch 1 (ComponentResearch 0) (ComponentResearch 0) smallCargoHold

standardCargoHold :: CargoHoldComponent
standardCargoHold =
  CargoHoldComponent
    (ComponentName "Standard Cargo Hold")
    (ComponentCost 40.0)
    (ComponentSize 400.0)
    (ComponentRating 10000.0)

standardCargoHoldResearch :: CargoHoldResearch
standardCargoHoldResearch =
  CargoHoldResearch
    2
    (ComponentResearch 2000)
    (ComponentResearch 0)
    standardCargoHold

largeCargoHold :: CargoHoldComponent
largeCargoHold =
  CargoHoldComponent
    (ComponentName "Large Cargo Hold")
    (ComponentCost 160.0)
    (ComponentSize 1500.0)
    (ComponentRating 50000.0)

largeCargoHoldResearch :: CargoHoldResearch
largeCargoHoldResearch =
  CargoHoldResearch
    3
    (ComponentResearch 10000)
    (ComponentResearch 0)
    largeCargoHold

massiveCargoHold :: CargoHoldComponent
massiveCargoHold =
  CargoHoldComponent
    (ComponentName "Massive Cargo Hold")
    (ComponentCost 500.0)
    (ComponentSize 4000.0)
    (ComponentRating 200000.0)

massiveCargoHoldResearch :: CargoHoldResearch
massiveCargoHoldResearch =
  CargoHoldResearch
    4
    (ComponentResearch 100000)
    (ComponentResearch 0)
    massiveCargoHold

--
-- Jump Gate Construction Modules
--
jumpGateMineralCostRatio :: MineralCost
jumpGateMineralCostRatio =
  Map.fromList [(Duranium, 0.6), (Sorium, 0.4), (Boronide, 0.2)]

smallJumpGate :: JumpGateComponent
smallJumpGate =
  JumpGateComponent
    (ComponentName "Small Jump Gate Construction Module")
    (ComponentCost 500)
    (ComponentSize 500)
    (ComponentRating 1)

smallJumpGateResearch :: JumpGateResearch
smallJumpGateResearch =
  JumpGateResearch
    1
    (ComponentResearch 2500)
    (ComponentResearch 0)
    smallJumpGate

mediumJumpGate :: JumpGateComponent
mediumJumpGate =
  JumpGateComponent
    (ComponentName "Medium Jump Gate Construction Module")
    (ComponentCost 1200)
    (ComponentSize 1000)
    (ComponentRating 4)

mediumJumpGateResearch :: JumpGateResearch
mediumJumpGateResearch =
  JumpGateResearch
    2
    (ComponentResearch 40000)
    (ComponentResearch 0)
    mediumJumpGate

largeJumpGate :: JumpGateComponent
largeJumpGate =
  JumpGateComponent
    (ComponentName "Large Jump Gate Construction Module")
    (ComponentCost 3000)
    (ComponentSize 2000)
    (ComponentRating 12)

largeJumpGateResearch :: JumpGateResearch
largeJumpGateResearch =
  JumpGateResearch
    3
    (ComponentResearch 300000)
    (ComponentResearch 0)
    largeJumpGate

totalJumpGatesRating :: [JumpGateComponent] -> ComponentRating
totalJumpGatesRating jgs = totalRating
  where
    totalRating = ComponentRating tRating
    tRating = foldl tallyComponentRating 0 jgs
    tallyComponentRating count jg = unpack (view jgRating jg) + count
