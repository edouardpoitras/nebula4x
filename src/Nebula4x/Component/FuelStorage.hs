{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Nebula4x.Component.FuelStorage where

import qualified Data.Map.Strict as Map

import           Nebula4x.Types

fuelStorageMineralCostRatio :: MineralCost
fuelStorageMineralCostRatio =
  Map.fromList [(Duranium, 0.5), (Boronide, 0.5)] :: MineralCost

fuelStorageTiny :: FuelStorage
fuelStorageTiny =
  FuelStorage
    (ComponentName "Tiny Fuel Storage")
    (ComponentCost 2.0)
    (ComponentSize 0.1)
    (ComponentRating 5000)

fuelStorageTinyResearch :: FuelStorageResearch
fuelStorageTinyResearch =
  FuelStorageResearch
    1
    (ComponentResearch 3000)
    (ComponentResearch 0)
    fuelStorageTiny

fuelStorageSmall :: FuelStorage
fuelStorageSmall =
  FuelStorage
    (ComponentName "Small Fuel Storage")
    (ComponentCost 3.0)
    (ComponentSize 0.2)
    (ComponentRating 10000)

fuelStorageSmallResearch :: FuelStorageResearch
fuelStorageSmallResearch =
  FuelStorageResearch
    2
    (ComponentResearch 1000)
    (ComponentResearch 0)
    fuelStorageSmall

fuelStorageBasic :: FuelStorage
fuelStorageBasic =
  FuelStorage
    (ComponentName "Basic Fuel Storage")
    (ComponentCost 10.0)
    (ComponentSize 1)
    (ComponentRating 50000)

fuelStorageBasicResearch :: FuelStorageResearch
fuelStorageBasicResearch =
  FuelStorageResearch
    3
    (ComponentResearch 0)
    (ComponentResearch 0)
    fuelStorageBasic

fuelStorageLarge :: FuelStorage
fuelStorageLarge =
  FuelStorage
    (ComponentName "Large Fuel Storage")
    (ComponentCost 30.0)
    (ComponentSize 4)
    (ComponentRating 250000)

fuelStorageLargeResearch :: FuelStorageResearch
fuelStorageLargeResearch =
  FuelStorageResearch
    4
    (ComponentResearch 1000)
    (ComponentResearch 0)
    fuelStorageLarge

fuelStorageVeryLarge :: FuelStorage
fuelStorageVeryLarge =
  FuelStorage
    (ComponentName "Very Large Fuel Storage")
    (ComponentCost 100.0)
    (ComponentSize 15)
    (ComponentRating 1000000)

fuelStorageVeryLargeResearch :: FuelStorageResearch
fuelStorageVeryLargeResearch =
  FuelStorageResearch
    5
    (ComponentResearch 2500)
    (ComponentResearch 0)
    fuelStorageVeryLarge

fuelStorageUltraLarge :: FuelStorage
fuelStorageUltraLarge =
  FuelStorage
    (ComponentName "Ultra Large Fuel Storage")
    (ComponentCost 400.0)
    (ComponentSize 60)
    (ComponentRating 5000000)

fuelStorageUltraLargeResearch :: FuelStorageResearch
fuelStorageUltraLargeResearch =
  FuelStorageResearch
    6
    (ComponentResearch 5000)
    (ComponentResearch 0)
    fuelStorageUltraLarge
