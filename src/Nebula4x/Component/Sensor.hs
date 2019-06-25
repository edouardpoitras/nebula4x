module Nebula4x.Component.Sensor where

import           Control.Newtype.Generics
import qualified Data.Map.Strict          as Map

import           Nebula4x.Types

geologicalSensorMineralCostRatio :: MineralCost
geologicalSensorMineralCostRatio = Map.fromList [(Uridium, 1.0)] :: MineralCost

basicGeologicalSensor :: Sensor
basicGeologicalSensor =
  GeologicalSensor
    (ComponentName "Basic Geological Sensor")
    (ComponentCost 100.0)
    (ComponentSize 5)
    (ComponentRating 1)

basicGeologicalSensorResearch :: SensorResearch
basicGeologicalSensorResearch =
  SensorResearch
    1
    (ComponentResearch 0)
    (ComponentResearch 0)
    basicGeologicalSensor

improvedGeologicalSensor :: Sensor
improvedGeologicalSensor =
  GeologicalSensor
    (ComponentName "Improved Geological Sensor")
    (ComponentCost 200.0)
    (ComponentSize 10)
    (ComponentRating 3)

improvedGeologicalSensorResearch :: SensorResearch
improvedGeologicalSensorResearch =
  SensorResearch
    2
    (ComponentResearch 10000)
    (ComponentResearch 0)
    improvedGeologicalSensor

advancedGeologicalSensor :: Sensor
advancedGeologicalSensor =
  GeologicalSensor
    (ComponentName "Advanced Geological Sensor")
    (ComponentCost 400.0)
    (ComponentSize 20)
    (ComponentRating 9)

advancedGeologicalSensorResearch :: SensorResearch
advancedGeologicalSensorResearch =
  SensorResearch
    3
    (ComponentResearch 100000)
    (ComponentResearch 0)
    advancedGeologicalSensor

gravitationalSensorMineralCostRatio :: MineralCost
gravitationalSensorMineralCostRatio =
  Map.fromList [(Uridium, 1.0)] :: MineralCost

basicGravitationalSensor :: Sensor
basicGravitationalSensor =
  GravitationalSensor
    (ComponentName "Basic Gravitational Sensor")
    (ComponentCost 100.0)
    (ComponentSize 5)
    (ComponentRating 1)

basicGravitationalSensorResearch :: SensorResearch
basicGravitationalSensorResearch =
  SensorResearch
    4
    (ComponentResearch 0)
    (ComponentResearch 0)
    basicGravitationalSensor

improvedGravitationalSensor :: Sensor
improvedGravitationalSensor =
  GravitationalSensor
    (ComponentName "Improved Gravitational Sensor")
    (ComponentCost 200.0)
    (ComponentSize 10)
    (ComponentRating 3)

improvedGravitationalSensorResearch :: SensorResearch
improvedGravitationalSensorResearch =
  SensorResearch
    5
    (ComponentResearch 10000)
    (ComponentResearch 0)
    improvedGravitationalSensor

advancedGravitationalSensor :: Sensor
advancedGravitationalSensor =
  GravitationalSensor
    (ComponentName "Advanced Gravitational Sensor")
    (ComponentCost 400.0)
    (ComponentSize 20)
    (ComponentRating 9)

advancedGravitationalSensorResearch :: SensorResearch
advancedGravitationalSensorResearch =
  SensorResearch
    6
    (ComponentResearch 100000)
    (ComponentResearch 0)
    advancedGravitationalSensor

isGeoSensor :: Sensor -> Bool
isGeoSensor (GeologicalSensor _ _ _ _) = True
isGeoSensor _                          = False

geologicalSensorRating :: Sensor -> ComponentRating
geologicalSensorRating (GeologicalSensor _ _ _ r) = r
geologicalSensorRating _                          = ComponentRating 0

totalGeologicalSensorRating :: [Sensor] -> ComponentRating
totalGeologicalSensorRating sens = totalRating
  where
    totalRating = ComponentRating tRating
    tRating = foldl tallyComponentRating 0 sens
    tallyComponentRating count sensor =
      unpack (geologicalSensorRating sensor) + count

isGravSensor :: Sensor -> Bool
isGravSensor (GravitationalSensor _ _ _ _) = True
isGravSensor _                             = False

gravitationalSensorRating :: Sensor -> ComponentRating
gravitationalSensorRating (GravitationalSensor _ _ _ r) = r
gravitationalSensorRating _                             = ComponentRating 0

totalGravitationalSensorRating :: [Sensor] -> ComponentRating
totalGravitationalSensorRating sens = totalRating
  where
    totalRating = ComponentRating tRating
    tRating = foldl tallyComponentRating 0 sens
    tallyComponentRating count sensor =
      unpack (gravitationalSensorRating sensor) + count
