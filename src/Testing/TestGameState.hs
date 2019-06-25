{-# LANGUAGE ScopedTypeVariables #-}

module Testing.TestGameState where

import           Control.Lens
import           Data.Map.Strict               as Map
import           System.Random

import           Nebula4x.Component.Armor
import           Nebula4x.Component.Engine
import           Nebula4x.Component.FuelStorage
import           Nebula4x.Component.Laser
import           Nebula4x.Component.Shield
import           Nebula4x.Component.Sensor
import           Nebula4x.Component.Transport
import           Nebula4x.GameState
import           Nebula4x.Installment
import           Nebula4x.Mineral
import           Nebula4x.Research
import           Nebula4x.Ship
import           Nebula4x.Shipyard
import           Nebula4x.StarSystem
import           Nebula4x.Time
import           Nebula4x.Types
import           Nebula4x.Utils
import           Nebula4x.Wormhole

startingSeed :: StdGen
startingSeed = mkStdGen 1

startingRace :: ComponentId
startingRace = 1

enemyRace :: ComponentId
enemyRace = 2

startingStars :: Int
startingStars = 100

testingResearch :: GameState -> GameState
testingResearch =
  set (research . at'' startingRace)
      (Just (set rShipDesigns testShipDesigns startingResearch))
    . set (research . at'' enemyRace)
          (Just (set rShipDesigns testEnemyShipDesigns startingResearch))

testGameState :: IO GameState
testGameState = do
  let startingWithResearch = testingResearch startingGameState
  let gs = startingWithResearch { _systems      = testSystems startingSeed
                                , _races        = testRaces
                                , _universeTime = addSeconds (-1) defaultStartTime
                                }
  -- Need to tick once to align the orbits
  (gs', _) <- tick 1 gs
  return gs'

testRaces :: Races
testRaces = Map.fromList
  [ (startingRace, Race startingRace (RaceName "Human") Human)
  , (enemyRace   , Race enemyRace (RaceName "Zerg") AI)
  ]

--
-- Test Systems
--
testSystems :: StdGen -> StarSystems
testSystems gen = newSystems
 where
  (systems', gen') =
    generateStarSystems gen (view config startingGameState) startingStars
  systems'' = Map.insert
    (view ssId starSystem1)
    starSystem1
    (Map.insert (view ssId starSystem2) starSystem2 systems')
  (newSystems, _) = generateWormholes gen' systems''

system1Wormholes :: Wormholes
system1Wormholes = Map.insert
  9000
  (Wormhole 9000
            (WormholeLocation (-10000000) (-10000000))
            (Just 9001)
            (Just 2)
            (Just $ WormholeLocation 10000000 10000000)
            True
            (TaskProgress 1)
            True
            (TaskProgress 1)
  )
  noWormholes

system2Wormholes :: Wormholes
system2Wormholes = Map.insert
  9001
  (Wormhole 9001
            (WormholeLocation 10000000 10000000)
            (Just 9000)
            (Just 1)
            (Just $ WormholeLocation (-10000000) (-10000000))
            True
            (TaskProgress 1)
            True
            (TaskProgress 1)
  )
  noWormholes

starSystem1 :: StarSystem
starSystem1 = StarSystem
  1
  star1
  (Map.fromList
    [ (view bId planet1, planet1)
    , (view bId planet2, planet2)
    , (view bId moon1  , moon1)
    ]
  )
  noPackets
  (Map.fromList
    [ (view sId commercialShip1, commercialShip1)
    , (view sId militaryShip1  , militaryShip1)
    , (view sId militaryShip2  , militaryShip2)
    ]
  )
  noMissleSalvos
  system1Wormholes
  (Map.fromList [(startingRace, True)])

starSystem2 :: StarSystem
starSystem2 = StarSystem
  2
  star2
  (Map.fromList [(view bId planet3, planet3), (view bId planet4, planet4)])
  noPackets
  noShips
  noMissleSalvos
  system2Wormholes
  (Map.fromList [(enemyRace, True)])

star1 :: Star
star1 = Star 1 (BodyName "Star 1") (BodyMass 2.0e30) (BodyRadius 700000)

star2 :: Star
star2 = Star 2 (BodyName "Star 2") (BodyMass 3.0e30) (BodyRadius 900000)

moon1 :: Body
moon1 = Body 5
             noRace
             (BodyName "Moon 1")
             (BodyMass 1e18)
             (BodyLocation 0 0)
             (BodyRadius 1e4)
             (Range 1e6 1e6)
             Moon
             noShipyards
             (FuelReserves 0)
             notRefining
             noInstallments
             noMinerals
             Nothing
             noSurveys
             (Just 1)

planet1 :: Body
planet1 = Body 1
               (Just startingRace)
               (BodyName "Planet 1")
               (BodyMass 6.0e24)
               (BodyLocation 0 0)
               (BodyRadius 6000)
               (Range 100000000 110000000)
               Terrestrial
               planet1Shipyards
               (FuelReserves 1000000)
               (ActiveRefining True)
               planet1Installments
               planet1Minerals
               Nothing
               (Map.fromList [(startingRace, BodySurvey True 1.0)])
               Nothing

planet1Installments :: Installments
planet1Installments =
  (Map.insert
      (_iId basicConstructionFactory)
      (InstallmentStack (InstallmentCount 10000) basicConstructionFactory)
    )
    $ (Map.insert (_iId basicResearchLab)
                  (InstallmentStack (InstallmentCount 1000) basicResearchLab)
                  noInstallments
      )

planet2 :: Body
planet2 = Body
  2
  noRace
  (BodyName "Planet 2")
  (BodyMass 9.0e24)
  (BodyLocation 0 0)
  (BodyRadius 9000)
  (Range 200000000 250000000)
  GasGiant
  noShipyards
  (FuelReserves 0)
  notRefining
  (Map.insert (_iId basicMassDriver)
              (InstallmentStack (InstallmentCount 1000) basicMassDriver)
              noInstallments
  )
  planet2Minerals
  (Just 1)
  noSurveys
  Nothing

planet3 :: Body
planet3 = Body 3
               (Just enemyRace)
               (BodyName "Planet 3")
               (BodyMass 3.0e25)
               (BodyLocation 0 0)
               (BodyRadius 11000)
               (Range 50000000 51000000)
               Terrestrial
               noShipyards
               (FuelReserves 0)
               notRefining
               noInstallments
               noMinerals
               Nothing
               noSurveys
               Nothing

planet4 :: Body
planet4 = Body 4
               (Just enemyRace)
               (BodyName "Planet 4")
               (BodyMass 5.0e23)
               (BodyLocation 0 0)
               (BodyRadius 2000)
               (Range 250000000 350000000)
               Terrestrial
               noShipyards
               (FuelReserves 0)
               notRefining
               noInstallments
               noMinerals
               Nothing
               noSurveys
               Nothing

planet1Shipyards :: Shipyards
planet1Shipyards = Map.fromList
  [ (view syId planet1Shipyard1, planet1Shipyard1)
  , (view syId planet1Shipyard2, planet1Shipyard2)
  ]

planet1Minerals :: Minerals
planet1Minerals = Map.fromList
  [ ( Duranium
    , Mineral Duranium
              (Accessibility 1.0)
              (MineralCount 1000000)
              (MineralCount 100000)
    )
  , ( Gallicite
    , Mineral Gallicite
              (Accessibility 1.0)
              (MineralCount 1000000)
              (MineralCount 100000)
    )
  , ( Neutronium
    , Mineral Neutronium
              (Accessibility 1.0)
              (MineralCount 1000000)
              (MineralCount 100000)
    )
  , ( Corbomite
    , Mineral Corbomite
              (Accessibility 1.0)
              (MineralCount 1000000)
              (MineralCount 100000)
    )
  , ( Corundium
    , Mineral Corundium
              (Accessibility 1.0)
              (MineralCount 1000000)
              (MineralCount 100000)
    )
  , ( Sorium
    , Mineral Sorium
              (Accessibility 1.0)
              (MineralCount 1000000)
              (MineralCount 100000)
    )
  , ( Vendarite
    , Mineral Vendarite
              (Accessibility 1.0)
              (MineralCount 1000000)
              (MineralCount 100000)
    )
  , ( Tritanium
    , Mineral Tritanium
              (Accessibility 1.0)
              (MineralCount 1000000)
              (MineralCount 100000)
    )
  , ( Uridium
    , Mineral Uridium
              (Accessibility 1.0)
              (MineralCount 1000000)
              (MineralCount 100000)
    )
  , ( Mercassium
    , Mineral Mercassium
              (Accessibility 1.0)
              (MineralCount 1000000)
              (MineralCount 100000)
    )
  , ( Boronide
    , Mineral Boronide
              (Accessibility 1.0)
              (MineralCount 1000000)
              (MineralCount 100000)
    )
  ]

planet2Minerals :: Minerals
planet2Minerals = planet1Minerals

planet1Shipyard1 :: Shipyard
planet1Shipyard1 =
  newShipyard 1 (ShipyardName "Planet 1 Commercial Shipyard") CommercialShipyard

planet1Shipyard2 :: Shipyard
planet1Shipyard2 =
  newShipyard 2 (ShipyardName "Planet 1 Military Shipyard") NavyShipyard

commercialShip1NoFuel :: Ship
commercialShip1NoFuel = newShip 1337
                                1
                                (ShipName "Commercial Ship 1")
                                (ShipLocation (-100000000) (-6500000))
                                commercialShipDesign1

commercialShip1 :: Ship
commercialShip1 = commercialShip1NoFuel { _sFuel = ShipFuel 5000000 }

militaryShip1 :: Ship
militaryShip1 = (newShip 1338
                         2
                         (ShipName "Military Ship 1")
                         (ShipLocation (-101000000) (-6500000))
                         militaryShipDesign1
                ) { _sFuel = ShipFuel 1000000
                  }

militaryShip2 :: Ship
militaryShip2 = (newShip 1339
                         2
                         (ShipName "Military Ship 2")
                         (ShipLocation (-101000000) (-7500000))
                         militaryShipDesign1
                ) { _sFuel = ShipFuel 1000000
                  }

--
-- Test Designs
--
testShipDesigns :: ResearchStatus ShipDesign
testShipDesigns = ResearchStatus
  (Map.fromList
    [ (view sdId commercialShipDesign1, commercialShipDesign1)
    , (view sdId militaryShipDesign1  , militaryShipDesign1)
    ]
  )
  Nothing
  Map.empty
  Map.empty

testEnemyShipDesigns :: ResearchStatus ShipDesign
testEnemyShipDesigns = ResearchStatus
  (Map.fromList [(view sdId militaryShipDesign2, militaryShipDesign2)])
  Nothing
  Map.empty
  Map.empty

commercialEngineDesign1 :: EngineDesign
commercialEngineDesign1 = newEngineDesign
  1
  (ComponentName "Commercial Photonic Engine Design 1")
  photonicEngineTechnology
  power1Efficiency1Modifier
  fuelConsumption01
  engineSize50

militaryEngineDesign1 :: EngineDesign
militaryEngineDesign1 = newEngineDesign
  2
  (ComponentName "Military Conventional Engine Design 1")
  conventionalEngineTechnology
  power1Efficiency1Modifier
  fuelConsumption1
  engineSize10

commercialEngineDesign2 :: EngineDesign
commercialEngineDesign2 = newEngineDesign
  3
  (ComponentName "Commercial Nuclear Engine Design 2")
  nuclearThermalEngineTechnology
  power05Efficiency018Modifier
  fuelConsumption1
  engineSize10

commercialShipDesign1 :: ShipDesign
commercialShipDesign1 = newShipDesign
  3
  (ShipName "Commercial Ship Design 1")
  [ conventionalArmor
  , conventionalArmor
  , conventionalArmor
  , conventionalArmor
  , conventionalArmor
  , conventionalArmor
  ]
  [alphaShield]
  [view edEngine commercialEngineDesign1]
  []
  []
  [fuelStorageUltraLarge]
  []
  [largeCargoHold, largeCargoHold, largeCargoHold, largeCargoHold]
  [largeJumpGate]
  [basicGeologicalSensor]
  [basicGravitationalSensor]

testMissleLauncher :: MissleLauncher
testMissleLauncher = MissleLauncher
  (ComponentName "Test Missle Launcher")
  (ComponentCost 1)
  (ComponentSize 1)
  (ComponentRating 0.01)
  (Missle (MissleSpeed 20000) (MissleRange 50000000) (MissleStrength 1))

testLaser :: Laser
testLaser = Laser (ComponentName "Test Laser")
                  (ComponentCost 0)
                  (ComponentSize 1)
                  (ComponentRating 500000) -- Range
                  (ComponentRating 0.01) -- Recharge Rate
                  (ComponentRating 10) -- Damage

militaryShipDesign1 :: ShipDesign
militaryShipDesign1 = newShipDesign 4
                                    (ShipName "Military Ship Design 1")
                                    [conventionalArmor]
                                    [alphaShield]
                                    [view edEngine militaryEngineDesign1]
                                    [testMissleLauncher]
                                    [testLaser]
                                    [fuelStorageBasic]
                                    []
                                    []
                                    []
                                    []
                                    []

militaryShipDesign2 :: ShipDesign
militaryShipDesign2 = newShipDesign 5
                                    (ShipName "Zerg Military Ship Design")
                                    [conventionalArmor, conventionalArmor]
                                    [alphaShield, alphaShield]
                                    [view edEngine militaryEngineDesign1]
                                    [testMissleLauncher]
                                    [testLaser]
                                    [fuelStorageBasic]
                                    []
                                    []
                                    []
                                    []
                                    []

--
-- Test Research
--
testResearch :: Research
testResearch = startingResearch { _rEngineDesigns = testEngineDesign }

testEngineDesign :: ResearchStatus EngineDesign
testEngineDesign = ResearchStatus
  (Map.fromList [(view edId commercialEngineDesign1, commercialEngineDesign1)])
  (Just militaryEngineDesign1)
  Map.empty
  (Map.fromList [(view edId commercialEngineDesign2, commercialEngineDesign2)])

--
-- Sandbox Testing Only
--

sandboxShipDesign :: ShipDesign
sandboxShipDesign = newShipDesign
  88
  (ShipName "Sandbox Ship Design")
  [ conventionalArmor
  , conventionalArmor
  , conventionalArmor
  , conventionalArmor
  , conventionalArmor
  ]
                                  --[alphaShield]
  []
  [view edEngine militaryEngineDesign1]
  []
  []
  [fuelStorageBasic]
  []
  []
  []
  []
  []

sandboxShip :: Ship
sandboxShip = enableShipShields
  (newShip 89 1 (ShipName "Sandbox Ship") (ShipLocation 0 0) sandboxShipDesign)

sandboxLaser :: Laser
sandboxLaser = Laser (ComponentName "Test Laser")
                     (ComponentCost 0)
                     (ComponentSize 1)
                     (ComponentRating 1000000) -- Range
                     (ComponentRating 0.01) -- Recharge Rate
                     (ComponentRating 165) -- Damage

sandboxLasers :: Lasers
sandboxLasers = [sandboxLaser]

sandboxLaserTest :: (Maybe Ship, System.Random.StdGen)
sandboxLaserTest =
  lasersAttackShip (mkStdGen 88888888) sandboxShip sandboxLasers

sandboxTest :: String
sandboxTest = pTraceShow sandboxLaserTest $ "Test Complete"
