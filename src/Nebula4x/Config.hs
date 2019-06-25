module Nebula4x.Config where

import qualified Data.Map.Strict               as Map
import           System.Random

import           Nebula4x.Types

minId :: Int
minId = 1

-- The maximum javascript integer value (higher values have unspecified behavior)
-- Used to integrate nicely with web-based UIs
maxId :: Int
maxId = 2 ^ (53 :: Int)

randomId :: RandomGen g => g -> (Int, g)
randomId = randomR (minId, maxId)

randomIds :: RandomGen g => g -> ([Int], g)
randomIds g = (randomNumbers, gen')
 where
  (gen, gen')   = split g
  randomNumbers = randomRs (minId, maxId) gen

startingConfig :: Config
startingConfig = Config { _universeConfig   = startingUniverseConfig
                        , _pulseConfig      = startingPulseConfig
                        , _asteroidConfig   = startingAsteroidConfig
                        , _cometConfig      = startingCometConfig
                        , _starConfig       = startingStarConfig
                        , _starSystemConfig = startingStarSystemConfig
                        , _bodyConfigs      = startingBodyConfigs
                        }

startingUniverseConfig :: UniverseConfig
startingUniverseConfig = UniverseConfig
  { _ucRealStarSystems               = False
  , _ucHomeworldMineralAccessibility = Range (Accessibility 0.5)
                                             (Accessibility 1.0)
  , _ucHomeworldMineralAmounts       = (Map.fromList
                                         [ (Duranium  , Range 200000 600000)
                                         , (Sorium    , Range 100000 300000)
                                         , (Neutronium, Range 50000 200000)
                                         , (Corbomite , Range 50000 200000)
                                         , (Tritanium , Range 50000 200000)
                                         , (Boronide  , Range 50000 200000)
                                         , (Uridium   , Range 50000 200000)
                                         , (Corundium , Range 50000 200000)
                                         , (Mercassium, Range 50000 200000)
                                         , (Vendarite , Range 50000 200000)
                                         , (Gallicite , Range 50000 200000)
                                         ]
                                       )
  }

startingPulseConfig :: PulseConfig
startingPulseConfig = PulseConfig { _pcBasePulse         = 60
                                  , _pcBodyPosition      = 86400
                                  , _pcShipyardWork      = 3600
                                  , _pcResearchWork      = 3600
                                  , _pcProductionWork    = 3600
                                  , _pcMiningWork        = 3600
                                  , _pcMassDriverWork    = 86400
                                  , _pcMineralPacketWork = 3600 -- TODO: Fix bug that requires this to be lower than _pcMassDriverWork.
                                  , _pcMissleWork        = 60
                                  , _pcShipOrder         = 60
                                  }

startingStarConfig :: StarConfig
startingStarConfig = StarConfig
  { _scStarMass   = Range (BodyMass 1e29) (BodyMass 1e33)
  , _scStarRadius = Range (BodyRadius 5e5) (BodyRadius 5e8)
  }

startingAsteroidConfig :: AsteroidConfig
startingAsteroidConfig = AsteroidConfig
  { _acAsteroidsPerBelt       = Range 50 200
  , _acGenerationChance       = GenerationChance 0.95
  , _acAsteroidBelts          = Range 0 3
  , _acBeltGenerationChance   = GenerationChance 0.2
  , _acAsteroidOrbitDeviation = Range (OrbitDeviation 0) (OrbitDeviation 0.1)
  }

startingCometConfig :: CometConfig
startingCometConfig = CometConfig { _ccComets           = Range 0 25
                                  , _ccGenerationChance = GenerationChance 0.65
                                  }

startingStarSystemConfig :: StarSystemConfig
startingStarSystemConfig = StarSystemConfig
  { _sscPlanetGenerationChance = GenerationChance 0.8
  , _sscNPRGenerationChance    = GenerationChance 0.2
  , _sscPlanets                = Range 0 25
  }

startingBodyConfigs :: BodyConfigs
startingBodyConfigs = Map.fromList
  [ (Asteroid   , startingAsteroidBodyConfig)
  , (Comet      , startingCometBodyConfig)
  , (DwarfPlanet, startingDwarfPlanetBodyConfig)
  , (GasDwarf   , startingGasDwarfBodyConfig)
  , (GasGiant   , startingGasGiantBodyConfig)
  , (IceGiant   , startingIceGiantBodyConfig)
  , (Moon       , startingMoonBodyConfig)
  , (Terrestrial, startingTerrestrialBodyConfig)
  ]

startingAsteroidBodyConfig :: BodyConfig
startingAsteroidBodyConfig = BodyConfig
  Asteroid
  (Range (BodyMass 1e15) (BodyMass 1e20))
  (Range (BodyRadius 1e2) (BodyRadius 1e3))
  (Range 1e7 1e10)
  (GenerationChance 0)
  0
  (MassRatio 0.4)
  (Map.fromList
    [ (Duranium  , Range 10000 100000)
    , (Sorium    , Range 5000 50000)
    , (Neutronium, Range 5000 50000)
    , (Corbomite , Range 5000 50000)
    , (Tritanium , Range 5000 50000)
    , (Boronide  , Range 5000 50000)
    , (Uridium   , Range 5000 50000)
    , (Corundium , Range 5000 50000)
    , (Mercassium, Range 5000 50000)
    , (Vendarite , Range 5000 50000)
    , (Gallicite , Range 5000 50000)
    ]
  )
  (Range 1.0 1.0)
  (GenerationChance 0.05)

startingCometBodyConfig :: BodyConfig
startingCometBodyConfig = BodyConfig
  Comet
  (Range (BodyMass 1e13) (BodyMass 1e15))
  (Range (BodyRadius 1e2) (BodyRadius 1e3))
  (Range 1e9 1e11)
  (GenerationChance 0)
  0
  (MassRatio 0.4)
  (Map.fromList
    [ (Duranium  , Range 50000 1000000)
    , (Sorium    , Range 20000 500000)
    , (Neutronium, Range 20000 500000)
    , (Corbomite , Range 20000 500000)
    , (Tritanium , Range 20000 500000)
    , (Boronide  , Range 20000 500000)
    , (Uridium   , Range 20000 500000)
    , (Corundium , Range 20000 500000)
    , (Mercassium, Range 20000 500000)
    , (Vendarite , Range 20000 500000)
    , (Gallicite , Range 20000 500000)
    ]
  )
  (Range 1.0 2.0)
  (GenerationChance 0.5)

startingDwarfPlanetBodyConfig :: BodyConfig
startingDwarfPlanetBodyConfig = BodyConfig
  DwarfPlanet
  (Range (BodyMass 2e20) (BodyMass 5e23))
  (Range (BodyRadius 5e2) (BodyRadius 5e3))
  (Range 1e7 1e10)
  (GenerationChance 0.1)
  1
  (MassRatio 0.4)
  (Map.fromList
    [ (Duranium  , Range 2000000 20000000)
    , (Sorium    , Range 1000000 10000000)
    , (Neutronium, Range 1000000 10000000)
    , (Corbomite , Range 1000000 10000000)
    , (Tritanium , Range 1000000 10000000)
    , (Boronide  , Range 1000000 10000000)
    , (Uridium   , Range 1000000 10000000)
    , (Corundium , Range 1000000 10000000)
    , (Mercassium, Range 1000000 10000000)
    , (Vendarite , Range 1000000 10000000)
    , (Gallicite , Range 1000000 10000000)
    ]
  )
  (Range 0.8 1.0)
  (GenerationChance 0.2)

startingGasDwarfBodyConfig :: BodyConfig
startingGasDwarfBodyConfig = BodyConfig
  GasDwarf
  (Range (BodyMass 6e24) (BodyMass 9e25))
  (Range (BodyRadius 5e3) (BodyRadius 5e4))
  (Range 1e7 1e10)
  (GenerationChance 0.5)
  8
  (MassRatio 0.4)
  (Map.fromList [(Sorium, Range 1000000 10000000)])
  (Range 0.8 1.0)
  (GenerationChance 0.3)

startingGasGiantBodyConfig :: BodyConfig
startingGasGiantBodyConfig = BodyConfig
  GasGiant
  (Range (BodyMass 9e25) (BodyMass 3e27))
  (Range (BodyRadius 5e4) (BodyRadius 5e5))
  (Range 1e7 1e10)
  (GenerationChance 0.8)
  20
  (MassRatio 0.4)
  (Map.fromList [(Sorium, Range 1000000 50000000)])
  (Range 0.2 1.0)
  (GenerationChance 0.3)

startingIceGiantBodyConfig :: BodyConfig
startingIceGiantBodyConfig = BodyConfig
  IceGiant
  (Range (BodyMass 3e25) (BodyMass 2e26))
  (Range (BodyRadius 3e4) (BodyRadius 3e5))
  (Range 1e7 1e10)
  (GenerationChance 0.65)
  15
  (MassRatio 0.4)
  (Map.fromList [(Sorium, Range 1000000 50000000)])
  (Range 0.2 1.0)
  (GenerationChance 0.3)

startingMoonBodyConfig :: BodyConfig
startingMoonBodyConfig = BodyConfig
  Moon
  (Range (BodyMass 1e16) (BodyMass 5e23))
  (Range (BodyRadius 1e3) (BodyRadius 5e3))
  (Range 1e5 5e6)
  (GenerationChance 0.0)
  0
  (MassRatio 0.4)
  (Map.fromList
    [ (Duranium  , Range 200000 10000000)
    , (Sorium    , Range 100000 5000000)
    , (Neutronium, Range 100000 5000000)
    , (Corbomite , Range 100000 5000000)
    , (Tritanium , Range 100000 5000000)
    , (Boronide  , Range 100000 5000000)
    , (Uridium   , Range 100000 5000000)
    , (Corundium , Range 100000 5000000)
    , (Mercassium, Range 100000 5000000)
    , (Vendarite , Range 100000 5000000)
    , (Gallicite , Range 100000 5000000)
    ]
  )
  (Range 0.3 1.0)
  (GenerationChance 0.2)

startingTerrestrialBodyConfig :: BodyConfig
startingTerrestrialBodyConfig = BodyConfig
  Terrestrial
  (Range (BodyMass 3e23) (BodyMass 3e25))
  (Range (BodyRadius 1e3) (BodyRadius 5e4))
  (Range 1e7 1e10)
  (GenerationChance 0.4)
  4
  (MassRatio 0.4)
  (Map.fromList
    [ (Duranium  , Range 2000000 100000000)
    , (Sorium    , Range 1000000 50000000)
    , (Neutronium, Range 1000000 50000000)
    , (Corbomite , Range 1000000 50000000)
    , (Tritanium , Range 1000000 50000000)
    , (Boronide  , Range 1000000 50000000)
    , (Uridium   , Range 1000000 50000000)
    , (Corundium , Range 1000000 50000000)
    , (Mercassium, Range 1000000 50000000)
    , (Vendarite , Range 1000000 50000000)
    , (Gallicite , Range 1000000 50000000)
    ]
  )
  (Range 0.2 1.0)
  (GenerationChance 0.2)
