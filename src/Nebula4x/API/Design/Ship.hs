{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Nebula4x.API.Design.Ship where

import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           GHC.Generics                   ( Generic )
import           Network.HTTP.Types.Status      ( status404 )
import           System.Random
import           Web.Scotty.Trans               ( delete
                                                , get
                                                , json
                                                , jsonData
                                                , param
                                                , post
                                                , status
                                                )

import           Nebula4x.API.Server            ( WebRoute
                                                , modify
                                                , readState
                                                , webM
                                                )
import           Nebula4x.Config
import           Nebula4x.Race
import           Nebula4x.Ship
import           Nebula4x.Types          hiding ( status )
import           Nebula4x.Utils

data ShipDesignResponse = ShipDesignResponse
  { shipDesignRaceId :: ComponentId
  , shipDesign :: ShipDesign
  } deriving (Show, Eq, Generic)

instance ToJSON ShipDesignResponse

instance FromJSON ShipDesignResponse

data ShipDesignsResponse = ShipDesignsResponse
  { shipDesignsRaceId :: ComponentId
  , shipDesigns :: ResearchStatus ShipDesign
  } deriving (Show, Eq, Generic)

instance ToJSON ShipDesignsResponse

instance FromJSON ShipDesignsResponse

data CheckShipDesignRequest = CheckShipDesignRequest
  { checkShipDesignEngines         :: [ComponentId]
  , checkShipDesignMissleLaunchers :: [ComponentId]
  , checkShipDesignLasers          :: [ComponentId]
  , checkShipDesignArmor          :: [ComponentId]
  , checkShipDesignShields        :: [ComponentId]
  , checkShipDesignFuelStorages   :: [ComponentId]
  , checkShipDesignCargoHandlings :: [ComponentId]
  , checkShipDesignCargoHolds     :: [ComponentId]
  , checkShipDesignJumpGates      :: [ComponentId]
  , checkShipDesignGeoSensors     :: [ComponentId]
  , checkShipDesignGravSensors    :: [ComponentId]
  } deriving (Show, Eq, Generic)

instance ToJSON CheckShipDesignRequest

instance FromJSON CheckShipDesignRequest

data CreateShipDesignRequest = CreateShipDesighRequest
  { shipDesignEngines        :: [ComponentId]
  , shipDesignMissleLaunchers :: [ComponentId]
  , shipDesignLasers         :: [ComponentId]
  , shipDesignArmor          :: [ComponentId]
  , shipDesignShields        :: [ComponentId]
  , shipDesignFuelStorages   :: [ComponentId]
  , shipDesignCargoHandlings :: [ComponentId]
  , shipDesignCargoHolds     :: [ComponentId]
  , shipDesignJumpGates      :: [ComponentId]
  , shipDesignGeoSensors     :: [ComponentId]
  , shipDesignGravSensors    :: [ComponentId]
  , shipDesignName           :: String
  } deriving (Show, Eq, Generic)

instance ToJSON CreateShipDesignRequest

instance FromJSON CreateShipDesignRequest

data CreateShipResponse = CreateShipResponse
  { createdShipDesignRaceId :: ComponentId
  , createdShipDesign :: ShipDesign
  } deriving (Show, Eq, Generic)

instance ToJSON CreateShipResponse

instance FromJSON CreateShipResponse

newtype DeleteShipDesignRequest = DeleteShipDesignRequest
  { shipDesignId :: ComponentId
  } deriving (Show, Eq, Generic)

instance ToJSON DeleteShipDesignRequest

instance FromJSON DeleteShipDesignRequest

data DeleteShipDesignResponse = DeleteShipDesignResponse
  { deletedShipDesignId :: ComponentId
  , deletedShipDesignRaceId :: ComponentId
  } deriving (Show, Eq, Generic)

instance ToJSON DeleteShipDesignResponse

instance FromJSON DeleteShipDesignResponse

shipDesignRoute :: WebRoute ()
shipDesignRoute = do
  get "/api/design/ship" $ do
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    json
      $ (ShipDesignsResponse
          raceId
          (view (research . at' raceId . rShipDesigns) gameState)
        )
  get "/api/design/ship/:shipDesignId" $ do
    shipDesignIdStr <- param "shipDesignId"
    let shipDesignId' = read shipDesignIdStr
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    let shipDesigns' =
          view (research . at' raceId . rShipDesigns . unlocked) gameState
    let shipDesign' = Map.lookup shipDesignId' shipDesigns'
    case shipDesign' of
      Just sd -> json (ShipDesignResponse raceId sd)
      Nothing -> status status404
  post "/api/check/design/ship" $ do
    checkShipRequest <- jsonData
    let engineIDs         = checkShipDesignEngines checkShipRequest
    let missleLauncherIDs = checkShipDesignMissleLaunchers checkShipRequest
    let laserIDs          = checkShipDesignLasers checkShipRequest
    let armorIDs          = checkShipDesignArmor checkShipRequest
    let shieldIDs         = checkShipDesignShields checkShipRequest
    let fuelStorageIDs    = checkShipDesignFuelStorages checkShipRequest
    let cargoHandlingIDs  = checkShipDesignCargoHandlings checkShipRequest
    let cargoHoldIDs      = checkShipDesignCargoHolds checkShipRequest
    let jumpGateIDs       = checkShipDesignJumpGates checkShipRequest
    let geoSensorIDs      = checkShipDesignGeoSensors checkShipRequest
    let gravSensorIDs     = checkShipDesignGravSensors checkShipRequest
    gen <- liftIO getStdGen
    let (shipName, newGen) = generateShipName gen
    liftIO $ setStdGen newGen
    gameState <- webM readState
    json $ getShipDesign 0
                         shipName
                         engineIDs
                         missleLauncherIDs
                         laserIDs
                         armorIDs
                         shieldIDs
                         fuelStorageIDs
                         cargoHandlingIDs
                         cargoHoldIDs
                         jumpGateIDs
                         geoSensorIDs
                         gravSensorIDs
                         gameState
  post "/api/design/ship" $ do
    shipDesignRequest <- jsonData
    let engineIDs         = shipDesignEngines shipDesignRequest
    let missleLauncherIDs = shipDesignMissleLaunchers shipDesignRequest
    let laserIDs          = shipDesignLasers shipDesignRequest
    let armorIDs          = shipDesignArmor shipDesignRequest
    let shieldIDs         = shipDesignShields shipDesignRequest
    let fuelStorageIDs    = shipDesignFuelStorages shipDesignRequest
    let cargoHandlingIDs  = shipDesignCargoHandlings shipDesignRequest
    let cargoHoldIDs      = shipDesignCargoHolds shipDesignRequest
    let jumpGateIDs       = shipDesignJumpGates shipDesignRequest
    let geoSensorIDs      = shipDesignGeoSensors shipDesignRequest
    let gravSensorIDs     = shipDesignGravSensors shipDesignRequest
    let name              = shipDesignName shipDesignRequest
    gameState <- webM readState
    let gen          = read (view randomSeed gameState) :: StdGen
    let (sdid, gen') = randomR (minId, maxId) gen
    let shipDesign' = getShipDesign sdid
                                    name
                                    engineIDs
                                    missleLauncherIDs
                                    laserIDs
                                    armorIDs
                                    shieldIDs
                                    fuelStorageIDs
                                    cargoHandlingIDs
                                    cargoHoldIDs
                                    jumpGateIDs
                                    geoSensorIDs
                                    gravSensorIDs
                                    gameState
    let raceId = getPlayerRaceId gameState
    webM
      $ modify (addShipDesign shipDesign' raceId . set randomSeed (show gen'))
    json $ CreateShipResponse raceId shipDesign'
  delete "/api/design/ship" $ do
    deleteShipRequest <- jsonData
    let designId = shipDesignId deleteShipRequest
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    webM $ modify (removeShipDesign designId raceId)
    json $ DeleteShipDesignResponse raceId designId

getShipDesign
  :: ShipDesignId
  -> String
  -> [ComponentId]
  -> [ComponentId]
  -> [ComponentId]
  -> [ComponentId]
  -> [ComponentId]
  -> [ComponentId]
  -> [ComponentId]
  -> [ComponentId]
  -> [ComponentId]
  -> [ComponentId]
  -> [ComponentId]
  -> GameState
  -> ShipDesign
getShipDesign sdid shipName engineIDs missleLauncherIDs laserIDs armorIDs shieldIDs fuelStorageIDs cargoHandlingIDs cargoHoldIDs jumpGateIDs geoSensorIDs gravSensorIDs gameState
  = shipDesign'
 where
  raceId = getPlayerRaceId gameState
  engineMap =
    view (research . at' raceId . rEngineDesigns . unlocked) gameState
  missleLauncherMap =
    view (research . at' raceId . rMissleLauncherDesigns . unlocked) gameState
  laserMap = view (research . at' raceId . rLaserDesigns . unlocked) gameState
  armorMap = view (research . at' raceId . rArmor . unlocked) gameState
  maybeShipArmor     = map (`Map.lookup` armorMap) armorIDs
  shipArmor'         = map (view arArmor . fromJust) maybeShipArmor
  shieldMap = view (research . at' raceId . rShield . unlocked) gameState
  maybeShipShields   = map (`Map.lookup` shieldMap) shieldIDs
  shipShields'       = map (view shrShield . fromJust) maybeShipShields
  maybeEngineDesigns = map (`Map.lookup` engineMap) engineIDs
  engineDesigns      = map fromJust maybeEngineDesigns
  engs               = map (view edEngine) engineDesigns
  maybeMissleLauncherDesigns =
    map (`Map.lookup` missleLauncherMap) missleLauncherIDs
  missleLauncherDesigns = map fromJust maybeMissleLauncherDesigns
  missleLaunchers       = map (view mldMissleLauncher) missleLauncherDesigns
  maybeLaserDesigns     = map (`Map.lookup` laserMap) laserIDs
  laserDesigns          = map fromJust maybeLaserDesigns
  lasers                = map (view ldLaser) laserDesigns
  fuelStorageMap =
    view (research . at' raceId . rFuelStorage . unlocked) gameState
  maybeFuelStorages = map (`Map.lookup` fuelStorageMap) fuelStorageIDs
  fuelStorages      = map (view fsrFuelStorage . fromJust) maybeFuelStorages
  cargoHandlingMap =
    view (research . at' raceId . rCargoHandling . unlocked) gameState
  maybeCargoHandlings = map (`Map.lookup` cargoHandlingMap) cargoHandlingIDs
  cargoHandlings = map (view chsrCargoHandling . fromJust) maybeCargoHandlings
  cargoHoldMap = view (research . at' raceId . rCargoHold . unlocked) gameState
  maybeCargoHolds = map (`Map.lookup` cargoHoldMap) cargoHoldIDs
  cargoHolds = map (view chrCargoHold . fromJust) maybeCargoHolds
  jumpGateMap = view (research . at' raceId . rJumpGate . unlocked) gameState
  maybeJumpGates = map (`Map.lookup` jumpGateMap) jumpGateIDs
  jumpGates = map (view jgrJumpGate . fromJust) maybeJumpGates
  geoSensorMap =
    view (research . at' raceId . rGeologicalSensor . unlocked) gameState
  maybeGeos = map (`Map.lookup` geoSensorMap) geoSensorIDs
  geos      = map (view srSensor . fromJust) maybeGeos
  gravSensorMap =
    view (research . at' raceId . rGravitationalSensor . unlocked) gameState
  maybeGravs  = map (`Map.lookup` gravSensorMap) gravSensorIDs
  gravs       = map (view srSensor . fromJust) maybeGravs
  shipDesign' = newShipDesign sdid
                              (ShipName shipName)
                              shipArmor'
                              shipShields'
                              engs
                              missleLaunchers
                              lasers
                              fuelStorages
                              cargoHandlings
                              cargoHolds
                              jumpGates
                              geos
                              gravs

addShipDesign :: ShipDesign -> ComponentId -> GameState -> GameState
addShipDesign sd raceId gs = newGameState
 where
  newGameState = over (research . at' raceId . rShipDesigns . unlocked)
                      (Map.insert shipDesignId' sd)
                      gs
  shipDesignId' = view sdId sd

removeShipDesign :: ShipDesignId -> ComponentId -> GameState -> GameState
removeShipDesign sdid raceId =
  over (research . at' raceId . rShipDesigns . unlocked) (Map.delete sdid)
