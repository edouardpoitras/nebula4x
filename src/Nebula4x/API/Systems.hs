{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Nebula4x.API.Systems where

import           Control.Lens
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           GHC.Generics                   ( Generic )
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           Nebula4x.API.Server            ( WebRoute
                                                , readState
                                                , webM
                                                )
import           Network.HTTP.Types.Status      ( status404 )
import           Web.Scotty.Trans               ( get
                                                , json
                                                , param
                                                , status
                                                )

import           Nebula4x.Types          hiding ( status )
import           Nebula4x.Race
import           Nebula4x.Utils

data SystemItem = SystemItem
  { siId :: BodyId
  , siName :: BodyName
  , siDiscovered :: DiscoveryStatus
  } deriving (Show, Eq, Generic)

instance ToJSON SystemItem

instance FromJSON SystemItem

data BodyRace = BodyRace
  { bsId :: BodyId
  , bsName :: BodyName
  , bsRace :: Maybe RaceId
  } deriving (Show, Eq, Generic)

instance ToJSON BodyRace

instance FromJSON BodyRace

data SystemBodyResponse = SystemBodyResponse
  { sbrId :: BodyId
  , sbrBodyId :: BodyId
  , sbrBody :: Body
  } deriving (Show, Eq, Generic)

instance ToJSON SystemBodyResponse

instance FromJSON SystemBodyResponse

data SystemShipResponse = SystemShipResponse
  { ssrId :: BodyId
  , ssrShipId :: ShipId
  , ssrShip :: Ship
  } deriving (Show, Eq, Generic)

instance ToJSON SystemShipResponse

instance FromJSON SystemShipResponse

data SystemBody = SystemBody
  { sbId :: BodyId
  , sbDiscovered :: DiscoveryStatus
  , sbBodies :: [BodyRace]
  } deriving (Show, Eq, Generic)

instance ToJSON SystemBody

instance FromJSON SystemBody

data SystemBodies = SystemBodies
  { sbsId :: BodyId
  , sbsBodies :: Bodies
  } deriving (Show, Eq, Generic)

instance ToJSON SystemBodies

instance FromJSON SystemBodies

data SystemWormhole = SystemWormhole
  { swId :: BodyId
  , swName :: BodyName
  , swDiscovered :: DiscoveryStatus
  , swWormholes :: Wormholes
  } deriving (Show, Eq, Generic)

instance ToJSON SystemWormhole

instance FromJSON SystemWormhole

systemRoute :: WebRoute ()
systemRoute = do
  get "/api/systems" $ do
    gameState <- webM readState
    json $ view systems gameState
  get "/api/systems-discovered" $ do
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    json $ discoveredSystems raceId (view systems gameState)
  get "/api/systems-list" $ do
    gameState <- webM readState
    json $ systemItemsFromStarSystems (view systems gameState)
  get "/api/system-bodies/:systemId" $ do
    systemIdStr <- param "systemId"
    let systemId = read systemIdStr
    gameState <- webM readState
    json $ systemBodiesFromStarSystem
      (fromJust $ Map.lookup systemId (view systems gameState))
  get "/api/systems-bodies" $ do
    gameState <- webM readState
    json $ systemsBodiesFromStarSystems (view systems gameState)
  get "/api/system-body/:systemId/:bodyId" $ do
    systemIdStr <- param "systemId"
    bodyIdStr   <- param "bodyId"
    let systemId = read systemIdStr
    let bodyId   = read bodyIdStr
    gameState <- webM readState
    json $ systemBodyResponseFromStarSystem
      bodyId
      (view (systems . at' systemId) gameState)
  get "/api/systems-wormholes" $ do
    gameState <- webM readState
    json $ systemWormholesFromStarSystems (view systems gameState)
  get "/api/systems/:systemId" $ do
    systemIdStr <- param "systemId"
    let systemId = read systemIdStr
    gameState <- webM readState
    let systms = view systems gameState
    let system = Map.lookup systemId systms
    case system of
      Just sys -> json sys
      Nothing  -> status status404
  get "/api/systems/:systemId/ship/:shipId" $ do
    systemIdStr <- param "systemId"
    shipIdStr   <- param "shipId"
    let systemId = read systemIdStr
    let shipId   = read shipIdStr
    gameState <- webM readState
    json $ systemShipResponseFromStarSystem
      shipId
      (view (systems . at' systemId) gameState)

systemShipResponseFromStarSystem :: ShipId -> StarSystem -> SystemShipResponse
systemShipResponseFromStarSystem sid ss = SystemShipResponse
  (view ssId ss)
  sid
  (fromJust $ Map.lookup sid (view ssShips ss))

discoveredSystems :: RaceId -> StarSystems -> StarSystems
discoveredSystems raceId sss = discoveredStarSystems where
  discoveredStarSystems = Map.filter discovered sss
  discovered ss = if Map.member raceId (view ssDiscovered ss)
    then fromJust (Map.lookup raceId (view ssDiscovered ss))
    else False

systemBodyResponseFromStarSystem :: BodyId -> StarSystem -> SystemBodyResponse
systemBodyResponseFromStarSystem bid ss = sysBodyResp where
  sysBodyResp = SystemBodyResponse ssid bid bdy
  ssid        = view ssId ss
  bodies      = view ssBodies ss
  bdy         = fromJust $ Map.lookup bid bodies

systemBodiesFromStarSystem :: StarSystem -> SystemBodies
systemBodiesFromStarSystem ss = SystemBodies (view ssId ss) (view ssBodies ss)

systemsBodiesFromStarSystems :: StarSystems -> [SystemBody]
systemsBodiesFromStarSystems = Map.elems . Map.map systemBodyFromStarSystem

systemBodyFromStarSystem :: StarSystem -> SystemBody
systemBodyFromStarSystem ss = SystemBody
  (view ssId ss)
  (view ssDiscovered ss)
  (bodyRacesFromBodies (view ssBodies ss))

bodyRacesFromBodies :: Bodies -> [BodyRace]
bodyRacesFromBodies = Map.elems . Map.map bodyRaceFromBody

bodyRaceFromBody :: Body -> BodyRace
bodyRaceFromBody bdy =
  BodyRace (view bId bdy) (view bName bdy) (view bRace bdy)

systemItemFromStarSystem :: StarSystem -> SystemItem
systemItemFromStarSystem ss =
  SystemItem (view ssId ss) (view (ssStar . starName) ss) (view ssDiscovered ss)

systemItemsFromStarSystems :: StarSystems -> [SystemItem]
systemItemsFromStarSystems = Map.elems . Map.map systemItemFromStarSystem

systemWormholeFromStarSystem :: StarSystem -> SystemWormhole
systemWormholeFromStarSystem ss = SystemWormhole
  (view ssId ss)
  (view (ssStar . starName) ss)
  (view ssDiscovered ss)
  (view ssWormholes ss)

systemWormholesFromStarSystems :: StarSystems -> [SystemWormhole]
systemWormholesFromStarSystems =
  Map.elems . Map.map systemWormholeFromStarSystem
