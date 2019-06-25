{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nebula4x.API.Shipyard where

import           Control.Lens
import qualified Control.Newtype.Generics      as NT
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Map.Strict               as Map
import           Data.Maybe
import           GHC.Generics                   ( Generic )
import           Network.HTTP.Types.Status      ( mkStatus )
import           Web.Scotty.Trans               ( get
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
import           Nebula4x.Race
import           Nebula4x.Shipyard
import           Nebula4x.Time                  ( yearInSeconds )
import           Nebula4x.Types          hiding ( status )
import           Nebula4x.Utils

data CheckBuildRequest = CheckBuildRequest
  { checkBuildSystem   :: StarSystemId
  , checkBuildBody     :: BodyId
  , checkBuildShipyard :: ShipyardId
  } deriving (Show, Eq, Generic)

instance ToJSON CheckBuildRequest

instance FromJSON CheckBuildRequest

data CheckBuildResponse = CheckBuildResponse
  { checkBuildCost :: MineralCost
  , checkBuildRate :: TaskProgress
  } deriving (Show, Eq, Generic)

instance ToJSON CheckBuildResponse

instance FromJSON CheckBuildResponse

data CreateBuildRequest = CreateBuildRequest
  { createBuildSystem   :: StarSystemId
  , createBuildBody     :: BodyId
  , createBuildShipyard :: ShipyardId
  , createBuildShipName :: String
  } deriving (Show, Eq, Generic)

instance ToJSON CreateBuildRequest

instance FromJSON CreateBuildRequest

data CheckRetoolRequest = CheckRetoolRequest
  { checkRetoolSystem     :: StarSystemId
  , checkRetoolBody       :: BodyId
  , checkRetoolShipyard   :: ShipyardId
  , checkRetoolShipDesign :: ShipDesignId
  } deriving (Show, Eq, Generic)

instance ToJSON CheckRetoolRequest

instance FromJSON CheckRetoolRequest

data CreateRetoolRequest = CreateRetoolRequest
  { retoolSystem     :: StarSystemId
  , retoolBody       :: BodyId
  , retoolShipyard   :: ShipyardId
  , retoolShipDesign :: ShipDesignId
  } deriving (Show, Eq, Generic)

instance ToJSON CreateRetoolRequest

instance FromJSON CreateRetoolRequest

data CheckCapacityRequest = CheckCapacityRequest
  { checkCapacitySystem   :: StarSystemId
  , checkCapacityBody     :: BodyId
  , checkCapacityShipyard :: ShipyardId
  , checkCapacityAmount   :: CapacityAmount
  } deriving (Show, Eq, Generic)

instance ToJSON CheckCapacityRequest

instance FromJSON CheckCapacityRequest

data CreateCapacityRequest = CreateCapacityRequest
  { capacitySystem   :: StarSystemId
  , capacityBody     :: BodyId
  , capacityShipyard :: ShipyardId
  , capacityAmount   :: CapacityAmount
  } deriving (Show, Eq, Generic)

instance ToJSON CreateCapacityRequest

instance FromJSON CreateCapacityRequest

data CheckSlipwayRequest = CheckSlipwayRequest
  { checkSlipwaySystem   :: StarSystemId
  , checkSlipwayBody     :: BodyId
  , checkSlipwayShipyard :: ShipyardId
  } deriving (Show, Eq, Generic)

instance ToJSON CheckSlipwayRequest

instance FromJSON CheckSlipwayRequest

data CreateSlipwayRequest = CreateSlipwayRequest
  { slipwaySystem   :: StarSystemId
  , slipwayBody     :: BodyId
  , slipwayShipyard :: ShipyardId
  } deriving (Show, Eq, Generic)

instance ToJSON CreateSlipwayRequest

instance FromJSON CreateSlipwayRequest

shipyardRoute :: WebRoute ()
shipyardRoute = do
  get "/api/shipyard" $
        -- TODO
                        do
    gameState <- webM readState
    json gameState
  get "/api/shipyard/:systemId" $
        -- TODO
                                  do
    gameState <- webM readState
    json gameState
  get "/api/shipyard/:systemId/:bodyId" $ do
    systemId <- param "systemId"
    bodyId   <- param "bodyId"
    let sid = read systemId
    let bid = read bodyId
    gameState <- webM readState
    let starSystems = view systems gameState
    let system      = fromJust $ Map.lookup sid starSystems
    let body        = fromJust $ Map.lookup bid (view ssBodies system)
    json (view bShipyards body)
  get "/api/shipyard/:systemId/:bodyId/:shipyardId" $ do
    systemId   <- param "systemId"
    bodyId     <- param "bodyId"
    shipyardId <- param "shipyardId"
    let sid  = read systemId
    let bid  = read bodyId
    let syid = read shipyardId
    gameState <- webM readState
    let starSystems = view systems gameState
    let system      = fromJust $ Map.lookup sid starSystems
    let body        = fromJust $ Map.lookup bid (view ssBodies system)
    let shipyards   = view bShipyards body
    let shipyard    = fromJust $ Map.lookup syid shipyards
    json shipyard
  post "/api/check/shipyard/build" $ do
    checkBuildRequest <- jsonData
    let systemId   = checkBuildSystem checkBuildRequest
    let bodyId     = checkBuildBody checkBuildRequest
    let shipyardId = checkBuildShipyard checkBuildRequest
    gameState <- webM readState
    let starSystems       = view systems gameState
    let system            = fromJust $ Map.lookup systemId starSystems
    let body = fromJust $ Map.lookup bodyId (view ssBodies system)
    let shipyards         = view bShipyards body
    let shipyard          = fromJust $ Map.lookup shipyardId shipyards
    let shipDesign        = fromJust $ view syShipDesign shipyard
    let buildCost         = view sdBuildCost shipDesign
    let shipyardBuildRate = view syBuildRate shipyard
    let buildRate = NT.unpack shipyardBuildRate / fromIntegral yearInSeconds
    -- The larger the ship, the faster the build rate.
    -- Else we'd have to wait decades on bigger ships.
    let adjustedBR = adjustedBuildRate buildRate
                                       (view sdSize shipDesign)
                                       (view sdClass shipDesign)
    let shipBuildCost  = view sdBuildCost shipDesign
    let totalBuildCost = Map.foldr' (+) 0.0 shipBuildCost
    let progressRate   = adjustedBR / totalBuildCost * 100
    json $ CheckBuildResponse buildCost (TaskProgress progressRate)
  post "/api/shipyard/build" $ do
    createBuildRequest <- jsonData
    let systemId   = createBuildSystem createBuildRequest
    let bodyId     = createBuildBody createBuildRequest
    let shipyardId = createBuildShipyard createBuildRequest
    let shipName   = createBuildShipName createBuildRequest
    webM $ modify
      (addSystemShipyardBuild systemId bodyId shipyardId (ShipName shipName))
    json True
  post "/api/check/shipyard/retool" $ do
    checkRetoolRequest <- jsonData
    let systemId     = checkRetoolSystem checkRetoolRequest
    let bodyId       = checkRetoolBody checkRetoolRequest
    let shipyardId   = checkRetoolShipyard checkRetoolRequest
    let shipDesignId = checkRetoolShipDesign checkRetoolRequest
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    let shipDesigns =
          view (research . at' raceId . rShipDesigns . unlocked) gameState
    let shipDesign  = fromJust $ Map.lookup shipDesignId shipDesigns
    let starSystems = view systems gameState
    let system      = fromJust $ Map.lookup systemId starSystems
    let body = fromJust $ Map.lookup bodyId (view ssBodies system)
    let shipyards   = view bShipyards body
    let shipyard    = fromJust $ Map.lookup shipyardId shipyards
    if canRetoolToDesign shipDesign shipyard
      then json $ getShipyardRetoolTask shipDesign shipyard
      else status $ mkStatus
        403
        "Can not retool shipyard - ensure sufficient capacity, matching type, and empty slipways"
  post "/api/shipyard/retool" $ do
    retoolRequest <- jsonData
    let systemId     = retoolSystem retoolRequest
    let bodyId       = retoolBody retoolRequest
    let shipyardId   = retoolShipyard retoolRequest
    let shipDesignId = retoolShipDesign retoolRequest
    webM $ modify
      (setSystemShipyardRetoolTask systemId bodyId shipyardId shipDesignId)
    json True
  post "/api/check/shipyard/capacity" $ do
    checkCapacityRequest <- jsonData
    let systemId   = checkCapacitySystem checkCapacityRequest
    let bodyId     = checkCapacityBody checkCapacityRequest
    let shipyardId = checkCapacityShipyard checkCapacityRequest
    let capAmount  = checkCapacityAmount checkCapacityRequest
    gameState <- webM readState
    let starSystems = view systems gameState
    let system      = fromJust $ Map.lookup systemId starSystems
    let body = fromJust $ Map.lookup bodyId (view ssBodies system)
    let shipyards   = view bShipyards body
    let shipyard    = fromJust $ Map.lookup shipyardId shipyards
    json $ getShipyardCapacityTask capAmount shipyard
  post "/api/shipyard/capacity" $ do
    capacityRequest <- jsonData
    let systemId   = capacitySystem capacityRequest
    let bodyId     = capacityBody capacityRequest
    let shipyardId = capacityShipyard capacityRequest
    let capAmount  = capacityAmount capacityRequest
    webM $ modify
      (setSystemShipyardCapacityTask systemId bodyId shipyardId capAmount)
    json True
  post "/api/check/shipyard/slipway" $ do
    checkSlipwayRequest <- jsonData
    let systemId   = checkSlipwaySystem checkSlipwayRequest
    let bodyId     = checkSlipwayBody checkSlipwayRequest
    let shipyardId = checkSlipwayShipyard checkSlipwayRequest
    gameState <- webM readState
    let starSystems = view systems gameState
    let system      = fromJust $ Map.lookup systemId starSystems
    let body = fromJust $ Map.lookup bodyId (view ssBodies system)
    let shipyards   = view bShipyards body
    let shipyard    = fromJust $ Map.lookup shipyardId shipyards
    json $ getShipyardSlipwayTask shipyard
  post "/api/shipyard/slipway" $ do
    slipwayRequest <- jsonData
    let systemId   = slipwaySystem slipwayRequest
    let bodyId     = slipwayBody slipwayRequest
    let shipyardId = slipwayShipyard slipwayRequest
    webM $ modify (setSystemShipyardSlipwayTask systemId bodyId shipyardId)
    json True

-- TODO: Desperately need to make better use of Data.Map.Lens here...
addSystemShipyardBuild
  :: StarSystemId -> BodyId -> ShipyardId -> ShipName -> GameState -> GameState
addSystemShipyardBuild systemId bodyId shipyardId shipName gameState =
  newGameState
 where
  starSystems  = view systems gameState
  system       = fromJust $ Map.lookup systemId starSystems
  bodies       = view ssBodies system
  body         = fromJust $ Map.lookup bodyId bodies
  shipyards    = view bShipyards body
  shipyard     = fromJust $ Map.lookup shipyardId shipyards
  newShipyard' = addShipyardBuild shipName shipyard
  newShipyards = Map.insert (view syId shipyard) newShipyard' shipyards
  newBody      = set bShipyards newShipyards body
  newBodies    = Map.insert (view bId body) newBody bodies
  newSystem    = set ssBodies newBodies system
  newSystems   = Map.insert (view ssId system) newSystem starSystems
  newGameState = set systems newSystems gameState

-- TODO: Desperately need to make better use of Data.Map.Lens here...
setSystemShipyardRetoolTask
  :: StarSystemId
  -> BodyId
  -> ShipyardId
  -> ShipDesignId
  -> GameState
  -> GameState
setSystemShipyardRetoolTask systemId bodyId shipyardId shipDesignId gameState =
  newGameState
 where
  starSystems = view systems gameState
  system      = fromJust $ Map.lookup systemId starSystems
  bodies      = view ssBodies system
  body        = fromJust $ Map.lookup bodyId bodies
  shipyards   = view bShipyards body
  shipyard    = fromJust $ Map.lookup shipyardId shipyards
  raceId      = getPlayerRaceId gameState
  shipDesigns =
    view (research . at' raceId . rShipDesigns . unlocked) gameState
  shipDesign = fromJust $ Map.lookup shipDesignId shipDesigns
  newGameState =
    if canRetoolToDesign shipDesign shipyard then addedTask else gameState
  addedTask    = set systems newSystems gameState
  newSystems   = Map.insert (view ssId system) newSystem starSystems
  newSystem    = set ssBodies newBodies system
  newBodies    = Map.insert (view bId body) newBody bodies
  newBody      = set bShipyards newShipyards body
  newShipyards = Map.insert (view syId shipyard) newShipyard' shipyards
  newShipyard' = beginRetool shipDesign shipyard

-- TODO: Desperately need to make better use of Data.Map.Lens here...
setSystemShipyardCapacityTask
  :: StarSystemId
  -> BodyId
  -> ShipyardId
  -> CapacityAmount
  -> GameState
  -> GameState
setSystemShipyardCapacityTask systemId bodyId shipyardId capAmount gameState =
  newGameState
 where
  starSystems  = view systems gameState
  system       = fromJust $ Map.lookup systemId starSystems
  bodies       = view ssBodies system
  body         = fromJust $ Map.lookup bodyId bodies
  shipyards    = view bShipyards body
  shipyard     = fromJust $ Map.lookup shipyardId shipyards
  newGameState = set systems newSystems gameState
  newSystems   = Map.insert (view ssId system) newSystem starSystems
  newSystem    = set ssBodies newBodies system
  newBodies    = Map.insert (view bId body) newBody bodies
  newBody      = set bShipyards newShipyards body
  newShipyards = Map.insert (view syId shipyard) newShipyard' shipyards
  newShipyard' = beginCapacityExpand capAmount shipyard

-- TODO: Desperately need to make better use of Data.Map.Lens here...
setSystemShipyardSlipwayTask
  :: StarSystemId -> BodyId -> ShipyardId -> GameState -> GameState
setSystemShipyardSlipwayTask systemId bodyId shipyardId gameState =
  newGameState
 where
  starSystems  = view systems gameState
  system       = fromJust $ Map.lookup systemId starSystems
  bodies       = view ssBodies system
  body         = fromJust $ Map.lookup bodyId bodies
  shipyards    = view bShipyards body
  shipyard     = fromJust $ Map.lookup shipyardId shipyards
  newGameState = set systems newSystems gameState
  newSystems   = Map.insert (view ssId system) newSystem starSystems
  newSystem    = set ssBodies newBodies system
  newBodies    = Map.insert (view bId body) newBody bodies
  newBody      = set bShipyards newShipyards body
  newShipyards = Map.insert (view syId shipyard) newShipyard' shipyards
  newShipyard' = beginAddSlipway shipyard
