{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Nebula4x.API.Ship.Order where

import           Control.Lens
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Maybe
import           GHC.Generics                   ( Generic )
import           Nebula4x.API.Server            ( WebRoute
                                                , modify
                                                , readState
                                                , webM
                                                )
import           Web.Scotty.Trans               ( delete
                                                , json
                                                , jsonData
                                                , post
                                                )

import           Nebula4x.Ship
import           Nebula4x.Types
import           Nebula4x.Utils                 ( at' )

data CreateShipOrder = CreateShipOrder
  { createShipOrderSystemId :: StarSystemId
  , createShipOrderShipId   :: ShipId
  , createShipOrderGroupId  :: Maybe Int
  , createShipOrder         :: ShipOrder
  } deriving (Show, Eq, Generic)

instance ToJSON CreateShipOrder

instance FromJSON CreateShipOrder

data CreateShipOrderResponse = CreateShipOrderResponse
  { createShipOrderResponseSystemId :: StarSystemId
  , createShipOrderResponseShipId   :: ShipId
  , createShipOrderResponseShip     :: Ship
  } deriving (Show, Eq, Generic)

instance ToJSON CreateShipOrderResponse

instance FromJSON CreateShipOrderResponse

data CreateShipConditionalOrder = CreateShipConditionalOrder
  { createShipConditionalOrderSystemId  :: StarSystemId
  , createShipConditionalOrderShipId    :: ShipId
  , createShipConditionalOrderCondition :: OrderCondition
  , createShipConditionalOrder          :: ShipOrder
  } deriving (Show, Eq, Generic)

instance ToJSON CreateShipConditionalOrder

instance FromJSON CreateShipConditionalOrder

data UpdateShipOrder = UpdateShipOrder
  { updateShipOrderSystemId :: StarSystemId
  , updateShipOrderShipId   :: ShipId
  , updateShipOrderGroupId  :: Int
  , updateShipOrderRepeat   :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON UpdateShipOrder

instance FromJSON UpdateShipOrder

data UpdateShipOrderResponse = UpdateShipOrderResponse
  { updateShipOrderResponseSystemId :: StarSystemId
  , updateShipOrderResponseShipId   :: ShipId
  , updateShipOrderResponseShip     :: Ship
  } deriving (Show, Eq, Generic)

instance ToJSON UpdateShipOrderResponse

instance FromJSON UpdateShipOrderResponse

data DeleteShipOrder = DeleteShipOrder
  { deleteShipOrderSystemId :: StarSystemId
  , deleteShipOrderShipId   :: ShipId
  , deleteShipOrderGroupId  :: Int
  , deleteShipOrderId       :: Maybe Int
  } deriving (Show, Eq, Generic)

instance ToJSON DeleteShipOrder

instance FromJSON DeleteShipOrder

data DeleteShipOrderResponse = DeleteShipOrderResponse
  { deleteShipOrderResponseSystemId :: StarSystemId
  , deleteShipOrderResponseShipId   :: ShipId
  , deleteShipOrderResponseShip     :: Ship
  } deriving (Show, Eq, Generic)

instance ToJSON DeleteShipOrderResponse

instance FromJSON DeleteShipOrderResponse

data DeleteShipConditionalOrder = DeleteShipConditionalOrder
  { deleteShipConditionalOrderSystemId :: StarSystemId
  , deleteShipConditionalOrderShipId   :: ShipId
  , deleteShipConditionalOrderId  :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON DeleteShipConditionalOrder

instance FromJSON DeleteShipConditionalOrder

shipOrderRoute :: WebRoute ()
shipOrderRoute = do
  post "/api/ship/order" $ do
    shipOrder <- jsonData
    let systemId     = createShipOrderSystemId shipOrder
    let shipId       = createShipOrderShipId shipOrder
    let maybeGroupId = createShipOrderGroupId shipOrder
    let newOrder     = createShipOrder shipOrder
    webM $ modify (addOrder systemId shipId maybeGroupId newOrder)
    gameState <- webM readState
    json $ CreateShipOrderResponse
      systemId
      shipId
      (view (systems . at' systemId . ssShips . at' shipId) gameState)
  post "/api/ship/conditional-order" $ do
    shipConditionalOrder <- jsonData
    let systemId     = createShipConditionalOrderSystemId shipConditionalOrder
    let shipId       = createShipConditionalOrderShipId shipConditionalOrder
    let newCondition = createShipConditionalOrderCondition shipConditionalOrder
    let newOrder     = createShipConditionalOrder shipConditionalOrder
    webM $ modify (addConditionalOrder systemId shipId newCondition newOrder)
    gameState <- webM readState
    json $ CreateShipOrderResponse
      systemId
      shipId
      (view (systems . at' systemId . ssShips . at' shipId) gameState)
  post "/api/ship/order/update" $ do
    shipOrder <- jsonData
    let systemId  = updateShipOrderSystemId shipOrder
    let shipId    = updateShipOrderShipId shipOrder
    let groupId   = updateShipOrderGroupId shipOrder
    let newRepeat = updateShipOrderRepeat shipOrder
    webM $ modify (updateOrder systemId shipId groupId newRepeat)
    gameState <- webM readState
    json $ UpdateShipOrderResponse
      systemId
      shipId
      (view (systems . at' systemId . ssShips . at' shipId) gameState)
  delete "/api/ship/order" $ do
    deleteShipOrder <- jsonData
    let systemId     = deleteShipOrderSystemId deleteShipOrder
    let shipId       = deleteShipOrderShipId deleteShipOrder
    let groupId      = deleteShipOrderGroupId deleteShipOrder
    let maybeOrderId = deleteShipOrderId deleteShipOrder
    webM $ modify (removeOrder systemId shipId groupId maybeOrderId)
    newGameState <- webM readState
    let newShp =
          view (systems . at' systemId . ssShips . at' shipId) newGameState
    json $ DeleteShipOrderResponse systemId shipId newShp
  delete "/api/ship/conditional-order" $ do
    deleteShipConditionalOrder <- jsonData
    let systemId =
          deleteShipConditionalOrderSystemId deleteShipConditionalOrder
    let shipId = deleteShipConditionalOrderShipId deleteShipConditionalOrder
    let conditionalOrderId =
          deleteShipConditionalOrderId deleteShipConditionalOrder
    webM $ modify (removeConditionalOrder systemId shipId conditionalOrderId)
    newGameState <- webM readState
    let newShp =
          view (systems . at' systemId . ssShips . at' shipId) newGameState
    json $ DeleteShipOrderResponse systemId shipId newShp

addOrder
  :: StarSystemId -> ShipId -> Maybe Int -> ShipOrder -> GameState -> GameState
addOrder systemId shipId maybeGroupId newOrder gameState = newGameState
 where
  currentShip = view (systems . at' systemId . ssShips . at' shipId) gameState
  newShp      = if isNothing maybeGroupId
    then addShipOrder newOrder currentShip
    else addShipOrderToGroup newOrder (fromJust maybeGroupId) currentShip
  newGameState =
    set (systems . at' systemId . ssShips . at' shipId) newShp gameState

addConditionalOrder
  :: StarSystemId
  -> ShipId
  -> OrderCondition
  -> ShipOrder
  -> GameState
  -> GameState
addConditionalOrder systemId shipId newCondition newOrder gameState =
  newGameState
 where
  currentShip = view (systems . at' systemId . ssShips . at' shipId) gameState
  newShp      = addShipConditionalOrder newCondition newOrder currentShip
  newGameState =
    set (systems . at' systemId . ssShips . at' shipId) newShp gameState

updateOrder :: StarSystemId -> ShipId -> Int -> Int -> GameState -> GameState
updateOrder systemId shipId groupId newRepeat gameState = newGameState
 where
  currentShip = view (systems . at' systemId . ssShips . at' shipId) gameState
  newShp      = updateOrderGroupRepeat groupId newRepeat currentShip
  newGameState =
    set (systems . at' systemId . ssShips . at' shipId) newShp gameState

removeOrder
  :: StarSystemId -> ShipId -> Int -> Maybe Int -> GameState -> GameState
removeOrder systemId shipId groupIdx maybeOrderIdx gameState = newGameState
 where
  currentShip = view (systems . at' systemId . ssShips . at' shipId) gameState
  currentOrderGroups = view sOrderGroups currentShip
  currentOrderGroupOrders = if length currentOrderGroups > 0
    then view sogOrders (head currentOrderGroups)
    else []
  lastOrderInGroup = length currentOrderGroupOrders < 2
  newShp           = if isNothing maybeOrderIdx || lastOrderInGroup
    then removeShipOrderGroup groupIdx currentShip
    else removeShipOrderFromGroup groupIdx (fromJust maybeOrderIdx) currentShip
  newGameState =
    set (systems . at' systemId . ssShips . at' shipId) newShp gameState

removeConditionalOrder
  :: StarSystemId -> ShipId -> Int -> GameState -> GameState
removeConditionalOrder systemId shipId conditionalOrderIdx gameState =
  newGameState
 where
  currentShip = view (systems . at' systemId . ssShips . at' shipId) gameState
  newShp      = removeShipConditionalOrder conditionalOrderIdx currentShip
  newGameState =
    set (systems . at' systemId . ssShips . at' shipId) newShp gameState
