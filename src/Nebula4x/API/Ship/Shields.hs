{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Nebula4x.API.Ship.Shields where

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
import           Web.Scotty.Trans               ( json
                                                , jsonData
                                                , post
                                                )

import           Nebula4x.Types
import           Nebula4x.Utils                 ( at' )

data ToggleShields = ToggleShields
  { toggleShieldsSystemId :: StarSystemId
  , toggleShieldsShipId   :: ShipId
  } deriving (Show, Eq, Generic)

instance ToJSON ToggleShields

instance FromJSON ToggleShields

data ToggleShieldsResponse = ToggleShieldsResponse
  { toggleShieldsResponseSystemId :: StarSystemId
  , toggleShieldsResponseShipId   :: ShipId
  , toggleShieldsResponseShip     :: Ship
  } deriving (Show, Eq, Generic)

instance ToJSON ToggleShieldsResponse

instance FromJSON ToggleShieldsResponse

toggleShieldsRoute :: WebRoute ()
toggleShieldsRoute = post "/api/ship/toggle-shields" $ do
  toggleShields <- jsonData
  let systemId = toggleShieldsSystemId toggleShields
  let shipId   = toggleShieldsShipId toggleShields
  webM $ modify (toggleShipShields systemId shipId)
  gameState <- webM readState
  json $ ToggleShieldsResponse
    systemId
    shipId
    (view (systems . at' systemId . ssShips . at' shipId) gameState)

toggleShipShields :: StarSystemId -> ShipId -> GameState -> GameState
toggleShipShields systemId shipId gameState = newGameState
 where
  currentShip  = view (systems . at' systemId . ssShips . at' shipId) gameState
  maybeShields = view sShields currentShip
  newShp       = if isNothing maybeShields
    then currentShip
    else currentShip { _sShields = Just newShields }
  currentShields = fromJust maybeShields
  newShields     = over ssEnabled not currentShields
  newGameState =
    set (systems . at' systemId . ssShips . at' shipId) newShp gameState