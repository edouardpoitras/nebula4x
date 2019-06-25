{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Nebula4x.API.Research.FuelStorage where

import           Control.Lens                  as L
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           GHC.Generics                   ( Generic )
import           Web.Scotty.Trans

import           Nebula4x.API.Research
import           Nebula4x.API.Server
import           Nebula4x.Race
import           Nebula4x.Types
import           Nebula4x.Utils

data FuelStorageResponse = FuelStorageResponse
  { fsrRaceId :: ComponentId
  , fsrFuelStorage :: ResearchStatus FuelStorageResearch
  } deriving (Show, Eq, Generic)

instance ToJSON FuelStorageResponse

instance FromJSON FuelStorageResponse

fuelStorageResearchRoute :: WebRoute ()
fuelStorageResearchRoute = do
  get "/api/research/fuel-storage" $ do
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    json $ FuelStorageResponse raceId (view (research . at' raceId . rFuelStorage) gameState)
  post "/api/research/fuel-storage" $ do
    researchRequest <- jsonData
    gameState       <- webM readState
    let raceId = getPlayerRaceId gameState
    let currentLabs =
          view (research . at' raceId . rFuelStorage . researchLabs) gameState
    let newGameState = handleResearch
          currentLabs
          (\bodyId numLabs -> set
            (research . at' raceId . rFuelStorage . researchLabs . at' bodyId)
            numLabs
          )
          researchRequest
          gameState
    webM $ modify (\_ -> newGameState)
    json $ FuelStorageResponse raceId (view (research . at' raceId . rFuelStorage) newGameState)
