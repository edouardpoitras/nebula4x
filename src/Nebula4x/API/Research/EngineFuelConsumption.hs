{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Nebula4x.API.Research.EngineFuelConsumption where

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

data EngineFuelConsumptionResponse = EngineFuelConsumptionResponse
  { efcrRaceId :: ComponentId
  , efcrFuelConsumption :: ResearchStatus FuelConsumptionResearch
  } deriving (Show, Eq, Generic)

instance ToJSON EngineFuelConsumptionResponse

instance FromJSON EngineFuelConsumptionResponse

engineFuelConsumptionResearchRoute :: WebRoute ()
engineFuelConsumptionResearchRoute = do
  get "/api/research/engine-fuel-consumption" $ do
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    json $ EngineFuelConsumptionResponse
      raceId
      (view (research . at' raceId . rEngineFuelConsumption) gameState)
  post "/api/research/engine-fuel-consumption" $ do
    researchRequest <- jsonData
    gameState       <- webM readState
    let raceId = getPlayerRaceId gameState
    let currentLabs = view
          (research . at' raceId . rEngineFuelConsumption . researchLabs)
          gameState
    let newGameState = handleResearch
          currentLabs
          (\bodyId numLabs -> set
            ( research
            . at' raceId
            . rEngineFuelConsumption
            . researchLabs
            . at' bodyId
            )
            numLabs
          )
          researchRequest
          gameState
    webM $ modify (\_ -> newGameState)
    json $ EngineFuelConsumptionResponse
      raceId
      (view (research . at' raceId . rEngineFuelConsumption) newGameState)
