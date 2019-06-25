{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Nebula4x.API.Research.Sensors where

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

data SensorResponse = SensorResponse
  { srRaceId :: ComponentId
  , srSensor :: ResearchStatus SensorResearch
  } deriving (Show, Eq, Generic)

instance ToJSON SensorResponse

instance FromJSON SensorResponse

sensorsResearchRoute :: WebRoute ()
sensorsResearchRoute = do
  get "/api/research/geological-sensor" $ do
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    json $ SensorResponse
      raceId
      (view (research . at' raceId . rGeologicalSensor) gameState)
  post "/api/research/geological-sensor" $ do
    researchRequest <- jsonData
    gameState       <- webM readState
    let raceId = getPlayerRaceId gameState
    let currentLabs = view
          (research . at' raceId . rGeologicalSensor . researchLabs)
          gameState
    let newGameState = handleResearch
          currentLabs
          (\bodyId numLabs -> set
            ( research
            . at' raceId
            . rGeologicalSensor
            . researchLabs
            . at' bodyId
            )
            numLabs
          )
          researchRequest
          gameState
    webM $ modify (\_ -> newGameState)
    json $ view (research . at' raceId . rGeologicalSensor) newGameState
  get "/api/research/gravitational-sensor" $ do
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    json $ SensorResponse
      raceId
      (view (research . at' raceId . rGravitationalSensor) gameState)
  post "/api/research/gravitational-sensor" $ do
    researchRequest <- jsonData
    gameState       <- webM readState
    let raceId = getPlayerRaceId gameState
    let currentLabs = view
          (research . at' raceId . rGravitationalSensor . researchLabs)
          gameState
    let newGameState = handleResearch
          currentLabs
          (\bodyId numLabs -> set
            ( research
            . at' raceId
            . rGravitationalSensor
            . researchLabs
            . at' bodyId
            )
            numLabs
          )
          researchRequest
          gameState
    webM $ modify (\_ -> newGameState)
    json $ view (research . at' raceId . rGravitationalSensor) newGameState
