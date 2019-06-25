{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Nebula4x.API.Research.Transport where

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

data CargoHandlingResponse = CargoHandlingResponse
  { chrRaceId :: ComponentId
  , chrCargoHandling :: ResearchStatus CargoHandlingResearch
  } deriving (Show, Eq, Generic)

instance ToJSON CargoHandlingResponse

instance FromJSON CargoHandlingResponse

data CargoHoldResponse = CargoHoldResponse
  { chdrRaceId :: ComponentId
  , chdrCargoHold :: ResearchStatus CargoHoldResearch
  } deriving (Show, Eq, Generic)

instance ToJSON CargoHoldResponse

instance FromJSON CargoHoldResponse

data JumpGateResponse = JumpGateResponse
  { jgrRaceId :: ComponentId
  , jgrJumpGate :: ResearchStatus JumpGateResearch
  } deriving (Show, Eq, Generic)

instance ToJSON JumpGateResponse

instance FromJSON JumpGateResponse

cargoHandlingResearchRoute :: WebRoute ()
cargoHandlingResearchRoute = do
  get "/api/research/cargo-handling-systems" $ do
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    json $ CargoHandlingResponse raceId (view (research . at' raceId . rCargoHandling) gameState)
  post "/api/research/cargo-handling-systems" $ do
    researchRequest <- jsonData
    gameState       <- webM readState
    let raceId = getPlayerRaceId gameState
    let currentLabs =
          view (research . at' raceId . rCargoHandling . researchLabs) gameState
    let newGameState = handleResearch
          currentLabs
          (\bodyId numLabs -> set
            (research . at' raceId . rCargoHandling . researchLabs . at' bodyId)
            numLabs
          )
          researchRequest
          gameState
    webM $ modify (\_ -> newGameState)
    json $ CargoHandlingResponse raceId (view (research . at' raceId . rCargoHandling) newGameState)

cargoHoldResearchRoute :: WebRoute ()
cargoHoldResearchRoute = do
  get "/api/research/cargo-holds" $ do
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    json $ CargoHoldResponse raceId (view (research . at' raceId . rCargoHold) gameState)
  post "/api/research/cargo-holds" $ do
    researchRequest <- jsonData
    gameState       <- webM readState
    let raceId = getPlayerRaceId gameState
    let currentLabs =
          view (research . at' raceId . rCargoHold . researchLabs) gameState
    let newGameState = handleResearch
          currentLabs
          (\bodyId numLabs -> set
            (research . at' raceId . rCargoHold . researchLabs . at' bodyId)
            numLabs
          )
          researchRequest
          gameState
    webM $ modify (\_ -> newGameState)
    json $ CargoHoldResponse raceId (view (research . at' raceId . rCargoHold) newGameState)

jumpGateResearchRoute :: WebRoute ()
jumpGateResearchRoute = do
  get "/api/research/jump-gates" $ do
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    json $ JumpGateResponse raceId (view (research . at' raceId . rJumpGate) gameState)
  post "/api/research/jump-gates" $ do
    researchRequest <- jsonData
    gameState       <- webM readState
    let raceId = getPlayerRaceId gameState
    let currentLabs =
          view (research . at' raceId . rJumpGate . researchLabs) gameState
    let newGameState = handleResearch
          currentLabs
          (\bodyId numLabs -> set
            (research . at' raceId . rJumpGate . researchLabs . at' bodyId)
            numLabs
          )
          researchRequest
          gameState
    webM $ modify (\_ -> newGameState)
    json $ JumpGateResponse raceId (view (research . at' raceId . rJumpGate) newGameState)
