{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Nebula4x.API.Research.Installments where

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

data InstallmentResponse = InstallmentResponse
  { irRaceId :: ComponentId
  , irInstallment :: ResearchStatus InstallmentResearch
  } deriving (Show, Eq, Generic)

instance ToJSON InstallmentResponse

instance FromJSON InstallmentResponse

installmentsResearchRoute :: WebRoute ()
installmentsResearchRoute = do
  get "/api/research/mines" $ do
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    json $ InstallmentResponse raceId (view (research . at' raceId . rMines) gameState)
  post "/api/research/mines" $ do
    researchRequest <- jsonData
    gameState       <- webM readState
    let raceId = getPlayerRaceId gameState
    let currentLabs =
          view (research . at' raceId . rMines . researchLabs) gameState
    let newGameState = handleResearch
          currentLabs
          (\bodyId numLabs -> set
            (research . at' raceId . rMines . researchLabs . at' bodyId)
            numLabs
          )
          researchRequest
          gameState
    webM $ modify (\_ -> newGameState)
    json $ InstallmentResponse raceId (view (research . at' raceId . rMines) newGameState)
  get "/api/research/research-labs" $ do
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    json $ InstallmentResponse raceId (view (research . at' raceId . rResearchLabs) gameState)
  post "/api/research/research-labs" $ do
    researchRequest <- jsonData
    gameState       <- webM readState
    let raceId = getPlayerRaceId gameState
    let currentLabs =
          view (research . at' raceId . rResearchLabs . researchLabs) gameState
    let newGameState = handleResearch
          currentLabs
          (\bodyId numLabs -> set
            (research . at' raceId . rResearchLabs . researchLabs . at' bodyId)
            numLabs
          )
          researchRequest
          gameState
    webM $ modify (\_ -> newGameState)
    json $ InstallmentResponse raceId (view (research . at' raceId . rResearchLabs) newGameState)
  get "/api/research/fuel-refineries" $ do
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    json $ InstallmentResponse raceId (view (research . at' raceId . rFuelRefineries) gameState)
  post "/api/research/fuel-refineries" $ do
    researchRequest <- jsonData
    gameState       <- webM readState
    let raceId = getPlayerRaceId gameState
    let currentLabs = view
          (research . at' raceId . rFuelRefineries . researchLabs)
          gameState
    let newGameState = handleResearch
          currentLabs
          (\bodyId numLabs -> set
            (research . at' raceId . rFuelRefineries . researchLabs . at' bodyId
            )
            numLabs
          )
          researchRequest
          gameState
    webM $ modify (\_ -> newGameState)
    json $ InstallmentResponse raceId (view (research . at' raceId . rFuelRefineries) newGameState)
  get "/api/research/construction-factories" $ do
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    json $ InstallmentResponse raceId (view (research . at' raceId . rConstructionFactories) gameState)
  post "/api/research/construction-factories" $ do
    researchRequest <- jsonData
    gameState       <- webM readState
    let raceId = getPlayerRaceId gameState
    let currentLabs = view
          (research . at' raceId . rConstructionFactories . researchLabs)
          gameState
    let newGameState = handleResearch
          currentLabs
          (\bodyId numLabs -> set
            ( research
            . at' raceId
            . rConstructionFactories
            . researchLabs
            . at' bodyId
            )
            numLabs
          )
          researchRequest
          gameState
    webM $ modify (\_ -> newGameState)
    json $ InstallmentResponse raceId (view (research . at' raceId . rConstructionFactories) newGameState)
  get "/api/research/mass-drivers" $ do
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    json $ InstallmentResponse raceId (view (research . at' raceId . rMassDrivers) gameState)
  post "/api/research/mass-drivers" $ do
    researchRequest <- jsonData
    gameState       <- webM readState
    let raceId = getPlayerRaceId gameState
    let currentLabs =
          view (research . at' raceId . rMassDrivers . researchLabs) gameState
    let newGameState = handleResearch
          currentLabs
          (\bodyId numLabs -> set
            (research . at' raceId . rMassDrivers . researchLabs . at' bodyId)
            numLabs
          )
          researchRequest
          gameState
    webM $ modify (\_ -> newGameState)
    json $ InstallmentResponse raceId (view (research . at' raceId . rMassDrivers) newGameState)
