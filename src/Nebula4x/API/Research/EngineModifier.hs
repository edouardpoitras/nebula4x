{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Nebula4x.API.Research.EngineModifier where

import           Control.Lens          as L
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

data EngineModifierResponse = EngineModifierResponse
  { emrRaceId :: ComponentId
  , emrModifier :: ResearchStatus PowerEfficiencyModifierResearch
  } deriving (Show, Eq, Generic)

instance ToJSON EngineModifierResponse

instance FromJSON EngineModifierResponse

engineModifierResearchRoute :: WebRoute ()
engineModifierResearchRoute = do
  get "/api/research/engine-modifier" $ do
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    json $ EngineModifierResponse raceId (view (research . at' raceId . rEngineModifier) gameState)
  post "/api/research/engine-modifier" $ do
    researchRequest <- jsonData
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    let currentLabs = view (research . at' raceId . rEngineModifier . researchLabs) gameState
    let newGameState =
          handleResearch
            currentLabs
            (\bodyId numLabs ->
               set
                 (research . at' raceId . rEngineModifier . researchLabs . at' bodyId)
                 numLabs)
            researchRequest
            gameState
    webM $ modify (\_ -> newGameState)
    json $ EngineModifierResponse raceId (view (research . at' raceId . rEngineModifier) newGameState)
