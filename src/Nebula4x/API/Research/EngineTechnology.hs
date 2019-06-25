{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Nebula4x.API.Research.EngineTechnology where

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

data EngineTechnologyResponse = EngineTechnologyResponse
  { etrRaceId :: ComponentId
  , etrTechnology :: ResearchStatus EngineTechnologyResearch
  } deriving (Show, Eq, Generic)

instance ToJSON EngineTechnologyResponse

instance FromJSON EngineTechnologyResponse

engineTechnologyResearchRoute :: WebRoute ()
engineTechnologyResearchRoute = do
  get "/api/research/engine-technology" $ do
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    json $ EngineTechnologyResponse
      raceId
      (view (research . at' raceId . rEngineTechnology) gameState)
  post "/api/research/engine-technology" $ do
    researchRequest <- jsonData
    gameState       <- webM readState
    let raceId = getPlayerRaceId gameState
    let currentLabs = view
          (research . at' raceId . rEngineTechnology . researchLabs)
          gameState
    let newGameState = handleResearch
          currentLabs
          (\bodyId numLabs -> set
            ( research
            . at' raceId
            . rEngineTechnology
            . researchLabs
            . at' bodyId
            )
            numLabs
          )
          researchRequest
          gameState
    webM $ modify (\_ -> newGameState)
    json $ EngineTechnologyResponse
      raceId
      (view (research . at' raceId . rEngineTechnology) newGameState)
