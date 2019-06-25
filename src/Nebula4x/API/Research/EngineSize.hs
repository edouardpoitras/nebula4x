{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Nebula4x.API.Research.EngineSize where

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

data EngineSizeResponse = EngineSizeResponse
  { esrRaceId :: ComponentId
  , esrSize :: ResearchStatus EngineSizeResearch
  } deriving (Show, Eq, Generic)

instance ToJSON EngineSizeResponse

instance FromJSON EngineSizeResponse

engineSizeResearchRoute :: WebRoute ()
engineSizeResearchRoute = do
  get "/api/research/engine-size" $ do
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    json $ EngineSizeResponse
      raceId
      (view (research . at' raceId . rEngineSize) gameState)
  post "/api/research/engine-size" $ do
    researchRequest <- jsonData
    gameState       <- webM readState
    let raceId = getPlayerRaceId gameState
    let currentLabs =
          view (research . at' raceId . rEngineSize . researchLabs) gameState
    let newGameState = handleResearch
          currentLabs
          (\bodyId numLabs -> set
            (research . at' raceId . rEngineSize . researchLabs . at' bodyId)
            numLabs
          )
          researchRequest
          gameState
    webM $ modify (\_ -> newGameState)
    json $ EngineSizeResponse
      raceId
      (view (research . at' raceId . rEngineSize) newGameState)
