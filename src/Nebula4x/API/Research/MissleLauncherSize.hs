{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Nebula4x.API.Research.MissleLauncherSize where

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

data MissleLauncherSizeResponse = MissleLauncherSizeResponse
  { mlsrRaceId :: ComponentId
  , mlsrSize :: ResearchStatus MissleLauncherSizeResearch
  } deriving (Show, Eq, Generic)

instance ToJSON MissleLauncherSizeResponse

instance FromJSON MissleLauncherSizeResponse

missleLauncherSizeResearchRoute :: WebRoute ()
missleLauncherSizeResearchRoute = do
  get "/api/research/missle-launcher-size" $ do
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    json $ MissleLauncherSizeResponse raceId (view (research . at' raceId . rMissleLauncherSize) gameState)
  post "/api/research/missle-launcher-size" $ do
    researchRequest <- jsonData
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    let currentLabs = view (research . at' raceId . rMissleLauncherSize . researchLabs) gameState
    let newGameState =
          handleResearch
            currentLabs
            (\bodyId numLabs ->
               set (research . at' raceId . rMissleLauncherSize . researchLabs . at' bodyId) numLabs)
            researchRequest
            gameState
    webM $ modify (\_ -> newGameState)
    json $ MissleLauncherSizeResponse raceId (view (research . at' raceId . rMissleLauncherSize) newGameState)
