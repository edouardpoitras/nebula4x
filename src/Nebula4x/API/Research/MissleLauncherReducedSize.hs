{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Nebula4x.API.Research.MissleLauncherReducedSize where

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

data MissleLauncherReducedSizeResponse = MissleLauncherReducedSizeResponse
  { mlrsrRaceId :: ComponentId
  , mlrsrReducedSize :: ResearchStatus MissleLauncherReducedSizeResearch
  } deriving (Show, Eq, Generic)

instance ToJSON MissleLauncherReducedSizeResponse

instance FromJSON MissleLauncherReducedSizeResponse

missleLauncherReducedSizeResearchRoute :: WebRoute ()
missleLauncherReducedSizeResearchRoute = do
  get "/api/research/missle-launcher-reduced-size" $ do
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    json $ MissleLauncherReducedSizeResponse raceId (view (research . at' raceId . rMissleLauncherReducedSize) gameState)
  post "/api/research/missle-launcher-reduced-size" $ do
    researchRequest <- jsonData
    gameState       <- webM readState
    let raceId = getPlayerRaceId gameState
    let currentLabs = view
          (research . at' raceId . rMissleLauncherReducedSize . researchLabs)
          gameState
    let newGameState = handleResearch
          currentLabs
          (\bodyId numLabs -> set
            ( research
            . at' raceId
            . rMissleLauncherReducedSize
            . researchLabs
            . at' bodyId
            )
            numLabs
          )
          researchRequest
          gameState
    webM $ modify (\_ -> newGameState)
    json
      $ MissleLauncherReducedSizeResponse raceId (view (research . at' raceId . rMissleLauncherReducedSize) newGameState)
