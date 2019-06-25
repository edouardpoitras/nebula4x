{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Nebula4x.API.Research.MissleLauncherReloadRate where

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

data MissleLauncherReloadRateResponse = MissleLauncherReloadRateResponse
  { mlrrrRaceId :: ComponentId
  , mlrrrReloadRate :: ResearchStatus MissleLauncherReloadRateResearch
  } deriving (Show, Eq, Generic)

instance ToJSON MissleLauncherReloadRateResponse

instance FromJSON MissleLauncherReloadRateResponse

missleLauncherReloadRateResearchRoute :: WebRoute ()
missleLauncherReloadRateResearchRoute = do
  get "/api/research/missle-launcher-reload-rate" $ do
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    json $ MissleLauncherReloadRateResponse raceId (view (research . at' raceId . rMissleLauncherReloadRate) gameState)
  post "/api/research/missle-launcher-reload-rate" $ do
    researchRequest <- jsonData
    gameState       <- webM readState
    let raceId = getPlayerRaceId gameState
    let currentLabs = view
          (research . at' raceId . rMissleLauncherReloadRate . researchLabs)
          gameState
    let newGameState = handleResearch
          currentLabs
          (\bodyId numLabs -> set
            ( research
            . at' raceId
            . rMissleLauncherReloadRate
            . researchLabs
            . at' bodyId
            )
            numLabs
          )
          researchRequest
          gameState
    webM $ modify (\_ -> newGameState)
    json $ MissleLauncherReloadRateResponse raceId (view (research . at' raceId . rMissleLauncherReloadRate) newGameState)
