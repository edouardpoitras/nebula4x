{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Nebula4x.API.Research.LaserReducedSize where

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

data LaserReducedSizeResponse = LaserReducedSizeResponse
  { lrsrRaceId :: ComponentId
  , lrsrReducedSize :: ResearchStatus LaserReducedSizeResearch
  } deriving (Show, Eq, Generic)

instance ToJSON LaserReducedSizeResponse

instance FromJSON LaserReducedSizeResponse

laserReducedSizeResearchRoute :: WebRoute ()
laserReducedSizeResearchRoute = do
  get "/api/research/laser-reduced-size" $ do
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    json $ LaserReducedSizeResponse raceId (view (research . at' raceId . rLaserReducedSize) gameState)
  post "/api/research/laser-reduced-size" $ do
    researchRequest <- jsonData
    gameState       <- webM readState
    let raceId = getPlayerRaceId gameState
    let currentLabs = view
          (research . at' raceId . rLaserReducedSize . researchLabs)
          gameState
    let newGameState = handleResearch
          currentLabs
          (\bodyId numLabs -> set
            ( research
            . at' raceId
            . rLaserReducedSize
            . researchLabs
            . at' bodyId
            )
            numLabs
          )
          researchRequest
          gameState
    webM $ modify (\_ -> newGameState)
    json $ LaserReducedSizeResponse raceId (view (research . at' raceId . rLaserReducedSize) newGameState)
