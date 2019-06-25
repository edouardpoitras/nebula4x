{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Nebula4x.API.Research.LaserFocalSize where

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

data LaserFocalSizeResponse = LaserFocalSizeResponse
  { lfsrRaceId :: ComponentId
  , lfsrFocalSize:: ResearchStatus LaserFocalSizeResearch
  } deriving (Show, Eq, Generic)

instance ToJSON LaserFocalSizeResponse

instance FromJSON LaserFocalSizeResponse

laserFocalSizeResearchRoute :: WebRoute ()
laserFocalSizeResearchRoute = do
  get "/api/research/laser-focal-size" $ do
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    json $ LaserFocalSizeResponse raceId (view (research . at' raceId . rLaserFocalSize) gameState)
  post "/api/research/laser-focal-size" $ do
    researchRequest <- jsonData
    gameState       <- webM readState
    let raceId = getPlayerRaceId gameState
    let currentLabs = view
          (research . at' raceId . rLaserFocalSize . researchLabs)
          gameState
    let newGameState = handleResearch
          currentLabs
          (\bodyId numLabs -> set
            (research . at' raceId . rLaserFocalSize . researchLabs . at' bodyId
            )
            numLabs
          )
          researchRequest
          gameState
    webM $ modify (\_ -> newGameState)
    json $ LaserFocalSizeResponse raceId (view (research . at' raceId . rLaserFocalSize) newGameState)
