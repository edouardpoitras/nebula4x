{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Nebula4x.API.Research.LaserWavelength where

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

data LaserWavelengthResponse = LaserWavelengthResponse
  { lwrRaceId :: ComponentId
  , lwrWavelength :: ResearchStatus LaserWavelengthResearch
  } deriving (Show, Eq, Generic)

instance ToJSON LaserWavelengthResponse

instance FromJSON LaserWavelengthResponse

laserWavelengthResearchRoute :: WebRoute ()
laserWavelengthResearchRoute = do
  get "/api/research/laser-wavelength" $ do
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    json $ LaserWavelengthResponse
      raceId
      (view (research . at' raceId . rLaserWavelength) gameState)
  post "/api/research/laser-wavelength" $ do
    researchRequest <- jsonData
    gameState       <- webM readState
    let raceId = getPlayerRaceId gameState
    let currentLabs = view
          (research . at' raceId . rLaserWavelength . researchLabs)
          gameState
    let newGameState = handleResearch
          currentLabs
          (\bodyId numLabs -> set
            ( research
            . at' raceId
            . rLaserWavelength
            . researchLabs
            . at' bodyId
            )
            numLabs
          )
          researchRequest
          gameState
    webM $ modify (\_ -> newGameState)
    json $ LaserWavelengthResponse
      raceId
      (view (research . at' raceId . rLaserWavelength) newGameState)
