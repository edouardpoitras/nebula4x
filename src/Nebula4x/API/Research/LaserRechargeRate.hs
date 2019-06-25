{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Nebula4x.API.Research.LaserRechargeRate where

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

data LaserRechargeRateResponse = LaserRechargeRateResponse
  { lrrrRaceId :: ComponentId
  , lrrrRechargeRate :: ResearchStatus LaserRechargeRateResearch
  } deriving (Show, Eq, Generic)

instance ToJSON LaserRechargeRateResponse

instance FromJSON LaserRechargeRateResponse

laserRechargeRateResearchRoute :: WebRoute ()
laserRechargeRateResearchRoute = do
  get "/api/research/laser-recharge-rate" $ do
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    json $ LaserRechargeRateResponse
      raceId
      (view (research . at' raceId . rLaserRechargeRate) gameState)
  post "/api/research/laser-recharge-rate" $ do
    researchRequest <- jsonData
    gameState       <- webM readState
    let raceId = getPlayerRaceId gameState
    let currentLabs = view
          (research . at' raceId . rLaserRechargeRate . researchLabs)
          gameState
    let newGameState = handleResearch
          currentLabs
          (\bodyId numLabs -> set
            ( research
            . at' raceId
            . rLaserRechargeRate
            . researchLabs
            . at' bodyId
            )
            numLabs
          )
          researchRequest
          gameState
    webM $ modify (\_ -> newGameState)
    json $ LaserRechargeRateResponse
      raceId
      (view (research . at' raceId . rLaserRechargeRate) newGameState)
