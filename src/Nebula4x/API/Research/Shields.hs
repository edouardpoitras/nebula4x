{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Nebula4x.API.Research.Shields where

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

data ShieldResponse = ShieldResponse
  { shrRaceId :: ComponentId
  , shrShield :: ResearchStatus ShieldResearch
  } deriving (Show, Eq, Generic)

instance ToJSON ShieldResponse

instance FromJSON ShieldResponse

shieldsResearchRoute :: WebRoute ()
shieldsResearchRoute = do
  get "/api/research/shields" $ do
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    json $ ShieldResponse raceId (view (research . at' raceId . rShield) gameState)
  post "/api/research/shields" $ do
    researchRequest <- jsonData
    gameState       <- webM readState
    let raceId = getPlayerRaceId gameState
    let currentLabs = view (research . at' raceId . rShield . researchLabs) gameState
    let newGameState = handleResearch
          currentLabs
          (\bodyId numLabs ->
            set (research . at' raceId . rShield . researchLabs . at' bodyId) numLabs
          )
          researchRequest
          gameState
    webM $ modify (\_ -> newGameState)
    json $ ShieldResponse raceId (view (research . at' raceId . rShield) newGameState)
