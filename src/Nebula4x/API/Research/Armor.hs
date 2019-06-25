{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Nebula4x.API.Research.Armor where

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

data ArmorResponse = ArmorResponse
  { arRaceId :: ComponentId
  , arArmor :: ResearchStatus ArmorResearch
  } deriving (Show, Eq, Generic)

instance ToJSON ArmorResponse

instance FromJSON ArmorResponse

armorResearchRoute :: WebRoute ()
armorResearchRoute = do
  get "/api/research/armor" $ do
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    json $ ArmorResponse raceId (view (research . at' raceId . rArmor) gameState)
  post "/api/research/armor" $ do
    researchRequest <- jsonData
    gameState       <- webM readState
    let raceId = getPlayerRaceId gameState
    let currentLabs =
          view (research . at' raceId . rArmor . researchLabs) gameState
    let newGameState = handleResearch
          currentLabs
          (\bodyId numLabs -> set
            (research . at' raceId . rArmor . researchLabs . at' bodyId)
            numLabs
          )
          researchRequest
          gameState
    webM $ modify (\_ -> newGameState)
    json $ ArmorResponse raceId (view (research . at' raceId . rArmor) newGameState)
