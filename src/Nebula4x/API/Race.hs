{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Nebula4x.API.Race where

import           Control.Lens
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import qualified Data.Map.Strict               as Map
import           GHC.Generics                   ( Generic )
import           Nebula4x.API.Server            ( WebRoute
                                                , modify
                                                , readState
                                                , webM
                                                )
import           Network.HTTP.Types.Status      ( status404 )
import           Web.Scotty.Trans               ( get
                                                , json
                                                , jsonData
                                                , param
                                                , post
                                                , status
                                                )

import           Nebula4x.Race
import           Nebula4x.Types          hiding ( status )
import           Nebula4x.Utils

newtype SwitchRaceRequest = SwitchRaceRequest { srrRaceId :: RaceId } deriving (Show, Eq, Generic)

instance ToJSON SwitchRaceRequest

instance FromJSON SwitchRaceRequest

raceRoute :: WebRoute ()
raceRoute = do
  get "/api/races" $ do
    gameState <- webM readState
    json $ view races gameState
  get "/api/races/:raceId" $ do
    raceIdStr <- param "raceId"
    let raceId = read raceIdStr
    gameState <- webM readState
    let rcs  = view races gameState
    let race = Map.lookup raceId rcs
    case race of
      Just r  -> json r
      Nothing -> status status404
  post "/api/races" $ do
    switchRaceRequest <- jsonData
    gameState         <- webM readState
    let requestedRace = srrRaceId switchRaceRequest
    let currentRace   = getPlayerRaceId gameState
    webM
      $ modify
      $ set (races . at' currentRace . rController)   AI
      . set (races . at' requestedRace . rController) Human
    gameState' <- webM readState
    json (view races gameState')
