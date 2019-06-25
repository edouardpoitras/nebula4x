{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Nebula4x.API.Research where

import           Control.Lens                  as L
import           Control.Newtype.Generics
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           GHC.Generics                   ( Generic )
import           Web.Scotty.Trans

import           Nebula4x.API.Server
import           Nebula4x.Installment
import           Nebula4x.Race
import           Nebula4x.Research
import           Nebula4x.Types
import           Nebula4x.Utils

data ResearchRequest = ResearchRequest
  { rrSystemId :: StarSystemId
  , rrBodyId   :: BodyId
  , rrNumLabs  :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON ResearchRequest

instance FromJSON ResearchRequest

data ResearchResponse = ResearchResponse
  { rRaceId :: ComponentId
  , rResearch :: Research
  } deriving (Show, Eq, Generic)

instance ToJSON ResearchResponse

instance FromJSON ResearchResponse

data UnlockAllResearchRequest = UnlockAllResearchRequest
  { uarRaceId :: RaceId } deriving (Show, Eq, Generic)

instance ToJSON UnlockAllResearchRequest

instance FromJSON UnlockAllResearchRequest

researchRoute :: WebRoute ()
researchRoute = do
  get "/api/research" $ do
    gameState <- webM readState
    json $ L.view research gameState
  get "/api/research-race" $ do
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    json $ ResearchResponse raceId (L.view (research . at' raceId) gameState)
  post "/api/research-unlock-all" $ do
    unlockResearch <- jsonData
    let raceId = uarRaceId unlockResearch
    webM $ modify (unlockAllRaceResearch raceId)
    gameState <- webM readState
    json $ view research gameState

handleResearch
  :: BodyResearchLabs
  -> (BodyId -> Int -> GameState -> GameState)
  -> ResearchRequest
  -> GameState
  -> GameState
handleResearch currentLabs numLabsUpdater (ResearchRequest systemId bodyId numLabs) gs
  = newGameState
 where
  labId            = _iId basicResearchLab
  bodyInstallments = view (systems . at' systemId . ssBodies . at' bodyId . bInstallments) gs
  maybeBodyResearchLabs = Map.lookup labId bodyInstallments
  bodyResearchLabs      = if isNothing maybeBodyResearchLabs
    then InstallmentStack (InstallmentCount 0) basicResearchLab
    else fromJust maybeBodyResearchLabs
  availableLabs = floor $ unpack $ view isCount bodyResearchLabs
  usedLabs      = if elem bodyId (Map.keys currentLabs)
    then fromJust $ Map.lookup bodyId currentLabs
    else 0
  totalLabs        = availableLabs + usedLabs
  newAvailableLabs = availableLabs + (usedLabs - numLabs)
  newGameState =
    if newAvailableLabs
         +  numLabs
         /= totalLabs
         || newAvailableLabs
         <  0
         || numLabs
         <  0
      then gs
      else updatedGameState'
  updatedGameState' = numLabsUpdater bodyId numLabs updatedGameState
  updatedGameState =
    if isJust $ view
         ( systems
         . at' systemId
         . ssBodies
         . at' bodyId
         . bInstallments
         . at labId
         )
         gs
      then set
        ( systems
        . at' systemId
        . ssBodies
        . at' bodyId
        . bInstallments
        . at' labId
        . isCount
        )
        (InstallmentCount $ fromIntegral newAvailableLabs)
        gs
      else L.over
        (systems . at' systemId . ssBodies . at' bodyId . bInstallments)
        (Map.insert
          labId
          (InstallmentStack (InstallmentCount $ fromIntegral newAvailableLabs)
                            basicResearchLab
          )
        )
        gs
