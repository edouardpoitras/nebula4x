{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Nebula4x.API.Design.Engine where

import           Control.Lens
import           Control.Monad.IO.Class
import qualified Control.Newtype.Generics      as NT
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.List                      ( elemIndex )
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           GHC.Generics                   ( Generic )
import           Network.HTTP.Types.Status      ( status404 )
import           System.Random
import           Web.Scotty.Trans               ( delete
                                                , get
                                                , json
                                                , jsonData
                                                , param
                                                , post
                                                , status
                                                )

import           Nebula4x.API.Server            ( WebRoute
                                                , modify
                                                , readState
                                                , webM
                                                )
import           Nebula4x.Component
import           Nebula4x.Config
import           Nebula4x.Installment
import           Nebula4x.Race
import           Nebula4x.Types          hiding ( status )
import           Nebula4x.Utils

data EngineDesignResponse = EngineDesignResponse
  { engineDesignRaceId :: ComponentId
  , engineDesign :: EngineDesign
  } deriving (Show, Eq, Generic)

instance ToJSON EngineDesignResponse

instance FromJSON EngineDesignResponse

data EngineDesignsResponse = EngineDesignsResponse
  { engineDesignsRaceId :: ComponentId
  , engineDesigns :: ResearchStatus EngineDesign
  } deriving (Show, Eq, Generic)

instance ToJSON EngineDesignsResponse

instance FromJSON EngineDesignsResponse

data CheckEngineRequest = CheckEngineRequest
  { checkEngineModifier :: ComponentId
  , checkEngineSize     :: ComponentId
  } deriving (Show, Eq, Generic)

instance ToJSON CheckEngineRequest

instance FromJSON CheckEngineRequest

data CreateEngineRequest = CreateEngineRequest
  { engineModifier :: ComponentId
  , engineSize     :: ComponentId
  , engineName     :: String
  } deriving (Show, Eq, Generic)

instance ToJSON CreateEngineRequest

instance FromJSON CreateEngineRequest

data CreateEngineResponse = CreateEngineResponse
  { createdEngineDesignRaceId :: ComponentId
  , createdEngineDesign :: EngineDesign
  } deriving (Show, Eq, Generic)

instance ToJSON CreateEngineResponse

instance FromJSON CreateEngineResponse

data EngineResearchRequest = EngineResearchRequest
  { rreSystemId   :: StarSystemId
  , rreBodyId     :: BodyId
  , rreResearchId :: Maybe ComponentId
  , rreNumLabs    :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON EngineResearchRequest

instance FromJSON EngineResearchRequest

data EngineResearchResponse = EngineResearchResponse
  { rreRaceId :: ComponentId
  , rreEngineDesigns :: ResearchStatus EngineDesign
  } deriving (Show, Eq, Generic)

instance ToJSON EngineResearchResponse

instance FromJSON EngineResearchResponse

newtype DeleteEngineRequest = DeleteEngineRequest
  { engineDesignId :: ComponentId
  } deriving (Show, Eq, Generic)

instance ToJSON DeleteEngineRequest

instance FromJSON DeleteEngineRequest

data DeleteEngineResponse = DeleteEngineResponse
  { deletedEngineDesignId :: ComponentId
  , deletedEngineDesignRaceId :: ComponentId
  } deriving (Show, Eq, Generic)

instance ToJSON DeleteEngineResponse

instance FromJSON DeleteEngineResponse

engineDesignRoute :: WebRoute ()
engineDesignRoute = do
  get "/api/design/engine" $ do
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    json
      $ (EngineDesignsResponse
          raceId
          (view (research . at' raceId . rEngineDesigns) gameState)
        )
  get "/api/design/engine/:engineDesignId" $ do
    engineDesignIdStr <- param "engineDesignId"
    let engineDesignId' = read engineDesignIdStr
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    let engineDesigns' =
          view (research . at' raceId . rEngineDesigns . unlocked) gameState
    let engineDesign' = Map.lookup engineDesignId' engineDesigns'
    case engineDesign' of
      Just ed -> json (EngineDesignResponse raceId ed)
      Nothing -> status status404
  post "/api/check/design/engine" $ do
    checkEngineRequest <- jsonData
    gameState          <- webM readState
    let modifierId = checkEngineModifier checkEngineRequest
    let sizeId     = checkEngineSize checkEngineRequest
    gen <- liftIO getStdGen
    let (engineName', newGen) = generateEngineName gen
    liftIO $ setStdGen newGen
    json $ getEngineDesign 0 engineName' modifierId sizeId gameState
  post "/api/design/engine" $ do
    createEngineRequest <- jsonData
    gameState           <- webM readState
    let modifierId    = engineModifier createEngineRequest
    let sizeId        = engineSize createEngineRequest
    let name          = engineName createEngineRequest
    let gen           = read (view randomSeed gameState) :: StdGen
    let (edid, gen')  = randomId gen
    let engineDesign' = getEngineDesign edid name modifierId sizeId gameState
    let raceId        = getPlayerRaceId gameState
    webM
      $ modify
      $ (addEngineDesign engineDesign')
      . (set randomSeed $ show gen')
    json $ CreateEngineResponse raceId engineDesign'
  post "/api/research/engine" $ do
    engineResearchRequest <- jsonData
    gameState             <- webM readState
    let sysId       = rreSystemId engineResearchRequest
    let bid         = rreBodyId engineResearchRequest
    let raceId      = getPlayerRaceId gameState
    let
      newGameState = if isJust $ view (systems . at sysId) gameState
        then
          if isJust
             $ view (systems . at' sysId . ssBodies . at bid) gameState
          then
            handleEngineResearch engineResearchRequest gameState
          else
            gameState
        else gameState
    webM $ modify (\_ -> newGameState)
    json $ EngineResearchResponse
      raceId
      (view (research . at' raceId . rEngineDesigns) newGameState)
  delete "/api/design/engine" $ do
    deleteEngineRequest <- jsonData
    let designId = engineDesignId deleteEngineRequest
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    webM $ modify (removeEngineDesign designId)
    json $ DeleteEngineResponse designId raceId

getEngineDesign
  :: ComponentId
  -> String
  -> ComponentId
  -> ComponentId
  -> GameState
  -> EngineDesign
getEngineDesign edid name modifierId sizeId gameState = engineDesign'
 where
  raceId           = getPlayerRaceId gameState
  engineTechnology = latestEngineTechnology gameState
  fuelConsumption  = latestFuelConsumption gameState
  engineSize'      = fromMaybe engineSize1Research $ Map.lookup sizeId $ view
    (research . at' raceId . rEngineSize . unlocked)
    gameState
  powerEfficiencyModifier =
    fromMaybe power1Efficiency1ModifierResearch $ Map.lookup modifierId $ view
      (research . at' raceId . rEngineModifier . unlocked)
      gameState
  engineDesign' = newEngineDesign
    edid
    (ComponentName name)
    engineTechnology
    (view pemrPowerEfficiencyModifier powerEfficiencyModifier)
    fuelConsumption
    (view esrEngineSize engineSize')

addEngineDesign :: EngineDesign -> GameState -> GameState
addEngineDesign ed gs = newGameState
 where
  raceId = getPlayerRaceId gs
  newGameState =
    if isNothing $ view (research . at' raceId . rEngineDesigns . pending) gs
      then set (research . at' raceId . rEngineDesigns . pending) (Just ed) gs
      else over (research . at' raceId . rEngineDesigns . locked)
                (Map.insert engineDesignId' ed)
                gs
  engineDesignId' = view edId ed

removeEngineDesign :: ComponentId -> GameState -> GameState
removeEngineDesign edID gs@(GameState sys rcs raceRes prod ut cfg st seed) =
  newGameState
 where
  newGameState = GameState sys rcs newRaceRes prod ut cfg st seed
  edLocked     = view (rEngineDesigns . locked) res
  edUnlocked   = view (rEngineDesigns . unlocked) res
  edPending    = view (rEngineDesigns . pending) res
  raceId       = getPlayerRaceId gs
  res          = fromJust $ Map.lookup raceId raceRes
  newRaceRes   = Map.insert raceId newRes raceRes
  newRes       = if isNothing $ Map.lookup edID edLocked
    then newRes'
    else set (rEngineDesigns . locked) (Map.delete edID edLocked) res
  newRes' = if isNothing $ Map.lookup edID edUnlocked
    then newRes''
    else set (rEngineDesigns . unlocked) (Map.delete edID edUnlocked) res
  newRes'' = if isJust edPending && edID == (view edId $ fromJust edPending)
    then set (rEngineDesigns . pending) Nothing res
    else res

latestEngineTechnology :: GameState -> EngineTechnology
latestEngineTechnology gameState = engineTechnology
 where
  raceId = getPlayerRaceId gameState
  unlockedET =
    (view (research . at' raceId . rEngineTechnology . unlocked) gameState) :: Map.Map
        Int
        EngineTechnologyResearch
  engineTechnologies =
    (map snd (Map.toList unlockedET)) :: [EngineTechnologyResearch]
  engineTechnologiesResearch =
    (map (view etrResearchCost) engineTechnologies) :: [ComponentResearch]
  temp            = (map NT.unpack engineTechnologiesResearch) :: [Double]
  maxResearchCost = (maximum temp) :: Double
  ind             = (fromJust $ elemIndex maxResearchCost temp) :: Int
  engineTechnology =
    (view etrEngineTechnology $ engineTechnologies !! ind) :: EngineTechnology

latestFuelConsumption :: GameState -> FuelConsumption
latestFuelConsumption gameState = fuelConsumption
 where
  raceId = getPlayerRaceId gameState
  unlockedEFC =
    view (research . at' raceId . rEngineFuelConsumption . unlocked) gameState
  engineFuelConsumptions =
    (map snd (Map.toList unlockedEFC)) :: [FuelConsumptionResearch]
  temp =
    map (view fcrResearchCost) engineFuelConsumptions :: [ComponentResearch]
  engineFuelConsumptionsResearch = (map NT.unpack temp) :: [Double]
  maxResearchCost = maximum engineFuelConsumptionsResearch
  ind = fromJust $ elemIndex maxResearchCost engineFuelConsumptionsResearch
  fuelConsumption = view fcrFuelConsumption $ engineFuelConsumptions !! ind

-- This needs to be cleaned up big time.
-- Lots of this functionality should not be part of the API.
handleEngineResearch :: EngineResearchRequest -> GameState -> GameState
handleEngineResearch (EngineResearchRequest systemId bodyId maybeResearchId numLabs) gs
  = newGameState
 where
  labId = _iId basicResearchLab
  bodyInstallments =
    view (systems . at' systemId . ssBodies . at' bodyId . bInstallments) gs
  maybeBodyResearchLabs = Map.lookup labId bodyInstallments
  bodyResearchLabs      = if isNothing maybeBodyResearchLabs
    then InstallmentStack (InstallmentCount 0) basicResearchLab
    else fromJust maybeBodyResearchLabs
  availableLabs = floor $ NT.unpack $ view isCount bodyResearchLabs
  raceId        = getPlayerRaceId gs
  currentLabs = view (research . at' raceId . rEngineDesigns . researchLabs) gs
  usedLabs      = if elem bodyId (Map.keys currentLabs)
    then fromJust $ Map.lookup bodyId currentLabs
    else 0
  totalLabs        = availableLabs + usedLabs
  newAvailableLabs = availableLabs + (usedLabs - numLabs)
  newGameState =
    if newAvailableLabs + numLabs /= totalLabs || newAvailableLabs < 0
      then gs
      else updatedGameState''
  updatedGameState = if isNothing maybeResearchId
    then gs
    else set (research . at' raceId . rEngineDesigns) newEngineResearch gs
  updatedGameState' =
    if isJust $ view
         ( systems
         . at' systemId
         . ssBodies
         . at' bodyId
         . bInstallments
         . at labId
         )
         updatedGameState
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
        updatedGameState
      else over
        (systems . at' systemId . ssBodies . at' bodyId . bInstallments)
        (Map.insert
          labId
          (InstallmentStack (InstallmentCount $ fromIntegral newAvailableLabs)
                            basicResearchLab
          )
        )
        updatedGameState
  updatedGameState'' = set
    (research . at' raceId . rEngineDesigns . researchLabs . at' bodyId)
    numLabs
    updatedGameState'
  newEngineResearch =
    if isNothing $ view (research . at' raceId . rEngineDesigns . pending) gs
      then newEngineResearch'
      else newEngineResearch''
  researchId            = fromJust maybeResearchId
  currentEngineResearch = view (research . at' raceId . rEngineDesigns) gs
  newEngineResearch'    = set
    pending
    maybeNewPending
    (set (locked . at'' researchId) Nothing currentEngineResearch)
  maybeNewPending =
    view (research . at' raceId . rEngineDesigns . locked . at researchId) gs
  oldPending =
    fromJust $ view (research . at' raceId . rEngineDesigns . pending) gs
  newEngineResearch'' =
    if isNothing maybeNewPending
         || (view edId oldPending)
         == (view edId $ fromJust maybeNewPending)
      then currentEngineResearch
      else set (locked . at' (view edId oldPending))
               oldPending
               newEngineResearch'
