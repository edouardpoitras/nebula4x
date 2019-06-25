{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Nebula4x.API.Design.Laser where

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

data LaserDesignResponse = LaserDesignResponse
  { laserDesignRaceId :: ComponentId
  , laserDesign :: LaserDesign
  } deriving (Show, Eq, Generic)

instance ToJSON LaserDesignResponse

instance FromJSON LaserDesignResponse

data LaserDesignsResponse = LaserDesignsResponse
  { laserDesignsRaceId :: ComponentId
  , laserDesigns :: ResearchStatus LaserDesign
  } deriving (Show, Eq, Generic)

instance ToJSON LaserDesignsResponse

instance FromJSON LaserDesignsResponse

data CheckLaserRequest = CheckLaserRequest
  { checkLaserFocalSize        :: ComponentId
  , checkLaserReducedSize :: ComponentId
  } deriving (Show, Eq, Generic)

instance ToJSON CheckLaserRequest

instance FromJSON CheckLaserRequest

data CreateLaserRequest = CreateLaserRequest
  { laserFocalSize        :: ComponentId
  , laserReducedSize :: ComponentId
  , laserName        :: String
  } deriving (Show, Eq, Generic)

instance ToJSON CreateLaserRequest

instance FromJSON CreateLaserRequest

data CreateLaserResponse = CreateLaserResponse
  { createdLaserDesignRaceId :: ComponentId
  , createdLaserDesign :: LaserDesign
  } deriving (Show, Eq, Generic)

instance ToJSON CreateLaserResponse

instance FromJSON CreateLaserResponse

data LaserResearchRequest = LaserResearchRequest
  { rrlSystemId   :: StarSystemId
  , rrlBodyId     :: BodyId
  , rrlResearchId :: Maybe ComponentId
  , rrlNumLabs    :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON LaserResearchRequest

instance FromJSON LaserResearchRequest

data LaserResearchResponse = LaserResearchResponse
  { rrlRaceId :: ComponentId
  , rrlLaserDesigns :: ResearchStatus LaserDesign
  } deriving (Show, Eq, Generic)

instance ToJSON LaserResearchResponse

instance FromJSON LaserResearchResponse

newtype DeleteLaserRequest = DeleteLaserRequest
  { laserDesignId :: ComponentId
  } deriving (Show, Eq, Generic)

instance ToJSON DeleteLaserRequest

instance FromJSON DeleteLaserRequest

data DeleteLaserResponse = DeleteLaserResponse
  { deletedLaserDesignId :: ComponentId
  , deletedLaserDesignRaceId :: ComponentId
  } deriving (Show, Eq, Generic)

instance ToJSON DeleteLaserResponse

instance FromJSON DeleteLaserResponse

laserDesignRoute :: WebRoute ()
laserDesignRoute = do
  get "/api/design/laser" $ do
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    json $ LaserDesignsResponse
      raceId
      (view (research . at' raceId . rLaserDesigns) gameState)
  get "/api/design/laser/:laserDesignId" $ do
    laserDesignIdStr <- param "laserDesignId"
    let laserDesignId' = read laserDesignIdStr
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    let laserDesigns' =
          view (research . at' raceId . rLaserDesigns . unlocked) gameState
    let laserDesign' = Map.lookup laserDesignId' laserDesigns'
    case laserDesign' of
      Just mld -> json $ LaserDesignResponse raceId mld
      Nothing  -> status status404
  post "/api/check/design/laser" $ do
    checkLaserRequest <- jsonData
    gameState         <- webM readState
    let focalSizeId   = checkLaserFocalSize checkLaserRequest
    let reducedSizeId = checkLaserReducedSize checkLaserRequest
    gen <- liftIO getStdGen
    let (laserName', newGen) = generateLaserName gen
    liftIO $ setStdGen newGen
    json $ getLaserDesign 0 laserName' focalSizeId reducedSizeId gameState
  post "/api/design/laser" $ do
    createLaserRequest <- jsonData
    gameState          <- webM readState
    let focalSizeId   = laserFocalSize createLaserRequest
    let reducedSizeId = laserReducedSize createLaserRequest
    let name          = laserName createLaserRequest
    let gen           = read (view randomSeed gameState) :: StdGen
    let (ldid, gen')  = randomId gen
    let raceId        = getPlayerRaceId gameState
    let laserDesign' =
          getLaserDesign ldid name focalSizeId reducedSizeId gameState
    webM $ modify $ (addLaserDesign laserDesign') . (set randomSeed $ show gen')
    json $ CreateLaserResponse raceId laserDesign'
  post "/api/research/laser" $ do
    laserResearchRequest <- jsonData
    gameState            <- webM readState
    let raceId = getPlayerRaceId gameState
    let sysId  = rrlSystemId laserResearchRequest
    let bid    = rrlBodyId laserResearchRequest
    let newGameState = if isJust $ view (systems . at sysId) gameState
          then
            if isJust $ view (systems . at' sysId . ssBodies . at bid) gameState
              then handleLaserResearch laserResearchRequest gameState
              else gameState
          else gameState
    webM $ modify (\_ -> newGameState)
    json $ LaserResearchResponse
      raceId
      (view (research . at' raceId . rLaserDesigns) newGameState)
  delete "/api/design/laser" $ do
    deleteLaserRequest <- jsonData
    let designId = laserDesignId deleteLaserRequest
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    webM $ modify (removeLaserDesign designId)
    json $ DeleteLaserResponse designId raceId

getLaserDesign
  :: ComponentId
  -> String
  -> ComponentId
  -> ComponentId
  -> GameState
  -> LaserDesign
getLaserDesign ldid name focalSizeId reducedSizeId gameState = laserDesign'
 where
  raceId             = getPlayerRaceId gameState
  laserWavelength'   = latestLaserWavelength gameState
  laserRechargeRate' = latestLaserRechargeRate gameState
  laserSize' =
    fromMaybe laserFocalSize10Research $ Map.lookup focalSizeId $ view
      (research . at' raceId . rLaserFocalSize . unlocked)
      gameState
  laserReducedSize' =
    fromMaybe laserReducedSize1Research $ Map.lookup reducedSizeId $ view
      (research . at' raceId . rLaserReducedSize . unlocked)
      gameState
  laserDesign' = newLaserDesign ldid
                                (ComponentName name)
                                (view lfsrLaserFocalSize laserSize')
                                laserWavelength'
                                laserRechargeRate'
                                (view lrsrLaserReducedSize laserReducedSize')

addLaserDesign :: LaserDesign -> GameState -> GameState
addLaserDesign ld gs = newGameState
 where
  raceId = getPlayerRaceId gs
  newGameState =
    if isNothing $ view (research . at' raceId . rLaserDesigns . pending) gs
      then set (research . at' raceId . rLaserDesigns . pending) (Just ld) gs
      else over (research . at' raceId . rLaserDesigns . locked)
                (Map.insert laserDesignId' ld)
                gs
  laserDesignId' = view ldId ld

removeLaserDesign :: ComponentId -> GameState -> GameState
removeLaserDesign ldID gs@(GameState sys rcs raceRes prod ut cfg st seed) =
  newGameState
 where
  newGameState = GameState sys rcs newRaceRes prod ut cfg st seed
  ldLocked     = view (rLaserDesigns . locked) res
  ldUnlocked   = view (rLaserDesigns . unlocked) res
  ldPending    = view (rLaserDesigns . pending) res
  raceId       = getPlayerRaceId gs
  res          = fromJust $ Map.lookup raceId raceRes
  newRaceRes   = Map.insert raceId newRes raceRes
  newRes       = if isNothing $ Map.lookup ldID ldLocked
    then newRes'
    else set (rLaserDesigns . locked) (Map.delete ldID ldLocked) res
  newRes' = if isNothing $ Map.lookup ldID ldUnlocked
    then newRes''
    else set (rLaserDesigns . unlocked) (Map.delete ldID ldUnlocked) res
  newRes'' = if isJust ldPending && ldID == (view ldId $ fromJust ldPending)
    then set (rLaserDesigns . pending) Nothing res
    else res

latestLaserWavelength :: GameState -> LaserWavelength
latestLaserWavelength gameState = laserWavelength
 where
  raceId = getPlayerRaceId gameState
  unlockedLW =
    (view (research . at' raceId . rLaserWavelength . unlocked) gameState) :: Map.Map
        Int
        LaserWavelengthResearch
  laserWavelengths =
    (map snd (Map.toList unlockedLW)) :: [LaserWavelengthResearch]
  laserWavelengthsResearch =
    (map (view lwrResearchCost) laserWavelengths) :: [ComponentResearch]
  temp            = (map NT.unpack laserWavelengthsResearch) :: [Double]
  maxResearchCost = (maximum temp) :: Double
  ind             = (fromJust $ elemIndex maxResearchCost temp) :: Int
  laserWavelength =
    (view lwrLaserWavelength $ laserWavelengths !! ind) :: LaserWavelength

latestLaserRechargeRate :: GameState -> LaserRechargeRate
latestLaserRechargeRate gameState = laserRechargeRate
 where
  raceId = getPlayerRaceId gameState
  unlockedLRR =
    (view (research . at' raceId . rLaserRechargeRate . unlocked) gameState) :: Map.Map
        Int
        LaserRechargeRateResearch
  laserRechargeRates =
    (map snd (Map.toList unlockedLRR)) :: [LaserRechargeRateResearch]
  laserRechargeRatesResearch =
    (map (view lrrrResearchCost) laserRechargeRates) :: [ComponentResearch]
  temp            = (map NT.unpack laserRechargeRatesResearch) :: [Double]
  maxResearchCost = (maximum temp) :: Double
  ind             = (fromJust $ elemIndex maxResearchCost temp) :: Int
  laserRechargeRate =
    (view lrrrLaserRechargeRate $ laserRechargeRates !! ind) :: LaserRechargeRate

-- This needs to be cleaned up big time.
-- Lots of this functionality should not be part of the API.
handleLaserResearch :: LaserResearchRequest -> GameState -> GameState
handleLaserResearch (LaserResearchRequest systemId bodyId maybeResearchId numLabs) gs
  = newGameState
 where
  labId = _iId basicResearchLab
  bodyInstallments =
    view (systems . at' systemId . ssBodies . at' bodyId . bInstallments) gs
  maybeBodyResearchLabs = Map.lookup labId bodyInstallments
  bodyResearchLabs      = if isNothing maybeBodyResearchLabs
    then InstallmentStack (InstallmentCount 0) basicResearchLab
    else fromJust maybeBodyResearchLabs
  raceId        = getPlayerRaceId gs
  availableLabs = floor $ NT.unpack $ view isCount bodyResearchLabs
  currentLabs   = view (research . at' raceId . rLaserDesigns . researchLabs) gs
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
    else set (research . at' raceId . rLaserDesigns) newLaserResearch gs
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
    (research . at' raceId . rLaserDesigns . researchLabs . at' bodyId)
    numLabs
    updatedGameState'
  newLaserResearch =
    if isNothing $ view (research . at' raceId . rLaserDesigns . pending) gs
      then newLaserResearch'
      else newLaserResearch''
  researchId           = fromJust maybeResearchId
  currentLaserResearch = view (research . at' raceId . rLaserDesigns) gs
  newLaserResearch'    = set
    pending
    maybeNewPending
    (set (locked . at'' researchId) Nothing currentLaserResearch)
  maybeNewPending =
    view (research . at' raceId . rLaserDesigns . locked . at researchId) gs
  oldPending =
    fromJust $ view (research . at' raceId . rLaserDesigns . pending) gs
  newLaserResearch'' =
    if isNothing maybeNewPending
         || (view ldId oldPending)
         == (view ldId $ fromJust maybeNewPending)
      then currentLaserResearch
      else set (locked . at' (view ldId oldPending))
               oldPending
               newLaserResearch'
