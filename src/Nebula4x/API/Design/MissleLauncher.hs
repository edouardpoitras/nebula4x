{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Nebula4x.API.Design.MissleLauncher where

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

data MissleLauncherDesignResponse = MissleLauncherDesignResponse
  { missleLauncherDesignRaceId :: ComponentId
  , missleLauncherDesign :: MissleLauncherDesign
  } deriving (Show, Eq, Generic)

instance ToJSON MissleLauncherDesignResponse

instance FromJSON MissleLauncherDesignResponse

data MissleLauncherDesignsResponse = MissleLauncherDesignsResponse
  { missleLauncherDesignsRaceId :: ComponentId
  , missleLauncherDesigns :: ResearchStatus MissleLauncherDesign
  } deriving (Show, Eq, Generic)

instance ToJSON MissleLauncherDesignsResponse

instance FromJSON MissleLauncherDesignsResponse

data CheckMissleLauncherRequest = CheckMissleLauncherRequest
  { checkMissleLauncherSize        :: ComponentId
  , checkMissleLauncherReducedSize :: ComponentId
  } deriving (Show, Eq, Generic)

instance ToJSON CheckMissleLauncherRequest

instance FromJSON CheckMissleLauncherRequest

data CreateMissleLauncherRequest = CreateMissleLauncherRequest
  { missleLauncherSize        :: ComponentId
  , missleLauncherReducedSize :: ComponentId
  , missleLauncherName        :: String
  } deriving (Show, Eq, Generic)

instance ToJSON CreateMissleLauncherRequest

instance FromJSON CreateMissleLauncherRequest

data CreateMissleLauncherResponse = CreateMissleLauncherResponse
  { createdMissleLauncherDesignRaceId :: ComponentId
  , createdMissleLauncherDesign :: MissleLauncherDesign
  } deriving (Show, Eq, Generic)

instance ToJSON CreateMissleLauncherResponse

instance FromJSON CreateMissleLauncherResponse

data MissleLauncherResearchRequest = MissleLauncherResearchRequest
  { rrmlSystemId   :: StarSystemId
  , rrmlBodyId     :: BodyId
  , rrmlResearchId :: Maybe ComponentId
  , rrmlNumLabs    :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON MissleLauncherResearchRequest

instance FromJSON MissleLauncherResearchRequest

data MissleLauncherResponse = MissleLauncherResponse
  { rrmlRaceId :: ComponentId
  , rrmlMissleLauncherDesigns :: ResearchStatus MissleLauncherDesign
  } deriving (Show, Eq, Generic)

instance ToJSON MissleLauncherResponse

instance FromJSON MissleLauncherResponse

newtype DeleteMissleLauncherRequest = DeleteMissleLauncherRequest
  { missleLauncherDesignId :: ComponentId
  } deriving (Show, Eq, Generic)

instance ToJSON DeleteMissleLauncherRequest

instance FromJSON DeleteMissleLauncherRequest

data DeleteMissleLauncherResponse = DeleteMissleLauncherResponse
  { deletedMissleLauncherDesignId :: ComponentId
  , deletedMissleLauncherDesignRaceId :: ComponentId
  } deriving (Show, Eq, Generic)

instance ToJSON DeleteMissleLauncherResponse

instance FromJSON DeleteMissleLauncherResponse

missleLauncherDesignRoute :: WebRoute ()
missleLauncherDesignRoute = do
  get "/api/design/missle-launcher" $ do
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    json $ MissleLauncherDesignsResponse
      raceId
      (view (research . at' raceId . rMissleLauncherDesigns) gameState)
  get "/api/design/missle-launcher/:missleLauncherDesignId" $ do
    missleLauncherDesignIdStr <- param "missleLauncherDesignId"
    let missleLauncherDesignId' = read missleLauncherDesignIdStr
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    let missleLauncherDesigns' = view
          (research . at' raceId . rMissleLauncherDesigns . unlocked)
          gameState
    let missleLauncherDesign' =
          Map.lookup missleLauncherDesignId' missleLauncherDesigns'
    case missleLauncherDesign' of
      Just mld -> json $ MissleLauncherDesignResponse raceId mld
      Nothing  -> status status404
  post "/api/check/design/missle-launcher" $ do
    checkMissleLauncherRequest <- jsonData
    gameState                  <- webM readState
    let sizeId = checkMissleLauncherSize checkMissleLauncherRequest
    let reducedSizeId =
          checkMissleLauncherReducedSize checkMissleLauncherRequest
    gen <- liftIO getStdGen
    let (missleLauncherName', newGen) = generateMissleLauncherName gen
    liftIO $ setStdGen newGen
    json $ getMissleLauncherDesign 0
                                   missleLauncherName'
                                   sizeId
                                   reducedSizeId
                                   gameState
  post "/api/design/missle-launcher" $ do
    createMissleLauncherRequest <- jsonData
    gameState                   <- webM readState
    let sizeId = missleLauncherSize createMissleLauncherRequest
    let reducedSizeId = missleLauncherReducedSize createMissleLauncherRequest
    let name = missleLauncherName createMissleLauncherRequest
    let gen           = read (view randomSeed gameState) :: StdGen
    let (mldid, gen') = randomId gen
    let raceId        = getPlayerRaceId gameState
    let missleLauncherDesign' =
          getMissleLauncherDesign mldid name sizeId reducedSizeId gameState
    webM
      $ modify
      $ (addMissleLauncherDesign missleLauncherDesign')
      . (set randomSeed $ show gen')
    json $ CreateMissleLauncherResponse raceId missleLauncherDesign'
  post "/api/research/missle-launcher" $ do
    missleLauncherResearchRequest <- jsonData
    gameState                     <- webM readState
    let raceId = getPlayerRaceId gameState
    let sysId  = rrmlSystemId missleLauncherResearchRequest
    let bid    = rrmlBodyId missleLauncherResearchRequest
    let
      newGameState = if isJust $ view (systems . at sysId) gameState
        then
          if isJust $ view (systems . at' sysId . ssBodies . at bid) gameState
            then handleMissleLauncherResearch missleLauncherResearchRequest
                                              gameState
            else gameState
        else gameState
    webM $ modify (\_ -> newGameState)
    json $ MissleLauncherResponse
      raceId
      (view (research . at' raceId . rMissleLauncherDesigns) newGameState)
  delete "/api/design/missle-launcher" $ do
    deleteMissleLauncherRequest <- jsonData
    let designId = missleLauncherDesignId deleteMissleLauncherRequest
    gameState <- webM readState
    let raceId = getPlayerRaceId gameState
    webM $ modify (removeMissleLauncherDesign designId)
    json $ DeleteMissleLauncherResponse designId raceId

getMissleLauncherDesign
  :: ComponentId
  -> String
  -> ComponentId
  -> ComponentId
  -> GameState
  -> MissleLauncherDesign
getMissleLauncherDesign mldid name sizeId reducedSizeId gameState =
  missleLauncherDesign'
 where
  raceId                    = getPlayerRaceId gameState
  missleLauncherReloadRate' = latestMissleLauncherReloadRate gameState
  missleLauncherSize' =
    fromMaybe missleLauncherSize1Research $ Map.lookup sizeId $ view
      (research . at' raceId . rMissleLauncherSize . unlocked)
      gameState
  missleLauncherReducedSize' =
    fromMaybe missleLauncherReducedSize1Research
      $ Map.lookup reducedSizeId
      $ view (research . at' raceId . rMissleLauncherReducedSize . unlocked)
             gameState
  missleLauncherDesign' = newMissleLauncherDesign
    mldid
    (ComponentName name)
    (view mlsrMissleLauncherSize missleLauncherSize')
    missleLauncherReloadRate'
    (view mlrsrMissleLauncherReducedSize missleLauncherReducedSize')

addMissleLauncherDesign :: MissleLauncherDesign -> GameState -> GameState
addMissleLauncherDesign mld gs = newGameState
 where
  raceId = getPlayerRaceId gs
  newGameState =
    if isNothing
       $ view (research . at' raceId . rMissleLauncherDesigns . pending) gs
    then
      set (research . at' raceId . rMissleLauncherDesigns . pending)
          (Just mld)
          gs
    else
      over (research . at' raceId . rMissleLauncherDesigns . locked)
           (Map.insert missleLauncherDesignId' mld)
           gs
  missleLauncherDesignId' = view mldId mld

removeMissleLauncherDesign :: ComponentId -> GameState -> GameState
removeMissleLauncherDesign mldID gs@(GameState sys rcs raceRes prod ut cfg st seed)
  = newGameState
 where
  newGameState = GameState sys rcs newRaceRes prod ut cfg st seed
  mldLocked    = view (rMissleLauncherDesigns . locked) res
  mldUnlocked  = view (rMissleLauncherDesigns . unlocked) res
  mldPending   = view (rMissleLauncherDesigns . pending) res
  raceId       = getPlayerRaceId gs
  res          = fromJust $ Map.lookup raceId raceRes
  newRaceRes   = Map.insert raceId newRes raceRes
  newRes       = if isNothing $ Map.lookup mldID mldLocked
    then newRes'
    else set (rMissleLauncherDesigns . locked) (Map.delete mldID mldLocked) res
  newRes' = if isNothing $ Map.lookup mldID mldUnlocked
    then newRes''
    else set (rMissleLauncherDesigns . unlocked)
             (Map.delete mldID mldUnlocked)
             res
  newRes'' =
    if isJust mldPending && mldID == (view mldId $ fromJust mldPending)
      then set (rMissleLauncherDesigns . pending) Nothing res
      else res

latestMissleLauncherReloadRate :: GameState -> MissleLauncherReloadRate
latestMissleLauncherReloadRate gameState = missleLauncherReloadRate
 where
  raceId = getPlayerRaceId gameState
  unlockedMLRR =
    (view (research . at' raceId . rMissleLauncherReloadRate . unlocked)
          gameState
    ) :: Map.Map Int MissleLauncherReloadRateResearch
  missleLauncherReloadRates =
    (map snd (Map.toList unlockedMLRR)) :: [MissleLauncherReloadRateResearch]
  missleLauncherReloadRatesResearch =
    (map (view mlrrrResearchCost) missleLauncherReloadRates) :: [ ComponentResearch
      ]
  temp = (map NT.unpack missleLauncherReloadRatesResearch) :: [Double]
  maxResearchCost = (maximum temp) :: Double
  ind = (fromJust $ elemIndex maxResearchCost temp) :: Int
  missleLauncherReloadRate =
    (view mlrrrMissleLauncherReloadRate $ missleLauncherReloadRates !! ind) :: MissleLauncherReloadRate

-- This needs to be cleaned up big time.
-- Lots of this functionality should not be part of the API.
handleMissleLauncherResearch
  :: MissleLauncherResearchRequest -> GameState -> GameState
handleMissleLauncherResearch (MissleLauncherResearchRequest systemId bodyId maybeResearchId numLabs) gs
  = newGameState
 where
  raceId = getPlayerRaceId gs
  labId  = _iId basicResearchLab
  bodyInstallments =
    view (systems . at' systemId . ssBodies . at' bodyId . bInstallments) gs
  maybeBodyResearchLabs = Map.lookup labId bodyInstallments
  bodyResearchLabs      = if isNothing maybeBodyResearchLabs
    then InstallmentStack (InstallmentCount 0) basicResearchLab
    else fromJust maybeBodyResearchLabs
  availableLabs = floor $ NT.unpack $ view isCount bodyResearchLabs
  currentLabs =
    view (research . at' raceId . rMissleLauncherDesigns . researchLabs) gs
  usedLabs = if elem bodyId (Map.keys currentLabs)
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
    else set (research . at' raceId . rMissleLauncherDesigns)
             newMissleLauncherResearch
             gs
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
    (research . at' raceId . rMissleLauncherDesigns . researchLabs . at' bodyId)
    numLabs
    updatedGameState'
  newMissleLauncherResearch =
    if isNothing
       $ view (research . at' raceId . rMissleLauncherDesigns . pending) gs
    then
      newMissleLauncherResearch'
    else
      newMissleLauncherResearch''
  researchId = fromJust maybeResearchId
  currentMissleLauncherResearch =
    view (research . at' raceId . rMissleLauncherDesigns) gs
  newMissleLauncherResearch' = set
    pending
    maybeNewPending
    (set (locked . at'' researchId) Nothing currentMissleLauncherResearch)
  maybeNewPending = view
    (research . at' raceId . rMissleLauncherDesigns . locked . at researchId)
    gs
  oldPending = fromJust
    $ view (research . at' raceId . rMissleLauncherDesigns . pending) gs
  newMissleLauncherResearch'' =
    if isNothing maybeNewPending
         || (view mldId oldPending)
         == (view mldId $ fromJust maybeNewPending)
      then currentMissleLauncherResearch
      else set (locked . at' (view mldId oldPending))
               oldPending
               newMissleLauncherResearch'
