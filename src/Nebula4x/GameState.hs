module Nebula4x.GameState where

import           Control.Lens
import           Control.Monad.State.Strict
import qualified Data.Map.Strict               as Map
import           System.Random

import           Nebula4x.Config
import           Nebula4x.Research
import           Nebula4x.StarSystem
import           Nebula4x.Status
import           Nebula4x.Time
import           Nebula4x.Types

startingGameState :: GameState
startingGameState = GameState Map.empty
                              noRaces
                              startingRaceResearch
                              Map.empty
                              defaultStartTime
                              startingConfig
                              startingStatus
                              "1"

newGameState :: IO GameState
newGameState = do
      g <- getStdGen
      let (randomStarSystem, g') =
                generateStarSystem g (view config startingGameState)
      let
            gs = startingGameState
                  { _systems      =
                        Map.fromList
                              [(view ssId randomStarSystem, randomStarSystem)]
                  , _universeTime = addSeconds (-1) defaultStartTime
                  , _randomSeed   = show (g' :: StdGen)
                  }
        -- Need to tick once to align the orbits
      (newState, _) <- tick 1 gs
      return newState

tickFromString :: String -> GameState -> IO (GameState, GameLogs)
tickFromString timeString = tick (numSeconds $ tickTimeFromString timeString)

--
-- TODO: Dynamically change the base pulse if there's no activity/fighting.
--
tick :: Seconds -> GameState -> IO (GameState, GameLogs)
tick secs gs = liftIO $ (flip runNebula4x) gs (tick' secs >> getGameState)

-- TODO: Look into maybe storing the number of seconds in the state as well.
tick' :: Seconds -> Nebula4x ()
tick' secs = do
      gameState <- getGameState
      let bPulse = view (config . pulseConfig . pcBasePulse) gameState
      if secs < bPulse
            then consumePulse secs 0
            else consumePulse bPulse (secs - bPulse)

consumePulse :: Seconds -> Seconds -> Nebula4x ()
consumePulse secsToPulse secsLeft = do
      gameState <- getGameState
      let (lastShipPulse, nextShipPulse) =
                view (status . pulseStatus . psShipOrder) gameState
      let (lastMisslePulse, nextMisslePulse) =
                view (status . pulseStatus . psMissleWork) gameState
      let (lastShipyardPulse, nextShipyardPulse) =
                view (status . pulseStatus . psShipyardWork) gameState
      let (lastProductionPulse, nextProductionPulse) =
                view (status . pulseStatus . psProductionWork) gameState
      let (lastResearchPulse, nextResearchPulse) =
                view (status . pulseStatus . psResearchWork) gameState
      let (lastMiningPulse, nextMiningPulse) =
                view (status . pulseStatus . psMiningWork) gameState
      let (lastMineralPacketPulse, nextMineralPacketPulse) =
                view (status . pulseStatus . psMineralPacketWork) gameState
      let (lastBodyPulse, nextBodyPulse) =
                view (status . pulseStatus . psBodyPosition) gameState
      let startingUniverseTime = view universeTime gameState
      let newUniverseTime      = addSeconds secsToPulse startingUniverseTime
      let newTime              = timeToInt newUniverseTime
      modifyGameState (set universeTime newUniverseTime)
        -- Check if it's time to pulse each individual entities
        -- tickAllBodies needs to happen before tickAllShips so that orbiting ships can have their locations updated with latest body position.
      when (newTime >= nextBodyPulse || secsLeft <= 0)
           (tickAllBodies $ newTime - lastBodyPulse)
      when (newTime >= nextMiningPulse || secsLeft <= 0)
           (tickAllMiners $ newTime - lastMiningPulse)
      when
            (newTime >= nextMineralPacketPulse || secsLeft <= 0)
            (tickAllMineralPackets newTime $ newTime - lastMineralPacketPulse)
      when (newTime >= nextProductionPulse || secsLeft <= 0)
           (tickAllProduction $ newTime - lastProductionPulse)
      when (newTime >= nextResearchPulse || secsLeft <= 0)
           (tickAllResearch $ newTime - lastResearchPulse)
      when (newTime >= nextShipyardPulse || secsLeft <= 0)
           (tickAllShipyards $ newTime - lastShipyardPulse)
      when (newTime >= nextMisslePulse || secsLeft <= 0)
           (tickAllMissles $ newTime - lastMisslePulse)
      when (newTime >= nextShipPulse || secsLeft <= 0)
           (tickAllShips $ newTime - lastShipPulse)
        -- Continue consuming pulses until none left
      let newSecsToPulse = if secsLeft - secsToPulse >= 0
                then secsToPulse
                else secsLeft
      let newSecsLeft = secsLeft - newSecsToPulse
      when (secsLeft > 0) (consumePulse newSecsToPulse newSecsLeft)

-- TODO: For some reason converting this function to Nebula4x monad operations slows it down significantly.
-- This was tested with 'stack test --profile' went from ~3s to ~4.3s.
-- Worth noting that this was with just a few ships.
-- See shipsAttempt branch.
tickAllShips :: Seconds -> Nebula4x ()
tickAllShips secs = do
      gameState <- getGameState
      let starSystems          = view systems gameState
      let gen                  = read (view randomSeed gameState) :: StdGen
      let (newSystems, newGen) = updateStarSystemsShips gen secs starSystems
      let newSeed              = show newGen :: String
      let currentSeconds       = timeToInt $ view universeTime gameState
      let pulseSeconds = view (config . pulseConfig . pcShipOrder) gameState
      let newGS =
                set randomSeed newSeed
                      . set systems newSystems
                      . set
                              (status . pulseStatus . psShipOrder)
                              (currentSeconds, currentSeconds + pulseSeconds)
                      $ gameState
      setGameState newGS

tickAllMissles :: Seconds -> Nebula4x ()
tickAllMissles secs = do
      gameState <- getGameState
      let gen = read (view randomSeed gameState) :: StdGen
      let (newSystems, newGen) =
                updateStarSystemsMissles gen secs (view systems gameState)
      let newSeed        = show newGen
      let currentSeconds = timeToInt $ view universeTime gameState
      let pulseSeconds = view (config . pulseConfig . pcMissleWork) gameState
      let newGS =
                set randomSeed newSeed
                      . set systems newSystems
                      . set
                              (status . pulseStatus . psMissleWork)
                              (currentSeconds, currentSeconds + pulseSeconds)
                      $ gameState
      setGameState newGS

tickAllShipyards :: Seconds -> Nebula4x ()
tickAllShipyards secs = do
      updateStarSystemsShipyards secs
      gameState <- getGameState
      let currentSeconds = timeToInt $ view universeTime gameState
      let pulseSeconds = view (config . pulseConfig . pcShipyardWork) gameState
      let newGS = set (status . pulseStatus . psShipyardWork)
                      (currentSeconds, currentSeconds + pulseSeconds)
                      gameState
      setGameState newGS

tickAllProduction :: Seconds -> Nebula4x ()
tickAllProduction secs = do
      gameState <- getGameState
      let starSystems = view systems gameState
      let prod        = view production gameState
      let (newProduction, newSystems) =
                updateStarSystemsProduction secs prod starSystems
      let currentSeconds = timeToInt $ view universeTime gameState
      let pulseSeconds =
                view (config . pulseConfig . pcProductionWork) gameState
      let newGS =
                set systems newSystems
                      . set production newProduction
                      . set
                              (status . pulseStatus . psProductionWork)
                              (currentSeconds, currentSeconds + pulseSeconds)
                      $ gameState
      setGameState newGS

tickAllResearch :: Seconds -> Nebula4x ()
tickAllResearch secs = do
      gameState <- getGameState
      let currentSeconds = timeToInt $ view universeTime gameState
      let pulseSeconds = view (config . pulseConfig . pcResearchWork) gameState
      let newGS =
                set (status . pulseStatus . psResearchWork)
                    (currentSeconds, currentSeconds + pulseSeconds)
                      . over research (performRaceResearch secs)
                      $ gameState
      setGameState newGS

tickAllMiners :: Seconds -> Nebula4x ()
tickAllMiners secs = do
      gameState <- getGameState
      let starSystems    = view systems gameState
      let newSystems     = Map.map (updateStarSystemMiners secs) starSystems
      let currentSeconds = timeToInt $ view universeTime gameState
      let pulseSeconds   = view (config . pulseConfig . pcMiningWork) gameState
      let newGS =
                set (status . pulseStatus . psMiningWork)
                    (currentSeconds, currentSeconds + pulseSeconds)
                      . set systems newSystems
                      $ gameState
      setGameState newGS

tickAllMineralPackets :: Seconds -> Seconds -> Nebula4x ()
tickAllMineralPackets newTime secs = do
      gameState <- getGameState
      let starSystems = view systems gameState
      let (lastMassDriverPulse, nextMassDriverPulse) =
                view (status . pulseStatus . psMassDriverWork) gameState
      let mdSecs = newTime - lastMassDriverPulse
      let newSystems = if newTime >= nextMassDriverPulse
                then Map.map (updateMineralPackets secs) $ Map.map
                      (updateStarSystemMassDrivers mdSecs)
                      starSystems
                else Map.map (updateMineralPackets secs) starSystems
      let updatedGameState = if newTime >= nextMassDriverPulse
                then updateMassDriversPulse (set systems newSystems gameState)
                else gameState
      let currentSeconds = timeToInt $ view universeTime updatedGameState
      let pulseSeconds = view
                (config . pulseConfig . pcMineralPacketWork)
                updatedGameState
      let newGS =
                set (status . pulseStatus . psMineralPacketWork)
                    (currentSeconds, currentSeconds + pulseSeconds)
                      . set systems newSystems
                      $ updatedGameState
      setGameState newGS

-- TODO: Performance could be improved ever so slightly by merging this with tickAllMineralPackets.
updateMassDriversPulse :: GameState -> GameState
updateMassDriversPulse gs = newGameState'
   where
      newGameState' = set (status . pulseStatus . psMassDriverWork)
                          (currentSeconds, currentSeconds + pulseSeconds)
                          gs
      currentSeconds = timeToInt $ view universeTime gs
      pulseSeconds   = view (config . pulseConfig . pcMassDriverWork) gs

tickAllBodies :: Seconds -> Nebula4x ()
tickAllBodies _ = do
      gameState <- getGameState
      let newTime        = timeToInt $ view universeTime gameState
      let starSystems    = view systems gameState
      let newSystems     = Map.map (updateStarSystemBodies newTime) starSystems
      let currentSeconds = timeToInt $ view universeTime gameState
      let pulseSeconds = view (config . pulseConfig . pcBodyPosition) gameState
      let newGS =
                set (status . pulseStatus . psBodyPosition)
                    (currentSeconds, currentSeconds + pulseSeconds)
                      . set systems newSystems
                      $ gameState
      setGameState newGS
