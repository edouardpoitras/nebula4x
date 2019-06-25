module Nebula4x.StarSystem where

import           Control.Lens
import           Control.Monad
import           Control.Monad.ST
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           Data.STRef
import           System.Random

import           Nebula4x.Body.Body
import           Nebula4x.Component.MissleLauncher
import           Nebula4x.Body.Star
import           Nebula4x.Config
import           Nebula4x.Ship
import           Nebula4x.Types
import           Nebula4x.Utils

generateStarSystems :: RandomGen g => g -> Config -> Int -> (StarSystems, g)
generateStarSystems g conf num = generateStarSystems' g conf num Map.empty

generateStarSystems'
  :: RandomGen g => g -> Config -> Int -> StarSystems -> (StarSystems, g)
generateStarSystems' g _    0       accumulator = (accumulator, g)
generateStarSystems' g conf numLeft accumulator = generateStarSystems'
  newGen
  conf
  (numLeft - 1)
  newAccumulator
 where
  newAccumulator =
    Map.insert (view ssId newStarSystem) newStarSystem accumulator
  (newStarSystem, newGen) = generateStarSystem g conf

generateStarSystem :: RandomGen g => g -> Config -> (StarSystem, g)
generateStarSystem g conf =
  ( StarSystem ssid
               star
               systemBodies
               noPackets
               noShips
               noMissleSalvos
               noWormholes
               notDiscovered
  , asteroidsG
  )
 where
  (ssid     , ssidG     ) = randomId g
  (star     , starG     ) = generateStar ssidG (view starConfig conf)
  (bodies   , bodiesG   ) = generatePlanetsAndMoons starG conf
  (comets   , cometsG   ) = generateComets bodiesG conf
  (asteroids, asteroidsG) = generateAsteroidBelts cometsG conf
  systemBodies            = Map.union asteroids (Map.union bodies comets)

updateStarSystemBodies :: Second -> StarSystem -> StarSystem
updateStarSystemBodies currentTime ss = starSystem
 where
  newBodies  = Map.map (updateStarSystemBody currentTime ss) (view ssBodies ss)
  starSystem = ss { _ssBodies = newBodies }

updateStarSystemBody :: Second -> StarSystem -> Body -> Body
updateStarSystemBody currentTime ss body = newBody where
  (parentBodyMass, parentBodyLocation) =
    if isNothing maybeParentBodyId || isNothing maybeParentBody
      then (view starMass $ view ssStar ss, BodyLocation 0 0)
      else (view bMass parentBody, view bLocation parentBody)
  maybeParentBodyId = view bParentBody body
  maybeParentBody   = Map.lookup (fromJust maybeParentBodyId) (view ssBodies ss)
  parentBody        = fromJust maybeParentBody
  newBody =
    updateBodyPosition parentBodyLocation parentBodyMass currentTime body

updateStarSystemsProduction
  :: Seconds -> Production -> StarSystems -> (Production, StarSystems)
updateStarSystemsProduction secs prod systems' =
  Map.foldr (updateStarSystemsProduction' secs) (prod, systems') systems'

updateStarSystemsProduction'
  :: Seconds
  -> StarSystem
  -> (Production, StarSystems)
  -> (Production, StarSystems)
updateStarSystemsProduction' secs system (prod, systems') =
  (newProd, newSystems) where
  (newProd, newSystem) = updateStarSystemBodyProduction secs prod system
  systemId             = view ssId system
  newSystems           = Map.insert systemId newSystem systems'

updateStarSystemBodyProduction
  :: Seconds -> Production -> StarSystem -> (Production, StarSystem)
updateStarSystemBodyProduction secs prod system = foldr
  (updateStarSystemBodyProduction' secs)
  (prod, system)
  (Map.keys $ view ssBodies system)

updateStarSystemBodyProduction'
  :: Seconds -> BodyId -> (Production, StarSystem) -> (Production, StarSystem)
updateStarSystemBodyProduction' secs bodyId (prod, system) =
  (newProd, newSystem)
 where
  maybeBodyProd        = Map.lookup bodyId prod
  (newProd, newSystem) = if isNothing maybeBodyProd
    then (prod, system)
    else (Map.insert bodyId newBodyProd prod, set ssBodies newPlanets system)
  body = fromJust $ Map.lookup bodyId (view ssBodies system)
  (newBodyProd, newBody) =
    updateBodyProduction secs (fromJust maybeBodyProd) body
  newPlanets = Map.insert bodyId newBody (view ssBodies system)

updateStarSystemMiners :: Seconds -> StarSystem -> StarSystem
updateStarSystemMiners secs ss = starSystem where
  bodies     = view ssBodies ss
  newBodies  = Map.map (updateBodyMiners secs) bodies
  starSystem = ss { _ssBodies = newBodies }

updateStarSystemMassDrivers :: Seconds -> StarSystem -> StarSystem
updateStarSystemMassDrivers secs ss = newStarSystem where -- TODO: Checking every single body for a mass driver is not efficient.
  pckts                = view ssMineralPackets ss
  bodies               = view ssBodies ss
  generatedBodyPackets = map fromJust $ filter isJust $ Map.elems $ Map.map
    (generateNewMineralPacket secs)
    bodies
  newBodyPackets = map fst generatedBodyPackets
  newBodies =
    Map.fromList (map (\(_, bdy) -> (view bId bdy, bdy)) generatedBodyPackets)
  newStarSystem = ss { _ssBodies         = Map.union newBodies bodies
                     , _ssMineralPackets = pckts ++ newBodyPackets
                     }

updateMineralPackets :: Seconds -> StarSystem -> StarSystem
updateMineralPackets secs ss = newStarSystem where
  pckts                   = view ssMineralPackets ss
  bodies                  = view ssBodies ss
  (newBodies, newPackets) = updateBodiesMineralPackets secs bodies pckts
  newStarSystem = ss { _ssBodies = newBodies, _ssMineralPackets = newPackets }

updateStarSystemsMissles
  :: RandomGen g => g -> Seconds -> StarSystems -> (StarSystems, g)
updateStarSystemsMissles gen secs systems' = runST $ do
  systemsRef <- newSTRef (systems', gen)
  updateStarSystemsMissles' secs systemsRef

updateStarSystemsMissles'
  :: RandomGen g => Seconds -> STRef s (StarSystems, g) -> ST s (StarSystems, g)
updateStarSystemsMissles' secs systemsRef = do
  (sss, _) <- readSTRef systemsRef
  forM_ sss $ \system -> when
    (length (view ssMissleSalvos system) > 0)
    (do
      (systems', gen) <- readSTRef systemsRef
      let (newSystem, newGen) = updateStarSystemMissles gen secs system
      let newSystems = Map.insert (view ssId newSystem) newSystem systems'
      writeSTRef systemsRef (newSystems, newGen)
    )
  readSTRef systemsRef

updateStarSystemMissles
  :: RandomGen g => g -> Seconds -> StarSystem -> (StarSystem, g)
updateStarSystemMissles gen secs ss = (starSystem, newGen) where
  bodies = view ssBodies ss
  shps   = view ssShips ss
  salvos = view ssMissleSalvos ss
  (newShips, newBodies, newSalvos, newGen) =
    updateStarSystemMissles' gen secs salvos shps bodies []
  starSystem = ss { _ssShips        = newShips
                  , _ssBodies       = newBodies
                  , _ssMissleSalvos = newSalvos
                  }

updateStarSystemMissles'
  :: RandomGen g
  => g
  -> Seconds
  -> MissleSalvos
  -> Ships
  -> Bodies
  -> MissleSalvos
  -> (Ships, Bodies, MissleSalvos, g)
updateStarSystemMissles' gen _ [] shipAcc bodyAcc salvoAcc =
  (shipAcc, bodyAcc, salvoAcc, gen)
updateStarSystemMissles' gen secs (salvo : salvos) shipAcc bodyAcc salvoAcc =
  updateStarSystemMissles' gen' secs salvos newShipAcc newBodyAcc newSalvoAcc
 where
  (newShipAcc, newBodyAcc, maybeUpdatedMissle, gen') =
    updateMissle gen secs shipAcc bodyAcc salvo
  newSalvoAcc = if isNothing maybeUpdatedMissle
    then salvoAcc
    else (fromJust maybeUpdatedMissle) : salvoAcc

updateStarSystemsShipyards :: Seconds -> Nebula4x ()
updateStarSystemsShipyards secs = do
  gameState <- getGameState
  forM_ (view systems gameState) $ \system -> do
    -- TODO: Could cache whether or not we have shipyards at the system level to improve performance.
    -- IE: when (length (view ssShipyards system) > 0)...
    let systemId = view ssId system
    forM_ (view ssBodies system) $ \body -> do
      let shipyards = view bShipyards body
      if shipyards == Map.empty
      then return ()
      else do
        let bodyId = view bId body
        updateBodyShipyards secs systemId bodyId

updateStarSystemsShips
  :: RandomGen g => g -> Seconds -> StarSystems -> (StarSystems, g)
updateStarSystemsShips gen secs starSystems = (newSystems, newGen)
 where
  ((systems', shipTransits), newGen) = Map.foldl
    (\acc ss -> updateStarSystemShips secs ss acc)
    ((starSystems, Map.empty), gen)
    starSystems
  newSystems = resolveShipTransits shipTransits systems'

updateStarSystemShips
  :: RandomGen g
  => Seconds
  -> StarSystem
  -> ((StarSystems, ShipTransits), g)
  -> ((StarSystems, ShipTransits), g)
updateStarSystemShips secs ss ((systems', transits), gen)
  = if shps == Map.empty
    then ((systems', transits), gen)
    else ((newStarSystems, shipTransits), newGen)
 where
  shps = view ssShips ss
  (newStarSystem, shipTransits, newGen) =
    foldr (updateStarSystemShips' secs) (ss, transits, gen) (Map.keys shps)
  newStarSystems = Map.insert (view ssId ss) newStarSystem systems'

updateStarSystemShips'
  :: RandomGen g
  => Seconds
  -> ShipId
  -> (StarSystem, ShipTransits, g)
  -> (StarSystem, ShipTransits, g)
updateStarSystemShips' secs sid (starSystem, shipTransits, gen) =
  (newStarSystem, newShipTransits, newGen)
 where
  (newStarSystem, newShipTransits, newGen) =
    processShipTasks gen secs sid (starSystem, shipTransits)

resolveShipTransits :: ShipTransits -> StarSystems -> StarSystems
resolveShipTransits transits systems' = newSystems
 where
  newSystems = Map.foldl handleTransit systems' transits
  handleTransit systems'' (ShipTransit sid whid ship) = newSystems'
   where
    maybeSystem   = Map.lookup sid systems''
    system'       = fromJust maybeSystem
    maybeWormhole = Map.lookup whid (view ssWormholes system')
    raceId        = view sRace ship
    newSystems'   = if isNothing maybeSystem || isNothing maybeWormhole
      then systems''
      else Map.insert sid
                      (set (ssDiscovered . at' raceId) True newSystem)
                      systems''
    newSystem = over (ssWormholes . at' whid)
                     (set wSurveyed True)
                     (over ssShips (Map.insert (view sId ship) ship) system')
