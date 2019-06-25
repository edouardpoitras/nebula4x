module Nebula4x.Wormhole where

import           Control.Lens
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           System.Random
import           System.Random.Shuffle

import           Nebula4x.Config
import           Nebula4x.Types

generateWormholes :: RandomGen g => g -> StarSystems -> (StarSystems, g)
generateWormholes gen sss = (newSystems, newGen)
 where
  (gen', gen'')         = split gen
  starIds               = Map.keys sss
  shuffledStarIds       = shuffle' starIds (length starIds) gen''
  (systems', _, gen''') = foldl genSystemWormholes
                                (sss, head shuffledStarIds, gen')
                                (tail shuffledStarIds)
  (newSystems, newGen) = populateWormholes gen''' systems'

genSystemWormholes
  :: RandomGen g
  => (StarSystems, StarSystemId, g)
  -> StarSystemId
  -> (StarSystems, StarSystemId, g)
genSystemWormholes (sss, lastStarId, gen) ssid = (newSystems, ssid, newGen)
 where
  currentSystem = fromJust $ Map.lookup ssid sss
  lastSystem    = fromJust $ Map.lookup lastStarId sss
  -- Add mandatory wormhole that connects all stars in galaxy
  (currentSystem', lastSystem', gen') =
    addSystemWormhole gen currentSystem lastSystem
  systems' =
    Map.insert lastStarId lastSystem' (Map.insert ssid currentSystem' sss)
  -- Check if we need to add any more active wormholes
  (newWormholeChance, gen'' ) = randomR (1 :: Int, 100 :: Int) gen'
  (newSystems       , newGen) = if newWormholeChance < 25
    then addRandomSystemWormhole gen'' ssid systems'
    else (systems', gen'')

populateWormholes :: RandomGen g => g -> StarSystems -> (StarSystems, g)
populateWormholes gen sss = (newSystems, newGen)
  where (newSystems, newGen) = Map.foldl genEmptyWormholes (sss, gen) sss

genEmptyWormholes
  :: RandomGen g => (StarSystems, g) -> StarSystem -> (StarSystems, g)
genEmptyWormholes (sss, gen) system = (newSystems, newGen)
 where
  newSystems          = Map.insert (view ssId system) newSystem sss
  (newSystem, newGen) = genEmptyWormholes' gen system

genEmptyWormholes' :: RandomGen g => g -> StarSystem -> (StarSystem, g)
genEmptyWormholes' gen system = (newSystem, newGen)
 where
  numWormholes        = 25 - (length $ Map.keys (view ssWormholes system))
  (newSystem, newGen) = genEmptyWormhole gen numWormholes system

genEmptyWormhole :: RandomGen g => g -> Int -> StarSystem -> (StarSystem, g)
genEmptyWormhole gen 0   system = (system, gen)
genEmptyWormhole gen num system = (newSystem, newGen)
 where
  (newSystem, newGen) = genEmptyWormhole gen' (num - 1) system'
  system'             = system
    { _ssWormholes = Map.insert (view wId newWormhole)
                                newWormhole
                                (view ssWormholes system)
    }
  (newWormhole, gen') = genEmptyWormhole' gen

genEmptyWormhole' :: RandomGen g => g -> (Wormhole, g)
genEmptyWormhole' gen = (newWormhole, newGen)
 where
  newWormhole    = blankWormhole { _wId = wid, _wLocation = wLoc }
  (wid , gen'  ) = randomId gen
  (wLoc, newGen) = genWormholeLocation gen'

addSystemWormhole
  :: RandomGen g => g -> StarSystem -> StarSystem -> (StarSystem, StarSystem, g)
addSystemWormhole gen ss1 ss2 = (newSystem1, newSystem2, newGen)
 where
  (wLoc1, gen'  ) = genWormholeLocation gen
  (wLoc2, gen'' ) = genWormholeLocation gen'
  (wId1 , gen''') = randomId gen''
  (wId2 , newGen) = randomId gen'''
  newWH1          = blankWormhole { _wId                  = wId1
                                  , _wLocation            = wLoc1
                                  , _wDestinationId       = Just wId2
                                  , _wDestinationStarId   = Just (view ssId ss2)
                                  , _wDestinationLocation = Just wLoc2
                                  }
  newWH2 = blankWormhole { _wId                  = wId2
                         , _wLocation            = wLoc2
                         , _wDestinationId       = Just wId1
                         , _wDestinationStarId   = Just (view ssId ss1)
                         , _wDestinationLocation = Just wLoc1
                         }
  newSystem1 = ss1
    { _ssWormholes = Map.insert (view wId newWH1) newWH1 (view ssWormholes ss1)
    }
  newSystem2 = ss2
    { _ssWormholes = Map.insert (view wId newWH2) newWH2 (view ssWormholes ss2)
    }

addRandomSystemWormhole
  :: RandomGen g => g -> StarSystemId -> StarSystems -> (StarSystems, g)
addRandomSystemWormhole gen sid sss = (newSystems, newGen)
 where
  ss1                              = fromJust $ Map.lookup sid sss
  ss2                              = fromJust $ Map.lookup ss2id sss
  (ss2id, gen')                    = genWormholeDestination gen (Map.keys sss)
  (newSystem1, newSystem2, newGen) = addSystemWormhole gen' ss1 ss2
  newSystems = Map.insert ss2id newSystem2 (Map.insert sid newSystem1 sss)

genWormholeLocation :: RandomGen g => g -> (WormholeLocation, g)
genWormholeLocation gen = (WormholeLocation x y, newGen)
 where
  (x, gen'  ) = randomR (-1e10, 1e10) gen
  (y, newGen) = randomR (-1e10, 1e10) gen'

genWormholeDestination
  :: RandomGen g => g -> [StarSystemId] -> (StarSystemId, g)
genWormholeDestination gen ssids = (head newSSIDs, newGen)
 where
  (gen', newGen) = split gen
  newSSIDs       = shuffle' ssids (length ssids) gen'
