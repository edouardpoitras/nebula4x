{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Nebula4x.Types.StarSystem where

import           Control.Lens
import           Data.Aeson.TH
import qualified Data.Map.Strict               as Map
import           GHC.Generics                   ( Generic )

import           Nebula4x.Types.Body
import           Nebula4x.Types.Body.Star
import           Nebula4x.Types.Component
import           Nebula4x.Types.Component.MissleLauncher
import           Nebula4x.Types.Race
import           Nebula4x.Types.Ship
--import           Nebula4x.Types.Shipyard
import           Nebula4x.Types.Wormhole

type Discovered = Bool

type DiscoveryStatus = Map.Map RaceId Discovered

type StarSystems = Map.Map StarSystemId StarSystem

type StarSystemId = ComponentId

data StarSystem = StarSystem
  { _ssId             :: StarSystemId
  , _ssStar           :: Star
  , _ssBodies         :: Bodies
  , _ssMineralPackets :: MineralPackets
  --, _ssShipyards      :: Shipyards
  , _ssShips          :: Ships
  , _ssMissleSalvos   :: [MissleSalvo]
  , _ssWormholes      :: Wormholes
  , _ssDiscovered     :: DiscoveryStatus
  } deriving (Show, Eq, Generic)

makeLenses ''StarSystem

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''StarSystem

notDiscovered :: DiscoveryStatus
notDiscovered = Map.empty
