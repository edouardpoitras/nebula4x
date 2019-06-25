{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Nebula4x.Types.Shipyard where

import           Control.Lens
import           Control.Newtype.Generics
import           Data.Aeson               (FromJSON, ToJSON)
import           Data.Aeson.TH
import qualified Data.Map.Strict          as Map hiding (drop)
import           GHC.Generics             (Generic)

import           Nebula4x.Types.Component
import           Nebula4x.Types.Mineral
import           Nebula4x.Types.Ship

type ShipyardId = ComponentId

newtype ShipyardName =
  ShipyardName String
  deriving (Show, Eq, Generic)

data ShipyardSlipway = ShipyardSlipway
  { _sysActive   :: Bool
  , _sysProgress :: TaskProgress
  , _sysShipName :: Maybe ShipName
  } deriving (Show, Eq, Generic)

newtype ShipyardCapacity =
  ShipyardCapacity Double
  deriving (Show, Eq, Generic)

type CapacityAmount = Int

type ShipyardQueueItem = ShipName

data ShipyardType
  = CommercialShipyard
  | NavyShipyard
  deriving (Show, Eq, Generic)

data ShipyardTask
  = ShipyardRetool { _srShipDesign   :: ShipDesign
                   , _srRetoolCost   :: MineralCost -- Total retooling cost
                   , _srRetoolRate   :: MineralCost -- Mineral cost per second
                   , _srProgress     :: TaskProgress -- Total progress so far
                   , _srProgressRate :: TaskProgress -- Progress per second
                    }
  | ShipyardAddCapacity { _sacTargetCapacity   :: ShipyardCapacity -- Target capacity
                        , _sacStartingCapacity :: ShipyardCapacity -- Starting Capacity
                        , _sacCostRate         :: MineralCost -- Mineral cost per second
                        , _sacCapacityRate     :: TaskRate -- Added capacity per second
                         }
  | ShipyardAddSlipway { _sasCostRate     :: MineralCost -- Mineral cost per second
                       , _sasProgressRate :: TaskProgress -- Progress per second
                       , _sasProgress     :: TaskProgress -- Total progress so far
                        }
  deriving (Show, Eq, Generic)

newtype ShipyardBuildRate =
  ShipyardBuildRate Double
  deriving (Show, Eq, Generic)

instance ToJSON ShipyardName

instance FromJSON ShipyardName

instance Newtype ShipyardName

instance ToJSON ShipyardCapacity

instance FromJSON ShipyardCapacity

instance Newtype ShipyardCapacity

instance ToJSON ShipyardType

instance FromJSON ShipyardType

instance ToJSON ShipyardBuildRate

instance FromJSON ShipyardBuildRate

instance Newtype ShipyardBuildRate

makeLenses ''ShipyardSlipway

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''ShipyardSlipway

makeLenses ''ShipyardTask

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''ShipyardTask

data Shipyard = Shipyard
  { _syId          :: ShipyardId
  , _syName        :: ShipyardName
  , _syShipDesign  :: Maybe ShipDesign
  , _sySlipways    :: [ShipyardSlipway]
  , _syCapacity    :: ShipyardCapacity
  , _syType        :: ShipyardType
  , _syBuildRate   :: ShipyardBuildRate -- Yearly build rate
  , _syCurrentTask :: Maybe ShipyardTask
  , _syBuildQueue  :: [ShipyardQueueItem]
  } deriving (Show, Eq, Generic)

makeLenses ''Shipyard

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Shipyard

type Shipyards = Map.Map ShipyardId Shipyard
