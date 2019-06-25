{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Nebula4x.Types.Ship where

import           Control.Lens                  as L
import           Control.Newtype.Generics
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Aeson.TH
import qualified Data.Map.Strict               as Map
import           GHC.Generics                   ( Generic )

import           Nebula4x.Types.Component
import           Nebula4x.Types.Component.Armor
import           Nebula4x.Types.Component.Engine
import           Nebula4x.Types.Component.FuelStorage
import           Nebula4x.Types.Component.Laser
import           Nebula4x.Types.Component.MissleLauncher
import           Nebula4x.Types.Component.Sensor
import           Nebula4x.Types.Component.Shield
import           Nebula4x.Types.Component.Transport
import           Nebula4x.Types.Installment
import           Nebula4x.Types.Mineral
import           Nebula4x.Types.Race

--
-- The Ship Entity
--
type ShipId = ComponentId

type ShipDesignId = ComponentId

newtype ShipName =
  ShipName String
  deriving (Show, Eq, Generic)

newtype ShipFuel =
  ShipFuel Double
  deriving (Show, Eq, Generic)

newtype ShipSize =
  ShipSize Double
  deriving (Show, Eq, Generic)

newtype ShipSpeed =
  ShipSpeed Double
  deriving (Show, Eq, Generic)

newtype ShipRange =
  ShipRange Double
  deriving (Show, Eq, Generic)

data ShipClass
  = CommercialClass
  | MilitaryClass
  deriving (Show, Eq, Generic)

data ShipLocation = ShipLocation
  { _sLocationX :: Double
  , _sLocationY :: Double
  } deriving (Show, Eq, Generic)

instance ToJSON ShipName

instance FromJSON ShipName

instance Newtype ShipName

instance ToJSON ShipClass

instance FromJSON ShipClass

instance ToJSON ShipFuel

instance FromJSON ShipFuel

instance Newtype ShipFuel

instance ToJSON ShipSize

instance FromJSON ShipSize

instance Newtype ShipSize

instance ToJSON ShipSpeed

instance FromJSON ShipSpeed

instance Newtype ShipSpeed

instance ToJSON ShipRange

instance FromJSON ShipRange

instance Newtype ShipRange

makeLenses ''ShipLocation

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''ShipLocation

--
-- Ship Orders
--
-- Using ComponentId instead of BodyId because importing Nebula4x.Types.Body gives a cycle import issue.
--
data ShipOrder
  = MoveOrder { _moX :: Double
              , _moY :: Double }
  | MoveToBodyOrder { _mtboBodyId :: ComponentId }
  | OrbitOrder { _ooBodyId  :: ComponentId
               , _ooArrived :: Bool }
  | FollowOrder { _foShipId :: ShipId }
  | AttackShipOrder { _asoShipId :: ShipId }
  | AttackNearestShipOrder
  | ContinuousAttackNearestShipOrder
  | AttackBodyOrder { _aboBodyId :: ComponentId }
  | PickupMineralsOrder { _pmoBodyId     :: ComponentId
                        , _pmoMinerals   :: MineralStack
                        , _pmoInProgress :: Bool }
  | DropoffMineralsOrder { _dmoBodyId     :: ComponentId
                         , _dmoMinerals   :: MineralStack
                         , _dmoInProgress :: Bool }
  | PickupInstallmentOrder { _pioBodyId      :: ComponentId
                           , _pioInstallment :: Installment
                           , _pioProgress    :: TaskProgress
                           , _pioInProgress  :: Bool }
  | DropoffInstallmentOrder { _dioBodyId      :: ComponentId
                            , _dioInstallment :: Installment
                            , _dioProgress    :: TaskProgress
                            , _dioInProgress  :: Bool }
  | RefuelAtBodyOrder { _raboBodyId :: ComponentId }
  | RefuelAtShipOrder { _rasoShipId :: ComponentId }
  | TransferFuelToBody { _tftbBodyId     :: ComponentId
                       , _tftbFuelAmount :: ShipFuel }
  | TransferFuelToShip { _tftsShipId     :: ComponentId
                       , _tftsFuelAmount :: ShipFuel }
  | GeologicalSurveyOrder { _gsoBodyId     :: ComponentId
                          , _gsoInProgress :: Bool }
  | ContinuousGeologicalSurveyOrder { _cgsoCurrentBodyId :: Maybe ComponentId
                                    , _cgsoInProgress    :: Bool
                                    , _cgsoPlanets       :: Bool
                                    , _cgsoMoons         :: Bool
                                    , _cgsoComets        :: Bool
                                    , _cgsoAsteroids     :: Bool }
  | GravitationalSurveyOrder { _grsoWormholeId :: ComponentId
                             , _grsoInProgress :: Bool }
  | ContinuousGravitationalSurveyOrder { _cgrsoCurrentWormholeId :: Maybe ComponentId
                                       , _cgrsoInProgress        :: Bool }
  | BuildJumpGateOrder { _bjgoWormholeId :: ComponentId
                       , _bjgoInProgress :: Bool }
  | MoveToJumpGateOrder { _mtjgoWormholeId:: ComponentId }
  | TransitJumpGateOrder { _tjgoWormholeId :: ComponentId
                         , _tjgoSystemId :: ComponentId
                         , _tjgoCooldown :: TaskProgress }
  deriving (Show, Eq, Generic)

makeLenses ''ShipOrder

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''ShipOrder

data ShipOrderGroup = ShipOrderGroup
  { _sogOrders       :: [ShipOrder]
  , _sogRepeat       :: Int
  , _sogCurrentOrder :: Int
  } deriving (Show, Eq, Generic)

makeLenses ''ShipOrderGroup

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''ShipOrderGroup

data OrderCondition
  = EnemyInSightCondition
  | FuelCondition { _fcMinimumFuel :: ComponentRating }
  | ShieldCondition { _scMinimumShield :: ComponentRating }
  deriving (Show, Eq, Generic)

makeLenses ''OrderCondition

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''OrderCondition

data ShipConditionalOrder = ShipConditionalOrder
  { _scoCondition :: OrderCondition
  , _scoOrder :: ShipOrder
  } deriving (Show, Eq, Generic)

makeLenses ''ShipConditionalOrder

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''ShipConditionalOrder

type ShipConditionalOrders = [ShipConditionalOrder]

--
-- Ship Design
--
data ShipDesign = ShipDesign
  { _sdId                   :: ShipDesignId
  , _sdName                 :: ShipName
  , _sdArmor                :: [Armor]
  , _sdShields              :: [Shield]
  , _sdSize                 :: ShipSize
  , _sdSpeed                :: ShipSpeed
  , _sdRange                :: ShipRange
  , _sdClass                :: ShipClass
  , _sdEngines              :: [Engine]
  , _sdMissleLaunchers      :: [MissleLauncher]
  , _sdLasers               :: [Laser]
  , _sdFuelStorages         :: [FuelStorage]
  , _sdCargoHandlingSystems :: [CargoHandlingComponent]
  , _sdCargoHolds           :: [CargoHoldComponent]
  , _sdJumpGates            :: [JumpGateComponent]
  , _sdGeologicalSensors    :: [Sensor]
  , _sdGravitationalSensors :: [Sensor]
  , _sdBuildCost            :: MineralCost
  } deriving (Show, Eq, Generic)

makeLenses ''ShipDesign

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''ShipDesign

--
-- Ship Cargo
--
data ShipCargo = ShipCargo
  { _scMinerals     :: MineralStacks
  , _scInstallments :: Installments
  } deriving (Show, Eq, Generic)

makeLenses ''ShipCargo

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''ShipCargo

--
-- Ship
--
data Ship = Ship
  { _sId           :: ShipId
  , _sRace         :: RaceId
  , _sName         :: ShipName
  , _sLocation     :: ShipLocation
  , _sFuel         :: ShipFuel
  , _sShipMissleLaunchers :: Maybe ShipMissleLaunchers
  , _sShipLasers   :: Maybe ShipLasers
  , _sArmor        :: ShipArmor
  , _sShields      :: Maybe ShipShields
  , _sCurrentOrder :: Maybe ShipOrder
  , _sOrderGroups  :: [ShipOrderGroup]
  , _sCurrentConditionalOrder :: Maybe ShipConditionalOrder
  , _sConditionalOrders :: ShipConditionalOrders
  , _sCargo        :: ShipCargo
  , _sDesign       :: ShipDesign
  } deriving (Show, Eq, Generic)

makeLenses ''Ship

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Ship

type Ships = Map.Map ShipId Ship

data ShipTransit = ShipTransit
  { _tsDestinationId :: ComponentId
  , _tsWormholeId    :: ComponentId
  , _tsShip          :: Ship
  } deriving (Show, Eq, Generic)

makeLenses ''ShipTransit

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''ShipTransit

type ShipTransits = Map.Map ShipId ShipTransit

emptyCargo :: ShipCargo
emptyCargo = ShipCargo Map.empty Map.empty

noShips :: Ships
noShips = Map.empty
