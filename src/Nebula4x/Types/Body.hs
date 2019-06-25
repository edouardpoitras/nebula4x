{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Nebula4x.Types.Body where

import           Control.Lens
import           Control.Newtype.Generics       ( Newtype )
import           Data.Aeson
import qualified Data.Aeson.Encoding           as DAE
import           Data.Aeson.TH
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as Text
import           GHC.Generics                   ( Generic )

import           Nebula4x.Types.Component
import           Nebula4x.Types.Installment
import           Nebula4x.Types.Mineral
import           Nebula4x.Types.Race
import           Nebula4x.Types.Shipyard

type BodyId = ComponentId

newtype BodyName =
  BodyName String
  deriving (Show, Eq, Generic)

newtype BodyMass =
  BodyMass Double
  deriving (Show, Eq, Generic)

data BodyLocation = BodyLocation
  { _bLocationX :: Double
  , _bLocationY :: Double
  } deriving (Show, Eq, Generic)

newtype BodyRadius =
  BodyRadius Double
  deriving (Show, Eq, Generic)

data Range a = Range
  { _rangeMin :: a
  , _rangeMax :: a
  } deriving (Show, Eq, Generic)

type OrbitalDistance = Range Double

newtype FuelReserves =
  FuelReserves Double
  deriving (Show, Eq, Generic)

newtype ActiveRefining =
  ActiveRefining Bool
  deriving (Show, Eq, Generic)

data BodyType
  = Asteroid
  | Comet
  | DwarfPlanet
  | GasDwarf
  | GasGiant
  | IceGiant
  | Moon
  | Terrestrial
  deriving (Show, Read, Eq, Ord, Generic)

data BodySurvey = BodySurvey
  { _bsSurveyed :: Bool
  , _bsProgress :: Double
  } deriving (Show, Eq, Generic)

type BodySurveys = Map.Map RaceId BodySurvey

newtype OrbitDeviation =
  OrbitDeviation Double
  deriving (Show, Eq, Generic)

instance ToJSON OrbitDeviation

instance FromJSON OrbitDeviation

instance Newtype OrbitDeviation

makeLenses ''BodyLocation

makeLenses ''Range

makeLenses ''BodyType

makeLenses ''BodySurvey

instance ToJSON BodyName

instance FromJSON BodyName

instance ToJSON BodyMass

instance FromJSON BodyMass

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''BodyLocation

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Range

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''BodyType

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''BodySurvey

instance ToJSON BodyRadius

instance FromJSON BodyRadius

instance ToJSON FuelReserves

instance FromJSON FuelReserves

instance ToJSON ActiveRefining

instance FromJSON ActiveRefining

instance FromJSONKey BodyType where
  fromJSONKey = FromJSONKeyText (read . Text.unpack)

instance ToJSONKey BodyType where
  toJSONKey = ToJSONKeyText f g
   where
    f = Text.pack . show
    g = DAE.text . Text.pack . show

type Bodies = Map.Map BodyId Body

data Body = Body
  { _bId              :: BodyId
  , _bRace            :: Maybe RaceId
  , _bName            :: BodyName
  , _bMass            :: BodyMass
  , _bLocation        :: BodyLocation
  , _bRadius          :: BodyRadius
  , _bOrbitalDistance :: OrbitalDistance
  , _bType            :: BodyType
  , _bShipyards       :: Shipyards
  , _bFuelReserves    :: FuelReserves
  , _bRefiningFuel    :: ActiveRefining
  , _bInstallments    :: Installments
  , _bMinerals        :: Minerals
  , _bMassDriverBody  :: Maybe BodyId
  , _bSurveys         :: BodySurveys
  , _bParentBody      :: Maybe BodyId
  } deriving (Show, Eq, Generic)

makeLenses ''Body

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Body

data PacketLocation = PacketLocation
  { _plLocationX :: Double
  , _plLocationY :: Double
  } deriving (Show, Eq, Generic)

makeLenses ''PacketLocation

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''PacketLocation

newtype PacketSpeed =
  PacketSpeed Double
  deriving (Show, Eq, Generic)

instance ToJSON PacketSpeed

instance FromJSON PacketSpeed

instance Newtype PacketSpeed

-- Putting this here for now - having trouble with import cycles.
data MineralPacket = MineralPacket
  { _mpMineralStacks   :: MineralStacks
  , _mpDestinationBody :: BodyId
  , _mpSpeed           :: PacketSpeed
  , _mpLocation        :: PacketLocation
  } deriving (Show, Eq, Generic)

makeLenses ''MineralPacket

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''MineralPacket

type MineralPackets = [MineralPacket]

notRefining :: ActiveRefining
notRefining = ActiveRefining False

noMoons :: Bodies
noMoons = Map.empty

noComets :: Bodies
noComets = Map.empty

noAsteroids :: Bodies
noAsteroids = Map.empty

noPackets :: MineralPackets
noPackets = []

noSurvey :: BodySurvey
noSurvey = BodySurvey { _bsSurveyed = False, _bsProgress = 0 }

noSurveys :: BodySurveys
noSurveys = Map.empty
