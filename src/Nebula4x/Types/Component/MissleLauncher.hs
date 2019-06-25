{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Nebula4x.Types.Component.MissleLauncher where

import           Control.Lens
import           Control.Newtype.Generics
import           Data.Aeson
import           Data.Aeson.TH
import           GHC.Generics                   ( Generic )

import           Nebula4x.Types.Component

--
-- Missle
--
data MissleSalvoLocation = MissleSalvoLocation
  { _mLocationX :: Double
  , _mLocationY :: Double
  } deriving (Show, Eq, Generic)

makeLenses ''MissleSalvoLocation

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''MissleSalvoLocation

data MissleTargetType = ShipTarget | BodyTarget deriving (Show, Eq, Generic)

instance ToJSON MissleTargetType

instance FromJSON MissleTargetType

data MissleSalvoTarget = MissleSalvoTarget
  { _mtType   :: MissleTargetType
  , _mtTarget :: ComponentId
  } deriving (Show, Eq, Generic)

makeLenses ''MissleSalvoTarget

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''MissleSalvoTarget

noTarget :: MissleSalvoTarget
noTarget = MissleSalvoTarget ShipTarget 0

newtype MissleSpeed = MissleSpeed Double deriving (Show, Eq, Generic)

instance Newtype MissleSpeed

instance ToJSON MissleSpeed

instance FromJSON MissleSpeed

newtype MissleRange = MissleRange Double deriving (Show, Eq, Generic)

instance Newtype MissleRange

instance ToJSON MissleRange

instance FromJSON MissleRange

newtype MissleCount = MissleCount Int deriving (Show, Eq, Generic)

instance Newtype MissleCount

instance ToJSON MissleCount

instance FromJSON MissleCount

newtype MissleStrength = MissleStrength Double deriving (Show, Eq, Generic)

instance Newtype MissleStrength

instance ToJSON MissleStrength

instance FromJSON MissleStrength

data Missle = Missle
  { _missSpeed :: MissleSpeed
  , _missRange :: MissleRange
  , _missStrength :: MissleStrength
  } deriving (Show, Eq, Generic)

makeLenses ''Missle

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Missle

type MissleSalvoSpeed = MissleSpeed

type MissleSalvoRange = MissleRange

type MissleSalvoMissleStrength = MissleStrength

data MissleSalvo = MissleSalvo
  { _msMissleStrengths :: [MissleSalvoMissleStrength]
  , _msRange :: MissleSalvoRange
  , _msLocation :: MissleSalvoLocation
  , _msSpeed :: MissleSalvoSpeed
  , _msTarget :: MissleSalvoTarget
  } deriving (Show, Eq, Generic)

makeLenses ''MissleSalvo

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''MissleSalvo

type MissleSalvos = [MissleSalvo]

--
-- Missle Launcher
--
data MissleLauncher = MissleLauncher
  { _mlName :: ComponentName
  , _mlCost :: ComponentCost
  , _mlSize :: ComponentSize
  , _mlReloadRate :: ComponentRating
  , _mlMissle :: Missle
  } deriving (Show, Eq, Generic)

makeLenses ''MissleLauncher

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''MissleLauncher

data ShipMissleLaunchers = ShipMissleLaunchers
  { _smlReloadRate :: ComponentRating
  , _smlReloadStatus :: ComponentRating
  , _smlSalvo :: MissleSalvo
  } deriving (Show, Eq, Generic)

makeLenses ''ShipMissleLaunchers

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''ShipMissleLaunchers

data MissleLauncherDesign = MissleLauncherDesign
  { _mldId :: ComponentId
  , _mldResearchCost :: ComponentResearch
  , _mldResearchProgress :: ComponentResearch
  , _mldMissleLauncher :: MissleLauncher
  } deriving (Show, Eq, Generic)

makeLenses ''MissleLauncherDesign

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''MissleLauncherDesign

--
-- Missle Launcher Size
--
data MissleLauncherSize = MissleLauncherSize
  { _mlsName :: ComponentName
  , _mlsSize :: ComponentRating
  } deriving (Show, Eq, Generic)

makeLenses ''MissleLauncherSize

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''MissleLauncherSize

data MissleLauncherSizeResearch = MissleLauncherSizeResearch
  { _mlsrId :: ComponentId
  , _mlsrResearchCost :: ComponentResearch
  , _mlsrResearchProgress :: ComponentResearch
  , _mlsrMissleLauncherSize :: MissleLauncherSize
  } deriving (Show, Eq, Generic)

makeLenses ''MissleLauncherSizeResearch

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''MissleLauncherSizeResearch

--
-- Missle Launcher Reload Rate
--
data MissleLauncherReloadRate = MissleLauncherReloadRate
  { _mlrrName :: ComponentName
  , _mlrrReloadRate :: ComponentRating
  } deriving (Show, Eq, Generic)

makeLenses ''MissleLauncherReloadRate

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''MissleLauncherReloadRate

data MissleLauncherReloadRateResearch = MissleLauncherReloadRateResearch
  { _mlrrrId :: ComponentId
  , _mlrrrResearchCost :: ComponentResearch
  , _mlrrrResearchProgress :: ComponentResearch
  , _mlrrrMissleLauncherReloadRate :: MissleLauncherReloadRate
  } deriving (Show, Eq, Generic)

makeLenses ''MissleLauncherReloadRateResearch

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''MissleLauncherReloadRateResearch

--
-- Missle Launcher Reduced Size
--
data MissleLauncherReducedSize = MissleLauncherReducedSize
  { _mlrsName :: ComponentName
  , _mlrsReducedSize :: ComponentRating
  , _mlrsReloadMultiplier :: ComponentRating
  } deriving (Show, Eq, Generic)

makeLenses ''MissleLauncherReducedSize

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''MissleLauncherReducedSize

data MissleLauncherReducedSizeResearch = MissleLauncherReducedSizeResearch
  { _mlrsrId :: ComponentId
  , _mlrsrResearchCost :: ComponentResearch
  , _mlrsrResearchProgress :: ComponentResearch
  , _mlrsrMissleLauncherReducedSize :: MissleLauncherReducedSize
  } deriving (Show, Eq, Generic)

makeLenses ''MissleLauncherReducedSizeResearch

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''MissleLauncherReducedSizeResearch

data ArmorDamageTile = ArmorDamage | NoArmorDamage deriving (Show, Eq, Generic)

makeLenses ''ArmorDamageTile

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''ArmorDamageTile

type ArmorDamageRow = [ArmorDamageTile]
type ArmorDamageGrid = [ArmorDamageRow]

noMissleSalvos :: MissleSalvos
noMissleSalvos = []
