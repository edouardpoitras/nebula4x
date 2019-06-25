{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Nebula4x.Types.Component.Shield where

import           Control.Lens
import           Data.Aeson.TH
import           GHC.Generics                   ( Generic )

import           Nebula4x.Types.Component

data Shield = Shield
  { _shName   :: ComponentName
  , _shRating :: ComponentRating
  } deriving (Show, Eq, Generic)

makeLenses ''Shield

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Shield

data ShieldResearch = ShieldResearch
  { _shrId               :: ComponentId
  , _shrResearchCost     :: ComponentResearch
  , _shrResearchProgress :: ComponentResearch
  , _shrShield           :: Shield
  } deriving (Show, Eq, Generic)

makeLenses ''ShieldResearch

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''ShieldResearch

data ShipShields = ShipShields
  { _ssEnabled          :: Bool
  , _ssCurrentCapacity  :: ComponentRating
  , _ssMaxCapacity      :: ComponentRating
  , _ssRegenerationRate :: ComponentRating
  , _ssFuelConsumptionRate :: ComponentRating
  } deriving (Show, Eq, Generic)

makeLenses ''ShipShields

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''ShipShields
