{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Nebula4x.Types.Component.Sensor where

import           Control.Lens
import           Data.Aeson.TH
import           GHC.Generics             (Generic)

import           Nebula4x.Types.Component

data Sensor
  = GeologicalSensor { _senName   :: ComponentName
                     , _senCost   :: ComponentCost
                     , _senSize   :: ComponentSize
                     , _senRating :: ComponentRating }
  | GravitationalSensor { _senName   :: ComponentName
                        , _senCost   :: ComponentCost
                        , _senSize   :: ComponentSize
                        , _senRating :: ComponentRating }
  deriving (Show, Eq, Generic)

makeLenses ''Sensor

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Sensor

data SensorResearch = SensorResearch
  { _srId               :: ComponentId
  , _srResearchCost     :: ComponentResearch
  , _srResearchProgress :: ComponentResearch
  , _srSensor           :: Sensor
  } deriving (Show, Eq, Generic)

makeLenses ''SensorResearch

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''SensorResearch
