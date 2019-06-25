{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Nebula4x.Types.Component.FuelStorage where

import           Control.Lens
import           Data.Aeson.TH
import           GHC.Generics             (Generic)

import           Nebula4x.Types.Component

data FuelStorage = FuelStorage
  { _fsName   :: ComponentName
  , _fsCost   :: ComponentCost
  , _fsSize   :: ComponentSize
  , _fsRating :: ComponentRating
  } deriving (Show, Eq, Generic)

makeLenses ''FuelStorage

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''FuelStorage

data FuelStorageResearch = FuelStorageResearch
  { _fsrId               :: ComponentId
  , _fsrResearchCost     :: ComponentResearch
  , _fsrResearchProgress :: ComponentResearch
  , _fsrFuelStorage      :: FuelStorage
  } deriving (Show, Eq, Generic)

makeLenses ''FuelStorageResearch

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''FuelStorageResearch
