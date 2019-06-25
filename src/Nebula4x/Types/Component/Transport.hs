{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Nebula4x.Types.Component.Transport where

import           Control.Lens
import           Data.Aeson.TH
import           GHC.Generics             (Generic)

import           Nebula4x.Types.Component

data CargoHandlingComponent = CargoHandlingComponent
  { _cHandleName   :: ComponentName
  , _cHandleCost   :: ComponentCost
  , _cHandleSize   :: ComponentSize
  , _cHandleRating :: ComponentRating
  } deriving (Show, Eq, Generic)

makeLenses ''CargoHandlingComponent

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''CargoHandlingComponent

data CargoHoldComponent = CargoHoldComponent
  { _cHoldName   :: ComponentName
  , _cHoldCost   :: ComponentCost
  , _cHoldSize   :: ComponentSize
  , _cHoldRating :: ComponentRating
  } deriving (Show, Eq, Generic)

makeLenses ''CargoHoldComponent

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''CargoHoldComponent

data JumpGateComponent = JumpGateComponent
  { _jgName   :: ComponentName
  , _jgCost   :: ComponentCost
  , _jgSize   :: ComponentSize
  , _jgRating :: ComponentRating
  } deriving (Show, Eq, Generic)

makeLenses ''JumpGateComponent

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''JumpGateComponent

data CargoHandlingResearch = CargoHandlingResearch
  { _chsrId               :: ComponentId
  , _chsrResearchCost     :: ComponentResearch
  , _chsrResearchProgress :: ComponentResearch
  , _chsrCargoHandling    :: CargoHandlingComponent
  } deriving (Show, Eq, Generic)

makeLenses ''CargoHandlingResearch

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''CargoHandlingResearch

data CargoHoldResearch = CargoHoldResearch
  { _chrId               :: ComponentId
  , _chrResearchCost     :: ComponentResearch
  , _chrResearchProgress :: ComponentResearch
  , _chrCargoHold        :: CargoHoldComponent
  } deriving (Show, Eq, Generic)

makeLenses ''CargoHoldResearch

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''CargoHoldResearch

data JumpGateResearch = JumpGateResearch
  { _jgrId               :: ComponentId
  , _jgrResearchCost     :: ComponentResearch
  , _jgrResearchProgress :: ComponentResearch
  , _jgrJumpGate         :: JumpGateComponent
  } deriving (Show, Eq, Generic)

makeLenses ''JumpGateResearch

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''JumpGateResearch
