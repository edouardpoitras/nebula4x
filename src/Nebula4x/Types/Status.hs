{-# LANGUAGE TemplateHaskell #-}

module Nebula4x.Types.Status where

import           Control.Lens
import           Data.Aeson.TH

type LastPulse = Int

type NextPulse = Int

type Pulses = (LastPulse, NextPulse)

data PulseStatus = PulseStatus
  { _psBodyPosition      :: Pulses
  , _psShipyardWork      :: Pulses
  , _psResearchWork      :: Pulses
  , _psProductionWork    :: Pulses
  , _psMiningWork        :: Pulses
  , _psMassDriverWork    :: Pulses
  , _psMineralPacketWork :: Pulses
  , _psMissleWork        :: Pulses
  , _psShipOrder         :: Pulses
  } deriving (Show, Eq)

makeLenses ''PulseStatus

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''PulseStatus

data Status = Status
  { _pulseStatus :: PulseStatus
  } deriving (Show, Eq)

makeLenses ''Status

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Status
