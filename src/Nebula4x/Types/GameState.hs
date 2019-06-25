{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Nebula4x.Types.GameState where

import           Control.Lens
import           Data.Aeson.TH
import           Data.Time.Clock                ( UTCTime )
import           GHC.Generics                   ( Generic )

import           Nebula4x.Types.Config
import           Nebula4x.Types.Production
import           Nebula4x.Types.Race
import           Nebula4x.Types.Research
import           Nebula4x.Types.StarSystem
import           Nebula4x.Types.Status

data GameState = GameState
  { _systems      :: StarSystems
  , _races        :: Races
  , _research     :: RaceResearch
  , _production   :: Production
  , _universeTime :: UTCTime
  , _config       :: Config
  , _status       :: Status
  , _randomSeed   :: String
  } deriving (Show, Eq, Generic)

makeLenses ''GameState

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''GameState
