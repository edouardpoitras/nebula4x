{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Nebula4x.Types.Wormhole where

import           Control.Lens
import           Data.Aeson.TH
import qualified Data.Map.Strict          as Map
import           GHC.Generics             (Generic)

import           Nebula4x.Types.Component

data WormholeLocation = WormholeLocation
  { _wLocationX :: Double
  , _wLocationY :: Double
  } deriving (Show, Eq, Generic)

makeLenses ''WormholeLocation

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''WormholeLocation

type WormholeId = Int

type Wormholes = Map.Map WormholeId Wormhole

data Wormhole = Wormhole
  { _wId                  :: WormholeId
  , _wLocation            :: WormholeLocation
  , _wDestinationId       :: Maybe ComponentId
  , _wDestinationStarId   :: Maybe ComponentId
  , _wDestinationLocation :: Maybe WormholeLocation
  , _wSurveyed            :: Bool
  , _wSurveyProgress      :: TaskProgress
  , _wJumpGate            :: Bool
  , _wJumpGateProgress    :: TaskProgress
  } deriving (Show, Eq, Generic)

makeLenses ''Wormhole

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Wormhole

noWormholes :: Wormholes
noWormholes = Map.empty

blankWormhole :: Wormhole
blankWormhole =
  Wormhole
    { _wId = 0
    , _wLocation = WormholeLocation 0 0
    , _wDestinationId = Nothing
    , _wDestinationStarId = Nothing
    , _wDestinationLocation = Nothing
    , _wSurveyed = False
    , _wSurveyProgress = TaskProgress 0
    , _wJumpGate = False
    , _wJumpGateProgress = TaskProgress 0
    }
