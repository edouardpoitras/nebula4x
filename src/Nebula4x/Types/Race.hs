{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Nebula4x.Types.Race where

import           Control.Lens                  as L
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Aeson.TH
import qualified Data.Map.Strict               as Map
import           GHC.Generics                   ( Generic )

import           Nebula4x.Types.Component

type RaceId = ComponentId

newtype RaceName =
  RaceName String
  deriving (Show, Eq, Generic)

data RaceController = Human | AI deriving (Show, Eq, Generic)

instance ToJSON RaceName

instance FromJSON RaceName

instance ToJSON RaceController

instance FromJSON RaceController

--
-- Race
--
data Race = Race
  { _rId          :: RaceId
  , _rName        :: RaceName
  , _rController  :: RaceController
  } deriving (Show, Eq, Generic)

makeLenses ''Race

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Race

type Races = Map.Map RaceId Race

noRaces :: Races
noRaces = Map.empty

noRace :: Maybe RaceId
noRace = Nothing
