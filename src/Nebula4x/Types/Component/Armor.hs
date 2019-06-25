{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Nebula4x.Types.Component.Armor where

import           Control.Lens
import           Data.Aeson.TH
import           GHC.Generics                   ( Generic )

import           Nebula4x.Types.Component

data Armor = Armor
  { _aName :: ComponentName
  , _aRating :: ComponentRating -- Strength per HS
  } deriving (Show, Eq, Generic)

makeLenses ''Armor

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Armor

data ArmorResearch = ArmorResearch
  { _arId               :: ComponentId
  , _arResearchCost     :: ComponentResearch
  , _arResearchProgress :: ComponentResearch
  , _arArmor            :: Armor
  } deriving (Show, Eq, Generic)

makeLenses ''ArmorResearch

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''ArmorResearch

data ArmorTile = ArmorIntact | ArmorDamaged deriving (Show, Eq, Generic)

makeLenses ''ArmorTile

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''ArmorTile

type ArmorRow = [ArmorTile]

type ArmorGrid = [ArmorRow]

data ShipArmor = ShipArmor
  { _saArmor :: Armor
  , _saGrid :: ArmorGrid
  } deriving (Show, Eq, Generic)

makeLenses ''ShipArmor

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''ShipArmor

data DamageType = MissleDamage | LaserDamage deriving (Show, Eq, Generic)

makeLenses ''DamageType

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''DamageType
