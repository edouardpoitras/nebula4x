{-# LANGUAGE DeriveGeneric #-}

module Nebula4x.Types.Component where

import           Control.Newtype.Generics (Newtype)
import           Data.Aeson               (FromJSON, ToJSON)
import           GHC.Generics             (Generic)

type ComponentId = Int

newtype ComponentName =
  ComponentName String
  deriving (Show, Eq, Generic)

newtype ComponentCost =
  ComponentCost Double
  deriving (Show, Eq, Generic)

newtype ComponentSize =
  ComponentSize Double
  deriving (Show, Eq, Generic)

newtype ComponentRating =
  ComponentRating Double
  deriving (Show, Eq, Generic)

newtype ComponentEfficiency =
  ComponentEfficiency Double
  deriving (Show, Eq, Generic)

newtype ComponentConsumption =
  ComponentConsumption Double
  deriving (Show, Eq, Generic)

newtype ComponentModifier =
  ComponentModifier Double
  deriving (Show, Eq, Generic)

newtype ComponentResearch =
  ComponentResearch Double
  deriving (Show, Eq, Generic)

newtype TaskProgress =
  TaskProgress Double
  deriving (Show, Eq, Generic)

newtype TaskRate =
  TaskRate Double
  deriving (Show, Eq, Generic)

newtype GenerationChance =
  GenerationChance Double
  deriving (Show, Eq, Generic)

newtype MassRatio =
  MassRatio Double
  deriving (Show, Eq, Generic)

instance ToJSON ComponentName

instance FromJSON ComponentName

instance Newtype ComponentName

instance ToJSON ComponentCost

instance FromJSON ComponentCost

instance Newtype ComponentCost

instance ToJSON ComponentSize

instance FromJSON ComponentSize

instance Newtype ComponentSize

instance ToJSON ComponentRating

instance FromJSON ComponentRating

instance Newtype ComponentRating

instance ToJSON ComponentEfficiency

instance FromJSON ComponentEfficiency

instance Newtype ComponentEfficiency

instance ToJSON ComponentConsumption

instance FromJSON ComponentConsumption

instance Newtype ComponentConsumption

instance ToJSON ComponentModifier

instance FromJSON ComponentModifier

instance Newtype ComponentModifier

instance ToJSON ComponentResearch

instance FromJSON ComponentResearch

instance Newtype ComponentResearch

instance ToJSON TaskProgress

instance FromJSON TaskProgress

instance Newtype TaskProgress

instance ToJSON TaskRate

instance FromJSON TaskRate

instance Newtype TaskRate

instance ToJSON GenerationChance

instance FromJSON GenerationChance

instance Newtype GenerationChance

instance ToJSON MassRatio

instance FromJSON MassRatio

instance Newtype MassRatio
