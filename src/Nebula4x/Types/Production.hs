{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Nebula4x.Types.Production where

import           Control.Lens
import           Control.Newtype.Generics
import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.Aeson.TH
import qualified Data.Map.Strict            as Map
import           GHC.Generics               (Generic)

import           Nebula4x.Types.Body
import           Nebula4x.Types.Installment

newtype ProductionProgress =
  ProductionProgress Double
  deriving (Show, Eq, Generic)

instance ToJSON ProductionProgress

instance FromJSON ProductionProgress

instance Newtype ProductionProgress

newtype ProductionAllocation =
  ProductionAllocation Double
  deriving (Show, Eq, Generic)

instance ToJSON ProductionAllocation

instance FromJSON ProductionAllocation

instance Newtype ProductionAllocation

data InstallmentProduction = InstallmentProduction
  { _ipProgress    :: ProductionProgress
  , _ipAllocation  :: ProductionAllocation
  , _ipInstallment :: Installment
  } deriving (Show, Eq, Generic)

makeLenses ''InstallmentProduction

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''InstallmentProduction

type BodyProduction = Map.Map InstallmentId InstallmentProduction

type Production = Map.Map BodyId BodyProduction
