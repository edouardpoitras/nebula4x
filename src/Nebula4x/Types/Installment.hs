{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Nebula4x.Types.Installment where

import           Control.Lens
import           Control.Newtype.Generics
import           Data.Aeson               (FromJSON, ToJSON)
import           Data.Aeson.TH            (defaultOptions, deriveJSON,
                                           fieldLabelModifier)
import qualified Data.Map.Strict          as Map
import           GHC.Generics             (Generic)

import           Nebula4x.Types.Component

type InstallmentId = ComponentId

newtype InstallmentName =
  InstallmentName String
  deriving (Show, Eq, Generic)

newtype InstallmentCount =
  InstallmentCount Double
  deriving (Show, Eq, Generic)

newtype InstallmentCost =
  InstallmentCost Double
  deriving (Show, Eq, Generic)

newtype InstallmentSize =
  InstallmentSize Double
  deriving (Show, Eq, Generic)

newtype InstallmentRating =
  InstallmentRating Double
  deriving (Show, Eq, Generic)

instance ToJSON InstallmentName

instance FromJSON InstallmentName

instance Newtype InstallmentName

instance ToJSON InstallmentCount

instance FromJSON InstallmentCount

instance Newtype InstallmentCount

instance ToJSON InstallmentCost

instance FromJSON InstallmentCost

instance Newtype InstallmentCost

instance ToJSON InstallmentSize

instance FromJSON InstallmentSize

instance Newtype InstallmentSize

instance ToJSON InstallmentRating

instance FromJSON InstallmentRating

instance Newtype InstallmentRating

data Installment
  = Mine { _iId     :: InstallmentId
         , _iName   :: InstallmentName
         , _iSize   :: InstallmentSize
         , _iCost   :: InstallmentCost
         , _iRating :: InstallmentRating }
  | ResearchLab { _iId     :: InstallmentId
                , _iName   :: InstallmentName
                , _iSize   :: InstallmentSize
                , _iCost   :: InstallmentCost
                , _iRating :: InstallmentRating }
  | FuelRefinery { _iId     :: InstallmentId
                 , _iName   :: InstallmentName
                 , _iSize   :: InstallmentSize
                 , _iCost   :: InstallmentCost
                 , _iRating :: InstallmentRating }
  | ConstructionFactory { _iId     :: InstallmentId
                        , _iName   :: InstallmentName
                        , _iSize   :: InstallmentSize
                        , _iCost   :: InstallmentCost
                        , _iRating :: InstallmentRating }
  | MassDriver { _iId     :: InstallmentId
               , _iName   :: InstallmentName
               , _iSize   :: InstallmentSize
               , _iCost   :: InstallmentCost
               , _iRating :: InstallmentRating }
  | CommercialShipyardComplex { _iId     :: InstallmentId
                              , _iName   :: InstallmentName
                              , _iSize   :: InstallmentSize
                              , _iCost   :: InstallmentCost
                              , _iRating :: InstallmentRating }
  | NavalShipyardComplex { _iId     :: InstallmentId
                         , _iName   :: InstallmentName
                         , _iSize   :: InstallmentSize
                         , _iCost   :: InstallmentCost
                         , _iRating :: InstallmentRating }
  deriving (Show, Eq, Generic)

makeLenses ''Installment

makePrisms ''Installment

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Installment

type Installments = Map.Map InstallmentId InstallmentStack

data InstallmentStack = InstallmentStack
  { _isCount       :: InstallmentCount
  , _isInstallment :: Installment
  } deriving (Show, Eq, Generic)

makeLenses ''InstallmentStack

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''InstallmentStack

data InstallmentResearch = InstallmentResearch
  { _irId               :: InstallmentId
  , _irResearchCost     :: ComponentResearch
  , _irResearchProgress :: ComponentResearch
  , _irInstallment      :: Installment
  } deriving (Show, Eq, Generic)

makeLenses ''InstallmentResearch

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''InstallmentResearch

noInstallments :: Installments
noInstallments = Map.empty
