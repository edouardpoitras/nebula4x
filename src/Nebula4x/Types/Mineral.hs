{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Nebula4x.Types.Mineral where

import           Control.Lens
import           Control.Newtype.Generics
import           Data.Aeson
import qualified Data.Aeson.Encoding      as DAE
import           Data.Aeson.TH
import qualified Data.Map.Strict          as Map
import qualified Data.Text                as Text
import           GHC.Generics             (Generic)

data Element
  = Duranium
  | Sorium
  | Neutronium
  | Corbomite
  | Tritanium
  | Boronide
  | Uridium
  | Corundium
  | Mercassium
  | Vendarite
  | Gallicite
  deriving (Show, Read, Eq, Ord, Generic)

makeLenses ''Element

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Element

instance FromJSONKey Element where
  fromJSONKey = FromJSONKeyText (read . Text.unpack)

instance ToJSONKey Element where
  toJSONKey = ToJSONKeyText f g
    where
      f = Text.pack . show
      g = DAE.text . Text.pack . show

type Elements = [Element]

type ElementDescription = String

type MineralCost = Map.Map Element Double

type AvailableMinerals = MineralCost

newtype Accessibility =
  Accessibility Double
  deriving (Show, Eq, Generic)

instance ToJSON Accessibility

instance FromJSON Accessibility

instance Newtype Accessibility

newtype MineralCount =
  MineralCount Double
  deriving (Show, Eq, Generic)

instance ToJSON MineralCount

instance FromJSON MineralCount

instance Newtype MineralCount

data Mineral = Mineral
  { _mElement       :: Element
  , _mAccessibility :: Accessibility
  , _mCount         :: MineralCount
  , _mStockpile     :: MineralCount
  } deriving (Show, Eq, Generic)

makeLenses ''Mineral

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Mineral

type Minerals = Map.Map Element Mineral

data MineralStack = MineralStack
  { _minsElement :: Element
  , _minsCount   :: MineralCount
  } deriving (Show, Eq, Generic)

makeLenses ''MineralStack

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''MineralStack

type MineralStacks = Map.Map Element MineralStack
