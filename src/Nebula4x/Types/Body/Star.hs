{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Nebula4x.Types.Body.Star where

import           Control.Lens
import           Data.Aeson.TH
import           GHC.Generics        (Generic)

import           Nebula4x.Types.Body

data Star = Star
  { _starId     :: BodyId
  , _starName   :: BodyName
  , _starMass   :: BodyMass
  , _starRadius :: BodyRadius
  } deriving (Show, Eq, Generic)

makeLenses ''Star

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Star
