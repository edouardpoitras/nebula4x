{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Nebula4x.Body.Star where

import           Control.Lens
import           System.Random

import           Nebula4x.Config
import           Nebula4x.Types
import           Nebula4x.Utils

generateStar :: RandomGen g => g -> StarConfig -> (Star, g)
generateStar g sConfig =
  (Star sid (BodyName name) (BodyMass mass) (BodyRadius radius), radiusG)
  where
    (sid, sidG) = randomId g
    (name, nameG) = generateStarName sidG
    (mass, massG) = randomR (minStarMass, maxStarMass) nameG
    (radius, radiusG) = randomR (minStarRadius, maxStarRadius) massG
    (Range (BodyMass minStarMass) (BodyMass maxStarMass)) =
      view scStarMass sConfig
    (Range (BodyRadius minStarRadius) (BodyRadius maxStarRadius)) =
      view scStarRadius sConfig
