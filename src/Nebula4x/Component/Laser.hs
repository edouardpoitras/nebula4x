module Nebula4x.Component.Laser where

import           Control.Lens
import           Control.Newtype.Generics
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           System.Random

import           Nebula4x.Component.Armor
import           Nebula4x.Component.MissleLauncher
import           Nebula4x.Types

laserMineralCostRatio :: MineralCost
laserMineralCostRatio =
  Map.fromList [(Duranium, 0.2), (Boronide, 0.2), (Corundium, 0.6)]

--
-- Laser Focal Size
--
laserFocalSize10 :: LaserFocalSize
laserFocalSize10 =
  LaserFocalSize (ComponentName "10 cm") (ComponentSize 3) (ComponentRating 3)

laserFocalSize10Research :: LaserFocalSizeResearch
laserFocalSize10Research = LaserFocalSizeResearch 1
                                                  (ComponentResearch 0)
                                                  (ComponentResearch 0)
                                                  laserFocalSize10

laserFocalSize12 :: LaserFocalSize
laserFocalSize12 =
  LaserFocalSize (ComponentName "12 cm") (ComponentSize 4) (ComponentRating 4)

laserFocalSize12Research :: LaserFocalSizeResearch
laserFocalSize12Research = LaserFocalSizeResearch 2
                                                  (ComponentResearch 2000)
                                                  (ComponentResearch 0)
                                                  laserFocalSize12

laserFocalSize15 :: LaserFocalSize
laserFocalSize15 =
  LaserFocalSize (ComponentName "15 cm") (ComponentSize 4) (ComponentRating 6)

laserFocalSize15Research :: LaserFocalSizeResearch
laserFocalSize15Research = LaserFocalSizeResearch 3
                                                  (ComponentResearch 4000)
                                                  (ComponentResearch 0)
                                                  laserFocalSize15

laserFocalSize20 :: LaserFocalSize
laserFocalSize20 =
  LaserFocalSize (ComponentName "20 cm") (ComponentSize 6) (ComponentRating 10)

laserFocalSize20Research :: LaserFocalSizeResearch
laserFocalSize20Research = LaserFocalSizeResearch 4
                                                  (ComponentResearch 8000)
                                                  (ComponentResearch 0)
                                                  laserFocalSize20

laserFocalSize25 :: LaserFocalSize
laserFocalSize25 =
  LaserFocalSize (ComponentName "25 cm") (ComponentSize 8) (ComponentRating 16)

laserFocalSize25Research :: LaserFocalSizeResearch
laserFocalSize25Research = LaserFocalSizeResearch 5
                                                  (ComponentResearch 15000)
                                                  (ComponentResearch 0)
                                                  laserFocalSize25

laserFocalSize30 :: LaserFocalSize
laserFocalSize30 =
  LaserFocalSize (ComponentName "30 cm") (ComponentSize 9) (ComponentRating 24)

laserFocalSize30Research :: LaserFocalSizeResearch
laserFocalSize30Research = LaserFocalSizeResearch 6
                                                  (ComponentResearch 30000)
                                                  (ComponentResearch 0)
                                                  laserFocalSize30

laserFocalSize35 :: LaserFocalSize
laserFocalSize35 =
  LaserFocalSize (ComponentName "35 cm") (ComponentSize 11) (ComponentRating 32)

laserFocalSize35Research :: LaserFocalSizeResearch
laserFocalSize35Research = LaserFocalSizeResearch 7
                                                  (ComponentResearch 60000)
                                                  (ComponentResearch 0)
                                                  laserFocalSize35

laserFocalSize40 :: LaserFocalSize
laserFocalSize40 =
  LaserFocalSize (ComponentName "40 cm") (ComponentSize 12) (ComponentRating 42)

laserFocalSize40Research :: LaserFocalSizeResearch
laserFocalSize40Research = LaserFocalSizeResearch 8
                                                  (ComponentResearch 125000)
                                                  (ComponentResearch 0)
                                                  laserFocalSize40

laserFocalSize50 :: LaserFocalSize
laserFocalSize50 =
  LaserFocalSize (ComponentName "50 cm") (ComponentSize 16) (ComponentRating 65)

laserFocalSize50Research :: LaserFocalSizeResearch
laserFocalSize50Research = LaserFocalSizeResearch 9
                                                  (ComponentResearch 250000)
                                                  (ComponentResearch 0)
                                                  laserFocalSize50

laserFocalSize60 :: LaserFocalSize
laserFocalSize60 =
  LaserFocalSize (ComponentName "60 cm") (ComponentSize 19) (ComponentRating 94)

laserFocalSize60Research :: LaserFocalSizeResearch
laserFocalSize60Research = LaserFocalSizeResearch 10
                                                  (ComponentResearch 500000)
                                                  (ComponentResearch 0)
                                                  laserFocalSize60

laserFocalSize70 :: LaserFocalSize
laserFocalSize70 = LaserFocalSize (ComponentName "70 cm")
                                  (ComponentSize 22)
                                  (ComponentRating 128)

laserFocalSize70Research :: LaserFocalSizeResearch
laserFocalSize70Research = LaserFocalSizeResearch 11
                                                  (ComponentResearch 1000000)
                                                  (ComponentResearch 0)
                                                  laserFocalSize70

laserFocalSize80 :: LaserFocalSize
laserFocalSize80 = LaserFocalSize (ComponentName "80 cm")
                                  (ComponentSize 25)
                                  (ComponentRating 168)

laserFocalSize80Research :: LaserFocalSizeResearch
laserFocalSize80Research = LaserFocalSizeResearch 12
                                                  (ComponentResearch 2000000)
                                                  (ComponentResearch 0)
                                                  laserFocalSize80

--
-- Laser Wavelength
--
laserWavelengthInfrared :: LaserWavelength
laserWavelengthInfrared =
  LaserWavelength (ComponentName "Infrared") (ComponentRating 1)

laserWavelengthInfraredResearch :: LaserWavelengthResearch
laserWavelengthInfraredResearch = LaserWavelengthResearch
  1
  (ComponentResearch 0)
  (ComponentResearch 0)
  laserWavelengthInfrared

laserWavelengthVisibleLight :: LaserWavelength
laserWavelengthVisibleLight =
  LaserWavelength (ComponentName "Visible Light") (ComponentRating 2)

laserWavelengthVisibleLightResearch :: LaserWavelengthResearch
laserWavelengthVisibleLightResearch = LaserWavelengthResearch
  2
  (ComponentResearch 2000)
  (ComponentResearch 0)
  laserWavelengthVisibleLight

laserWavelengthNearUltraviolet :: LaserWavelength
laserWavelengthNearUltraviolet =
  LaserWavelength (ComponentName "Near Ultraviolet") (ComponentRating 3)

laserWavelengthNearUltravioletResearch :: LaserWavelengthResearch
laserWavelengthNearUltravioletResearch = LaserWavelengthResearch
  3
  (ComponentResearch 4000)
  (ComponentResearch 0)
  laserWavelengthNearUltraviolet

laserWavelengthUltraviolet :: LaserWavelength
laserWavelengthUltraviolet =
  LaserWavelength (ComponentName "Ultraviolet") (ComponentRating 4)

laserWavelengthUltravioletResearch :: LaserWavelengthResearch
laserWavelengthUltravioletResearch = LaserWavelengthResearch
  4
  (ComponentResearch 8000)
  (ComponentResearch 0)
  laserWavelengthUltraviolet

laserWavelengthFarUltraviolet :: LaserWavelength
laserWavelengthFarUltraviolet =
  LaserWavelength (ComponentName "Far Ultraviolet") (ComponentRating 5)

laserWavelengthFarUltravioletResearch :: LaserWavelengthResearch
laserWavelengthFarUltravioletResearch = LaserWavelengthResearch
  5
  (ComponentResearch 15000)
  (ComponentResearch 0)
  laserWavelengthFarUltraviolet

laserWavelengthSoftXRay :: LaserWavelength
laserWavelengthSoftXRay =
  LaserWavelength (ComponentName "Soft X-Ray") (ComponentRating 6)

laserWavelengthSoftXRayResearch :: LaserWavelengthResearch
laserWavelengthSoftXRayResearch = LaserWavelengthResearch
  6
  (ComponentResearch 30000)
  (ComponentResearch 0)
  laserWavelengthSoftXRay

laserWavelengthXRay :: LaserWavelength
laserWavelengthXRay =
  LaserWavelength (ComponentName "X-Ray") (ComponentRating 7)

laserWavelengthXRayResearch :: LaserWavelengthResearch
laserWavelengthXRayResearch = LaserWavelengthResearch
  7
  (ComponentResearch 60000)
  (ComponentResearch 0)
  laserWavelengthXRay

laserWavelengthFarXRay :: LaserWavelength
laserWavelengthFarXRay =
  LaserWavelength (ComponentName "Far X-Ray") (ComponentRating 8)

laserWavelengthFarXRayResearch :: LaserWavelengthResearch
laserWavelengthFarXRayResearch = LaserWavelengthResearch
  8
  (ComponentResearch 125000)
  (ComponentResearch 0)
  laserWavelengthFarXRay

laserWavelengthExtremeXRay :: LaserWavelength
laserWavelengthExtremeXRay =
  LaserWavelength (ComponentName "Extreme X-Ray") (ComponentRating 9)

laserWavelengthExtremeXRayResearch :: LaserWavelengthResearch
laserWavelengthExtremeXRayResearch = LaserWavelengthResearch
  9
  (ComponentResearch 250000)
  (ComponentResearch 0)
  laserWavelengthExtremeXRay

laserWavelengthNearGammaRay :: LaserWavelength
laserWavelengthNearGammaRay =
  LaserWavelength (ComponentName "Near Gamma Ray") (ComponentRating 10)

laserWavelengthNearGammaRayResearch :: LaserWavelengthResearch
laserWavelengthNearGammaRayResearch = LaserWavelengthResearch
  10
  (ComponentResearch 500000)
  (ComponentResearch 0)
  laserWavelengthNearGammaRay

laserWavelengthGammaRay :: LaserWavelength
laserWavelengthGammaRay =
  LaserWavelength (ComponentName "Gamma Ray") (ComponentRating 11)

laserWavelengthGammaRayResearch :: LaserWavelengthResearch
laserWavelengthGammaRayResearch = LaserWavelengthResearch
  11
  (ComponentResearch 1000000)
  (ComponentResearch 0)
  laserWavelengthGammaRay

laserWavelengthFarGammaRay :: LaserWavelength
laserWavelengthFarGammaRay =
  LaserWavelength (ComponentName "Far Gamma Ray") (ComponentRating 12)

laserWavelengthFarGammaRayResearch :: LaserWavelengthResearch
laserWavelengthFarGammaRayResearch = LaserWavelengthResearch
  12
  (ComponentResearch 2000000)
  (ComponentResearch 0)
  laserWavelengthFarGammaRay

--
-- Laser Recharge Rate
--
laserRechargeRate1 :: LaserRechargeRate
laserRechargeRate1 =
  LaserRechargeRate (ComponentName "Recharge Rage 1") (ComponentRating 1)

laserRechargeRate1Research :: LaserRechargeRateResearch
laserRechargeRate1Research = LaserRechargeRateResearch
  1
  (ComponentResearch 1000)
  (ComponentResearch 0)
  laserRechargeRate1

laserRechargeRate2 :: LaserRechargeRate
laserRechargeRate2 =
  LaserRechargeRate (ComponentName "Recharge Rage 2") (ComponentRating 2)

laserRechargeRate2Research :: LaserRechargeRateResearch
laserRechargeRate2Research = LaserRechargeRateResearch
  2
  (ComponentResearch 2000)
  (ComponentResearch 0)
  laserRechargeRate2

laserRechargeRate3 :: LaserRechargeRate
laserRechargeRate3 =
  LaserRechargeRate (ComponentName "Recharge Rage 3") (ComponentRating 3)

laserRechargeRate3Research :: LaserRechargeRateResearch
laserRechargeRate3Research = LaserRechargeRateResearch
  3
  (ComponentResearch 4000)
  (ComponentResearch 0)
  laserRechargeRate3

laserRechargeRate4 :: LaserRechargeRate
laserRechargeRate4 =
  LaserRechargeRate (ComponentName "Recharge Rage 4") (ComponentRating 4)

laserRechargeRate4Research :: LaserRechargeRateResearch
laserRechargeRate4Research = LaserRechargeRateResearch
  4
  (ComponentResearch 8000)
  (ComponentResearch 0)
  laserRechargeRate4

laserRechargeRate5 :: LaserRechargeRate
laserRechargeRate5 =
  LaserRechargeRate (ComponentName "Recharge Rage 5") (ComponentRating 5)

laserRechargeRate5Research :: LaserRechargeRateResearch
laserRechargeRate5Research = LaserRechargeRateResearch
  5
  (ComponentResearch 15000)
  (ComponentResearch 0)
  laserRechargeRate5

laserRechargeRate6 :: LaserRechargeRate
laserRechargeRate6 =
  LaserRechargeRate (ComponentName "Recharge Rage 6") (ComponentRating 6)

laserRechargeRate6Research :: LaserRechargeRateResearch
laserRechargeRate6Research = LaserRechargeRateResearch
  6
  (ComponentResearch 30000)
  (ComponentResearch 0)
  laserRechargeRate6

laserRechargeRate8 :: LaserRechargeRate
laserRechargeRate8 =
  LaserRechargeRate (ComponentName "Recharge Rage 8") (ComponentRating 8)

laserRechargeRate8Research :: LaserRechargeRateResearch
laserRechargeRate8Research = LaserRechargeRateResearch
  7
  (ComponentResearch 60000)
  (ComponentResearch 0)
  laserRechargeRate8

laserRechargeRate10 :: LaserRechargeRate
laserRechargeRate10 =
  LaserRechargeRate (ComponentName "Recharge Rage 10") (ComponentRating 10)

laserRechargeRate10Research :: LaserRechargeRateResearch
laserRechargeRate10Research = LaserRechargeRateResearch
  8
  (ComponentResearch 125000)
  (ComponentResearch 0)
  laserRechargeRate10

laserRechargeRate12 :: LaserRechargeRate
laserRechargeRate12 =
  LaserRechargeRate (ComponentName "Recharge Rage 12") (ComponentRating 12)

laserRechargeRate12Research :: LaserRechargeRateResearch
laserRechargeRate12Research = LaserRechargeRateResearch
  9
  (ComponentResearch 250000)
  (ComponentResearch 0)
  laserRechargeRate12

laserRechargeRate16 :: LaserRechargeRate
laserRechargeRate16 =
  LaserRechargeRate (ComponentName "Recharge Rage 16") (ComponentRating 16)

laserRechargeRate16Research :: LaserRechargeRateResearch
laserRechargeRate16Research = LaserRechargeRateResearch
  10
  (ComponentResearch 500000)
  (ComponentResearch 0)
  laserRechargeRate16

laserRechargeRate20 :: LaserRechargeRate
laserRechargeRate20 =
  LaserRechargeRate (ComponentName "Recharge Rage 20") (ComponentRating 20)

laserRechargeRate20Research :: LaserRechargeRateResearch
laserRechargeRate20Research = LaserRechargeRateResearch
  11
  (ComponentResearch 1000000)
  (ComponentResearch 0)
  laserRechargeRate20

laserRechargeRate25 :: LaserRechargeRate
laserRechargeRate25 =
  LaserRechargeRate (ComponentName "Recharge Rage 25") (ComponentRating 25)

laserRechargeRate25Research :: LaserRechargeRateResearch
laserRechargeRate25Research = LaserRechargeRateResearch
  12
  (ComponentResearch 2000000)
  (ComponentResearch 0)
  laserRechargeRate25

--
-- Laser Reduced Size
--
laserReducedSize1 :: LaserReducedSize
laserReducedSize1 = LaserReducedSize
  (ComponentName "Size x1.00 / Recharge Rate x1.00")
  (ComponentRating 1)
  (ComponentRating 1)

laserReducedSize1Research :: LaserReducedSizeResearch
laserReducedSize1Research = LaserReducedSizeResearch 1
                                                     (ComponentResearch 1000)
                                                     (ComponentResearch 0)
                                                     laserReducedSize1

laserReducedSize075 :: LaserReducedSize
laserReducedSize075 = LaserReducedSize
  (ComponentName "Size x0.75 / Recharge Rate x4")
  (ComponentRating 0.75)
  (ComponentRating 4)

laserReducedSize075Research :: LaserReducedSizeResearch
laserReducedSize075Research = LaserReducedSizeResearch
  2
  (ComponentResearch 5000)
  (ComponentResearch 0)
  laserReducedSize075

laserReducedSize05 :: LaserReducedSize
laserReducedSize05 = LaserReducedSize
  (ComponentName "Size x0.5 / Recharge Rate x20")
  (ComponentRating 0.5)
  (ComponentRating 20)

laserReducedSize05Research :: LaserReducedSizeResearch
laserReducedSize05Research = LaserReducedSizeResearch
  3
  (ComponentResearch 20000)
  (ComponentResearch 0)
  laserReducedSize05

laserReducedSize025 :: LaserReducedSize
laserReducedSize025 = LaserReducedSize
  (ComponentName "Size x0.25 / Recharge Rate x100")
  (ComponentRating 0.25)
  (ComponentRating 100)

laserReducedSize025Research :: LaserReducedSizeResearch
laserReducedSize025Research = LaserReducedSizeResearch
  4
  (ComponentResearch 100000)
  (ComponentResearch 0)
  laserReducedSize025

--
-- Laser Designs
--
newLaserDesign
  :: ComponentId
  -> ComponentName
  -> LaserFocalSize
  -> LaserWavelength
  -> LaserRechargeRate
  -> LaserReducedSize
  -> LaserDesign
newLaserDesign cId name (LaserFocalSize _ (ComponentSize size) (ComponentRating damage)) (LaserWavelength _ (ComponentRating rangeModifier)) (LaserRechargeRate _ (ComponentRating rechargeRate)) (LaserReducedSize _ (ComponentRating reducedSize) (ComponentRating rechargeMultiplier))
  = laserDesign where
  laserDesign = LaserDesign cId
                            (ComponentResearch researchCost)
                            (ComponentResearch 0.0)
                            laser
  laser = Laser name
                (pack cost)
                (pack finalSize)
                (pack finalRange)
                (pack rechargeRatePerSecond)
                (pack damage)
  -- TODO: Need to figure out if this cost works
  cost                  = finalSize * (rangeModifier / 2)
  finalSize             = size * reducedSize
  researchCost          = 10 * cost
  secondsToRecharge     = damage * 5 / rechargeRate * rechargeMultiplier
  rechargeRatePerSecond = 1 / secondsToRecharge
  finalRange            = damage * 10000 * rangeModifier

damageBodyWithLaser :: RandomGen g => g -> Int -> Body -> (Body, g)
damageBodyWithLaser gen damage bdy = (newBody, newGen) where
  (randomSelection, gen'  ) = randomR (0 :: Int, 3 :: Int) gen
  (newBody        , newGen) = case randomSelection of
    0 -> destroyBodyShipyards gen' damage bdy
    1 -> destroyBodyFuelReserves gen' damage bdy
    2 -> destroyBodyInstallments gen' damage bdy
    3 -> destroyBodyMinerals gen' damage bdy
    _ -> (bdy, gen')

lasersAttackShip :: RandomGen g => g -> Ship -> Lasers -> (Maybe Ship, g)
lasersAttackShip gen shp []               = (Just shp, gen)
lasersAttackShip gen shp (laser : lasers) = (maybeShp, newGen) where
  (shp'    , gen'  ) = laserAttackShip gen shp laser
  (maybeShp, newGen) = if isNothing shp'
    then (Nothing, gen')
    else lasersAttackShip gen' (fromJust shp') lasers

laserAttackShip :: RandomGen g => g -> Ship -> Laser -> (Maybe Ship, g)
laserAttackShip gen shp laser = (maybeShp, newGen) where
  (ComponentRating damage)    = view lDamage laser
  (shp'  , damage'  , gen'  ) = laserShipMiss gen shp (floor damage)
  (shp'' , damage'' , gen'' ) = laserShipShieldDamage gen' shp' damage'
  (shp''', damage''', gen''') = laserShipArmorDamage gen'' shp'' damage''
  (maybeShp, newGen)          = laserShipDamage gen''' shp''' damage'''

-- TODO: Figure out miss chance based on distance maybe?
laserShipMiss :: g -> Ship -> Int -> (Ship, Int, g)
laserShipMiss gen shp damage = (shp, damage, gen)

laserShipShieldDamage :: g -> Ship -> Int -> (Ship, Int, g)
laserShipShieldDamage gen ship 0      = (ship, 0, gen)
laserShipShieldDamage gen ship damage = shieldCheck where
  maybeShipShields = view sShields ship
  shieldsEnabled   = view ssEnabled shipShields
  shieldCheck      = if isJust maybeShipShields && shieldsEnabled
    then (newShip, newDamage, gen)
    else (ship, damage, gen)
  shipShields                      = fromJust maybeShipShields
  (ComponentRating shieldStrength) = view ssCurrentCapacity shipShields
  (newShieldStrength, newDamage) =
    calculateLaserShieldDamage shieldStrength damage
  newShip = ship
    { _sShields = Just $ shipShields
                    { _ssCurrentCapacity = ComponentRating newShieldStrength
                    }
    }

calculateLaserShieldDamage :: Double -> Int -> (Double, Int)
calculateLaserShieldDamage shieldStrength shieldDamage =
  (newShieldStrength, newDamage) where
  newShieldStrength = maximum [0, shieldStrength - fromIntegral shieldDamage]
  newDamage         = maximum [0, shieldDamage - floor shieldStrength]

laserShipArmorDamage :: RandomGen g => g -> Ship -> Int -> (Ship, Int, g)
laserShipArmorDamage gen ship 0      = (ship, 0, gen)
laserShipArmorDamage gen ship damage = (newShip, newDamage, newGen) where
  (ShipArmor shipArmor armorGrid) = view sArmor ship
  (randomCol, newGen)             = randomR (0, length (armorGrid !! 0) - 1) gen
  (newArmorGrid, newDamage) =
    updateArmorGridLaserDamage armorGrid randomCol damage
  newShip = ship { _sArmor = ShipArmor shipArmor newArmorGrid }

laserShipDamage :: RandomGen g => g -> Ship -> Int -> (Maybe Ship, g)
laserShipDamage gen shp 0      = (Just shp, gen)
laserShipDamage gen shp damage = (maybeShp, newGen) where
  (ShipSize shipSize)     = view (sDesign . sdSize) shp
  (destroyChance, gen'  ) = randomR (0, shipSize) gen
  (newShp       , gen'' ) = destroyShipComponent gen' (fromIntegral damage) shp
  (maybeShp     , newGen) = if destroyChance < fromIntegral damage
    then (Nothing, gen'')
    else (Just newShp, gen'')
