module Nebula4x.Component.MissleLauncher where

import           Control.Lens
import           Control.Newtype.Generics
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           System.Random

import           Nebula4x.Component.Armor
import           Nebula4x.Types

missleLauncherMineralCostRatio :: MineralCost
missleLauncherMineralCostRatio =
  Map.fromList [(Duranium, 0.25), (Tritanium, 0.75)]

--
-- Missle Launcher Reload Rate
--
missleLauncherReloadRate1 :: MissleLauncherReloadRate
missleLauncherReloadRate1 =
  MissleLauncherReloadRate (ComponentName "Reload Rate 1") (ComponentRating 1)

missleLauncherReloadRate1Research :: MissleLauncherReloadRateResearch
missleLauncherReloadRate1Research = MissleLauncherReloadRateResearch
  1
  (ComponentResearch 0)
  (ComponentResearch 0)
  missleLauncherReloadRate1

missleLauncherReloadRate2 :: MissleLauncherReloadRate
missleLauncherReloadRate2 =
  MissleLauncherReloadRate (ComponentName "Reload Rate 2") (ComponentRating 2)

missleLauncherReloadRate2Research :: MissleLauncherReloadRateResearch
missleLauncherReloadRate2Research = MissleLauncherReloadRateResearch
  2
  (ComponentResearch 2000)
  (ComponentResearch 0)
  missleLauncherReloadRate2

missleLauncherReloadRate3 :: MissleLauncherReloadRate
missleLauncherReloadRate3 =
  MissleLauncherReloadRate (ComponentName "Reload Rate 3") (ComponentRating 3)

missleLauncherReloadRate3Research :: MissleLauncherReloadRateResearch
missleLauncherReloadRate3Research = MissleLauncherReloadRateResearch
  3
  (ComponentResearch 4000)
  (ComponentResearch 0)
  missleLauncherReloadRate3

missleLauncherReloadRate4 :: MissleLauncherReloadRate
missleLauncherReloadRate4 =
  MissleLauncherReloadRate (ComponentName "Reload Rate 4") (ComponentRating 4)

missleLauncherReloadRate4Research :: MissleLauncherReloadRateResearch
missleLauncherReloadRate4Research = MissleLauncherReloadRateResearch
  4
  (ComponentResearch 8000)
  (ComponentResearch 0)
  missleLauncherReloadRate4

missleLauncherReloadRate5 :: MissleLauncherReloadRate
missleLauncherReloadRate5 =
  MissleLauncherReloadRate (ComponentName "Reload Rate 5") (ComponentRating 5)

missleLauncherReloadRate5Research :: MissleLauncherReloadRateResearch
missleLauncherReloadRate5Research = MissleLauncherReloadRateResearch
  5
  (ComponentResearch 15000)
  (ComponentResearch 0)
  missleLauncherReloadRate5

missleLauncherReloadRate6 :: MissleLauncherReloadRate
missleLauncherReloadRate6 =
  MissleLauncherReloadRate (ComponentName "Reload Rate 6") (ComponentRating 6)

missleLauncherReloadRate6Research :: MissleLauncherReloadRateResearch
missleLauncherReloadRate6Research = MissleLauncherReloadRateResearch
  6
  (ComponentResearch 30000)
  (ComponentResearch 0)
  missleLauncherReloadRate6

missleLauncherReloadRate7 :: MissleLauncherReloadRate
missleLauncherReloadRate7 =
  MissleLauncherReloadRate (ComponentName "Reload Rate 7") (ComponentRating 7)

missleLauncherReloadRate7Research :: MissleLauncherReloadRateResearch
missleLauncherReloadRate7Research = MissleLauncherReloadRateResearch
  7
  (ComponentResearch 60000)
  (ComponentResearch 0)
  missleLauncherReloadRate7

missleLauncherReloadRate8 :: MissleLauncherReloadRate
missleLauncherReloadRate8 =
  MissleLauncherReloadRate (ComponentName "Reload Rate 8") (ComponentRating 8)

missleLauncherReloadRate8Research :: MissleLauncherReloadRateResearch
missleLauncherReloadRate8Research = MissleLauncherReloadRateResearch
  8
  (ComponentResearch 125000)
  (ComponentResearch 0)
  missleLauncherReloadRate8

missleLauncherReloadRate9 :: MissleLauncherReloadRate
missleLauncherReloadRate9 =
  MissleLauncherReloadRate (ComponentName "Reload Rate 9") (ComponentRating 9)

missleLauncherReloadRate9Research :: MissleLauncherReloadRateResearch
missleLauncherReloadRate9Research = MissleLauncherReloadRateResearch
  9
  (ComponentResearch 250000)
  (ComponentResearch 0)
  missleLauncherReloadRate9

missleLauncherReloadRate10 :: MissleLauncherReloadRate
missleLauncherReloadRate10 =
  MissleLauncherReloadRate (ComponentName "Reload Rate 10") (ComponentRating 10)

missleLauncherReloadRate10Research :: MissleLauncherReloadRateResearch
missleLauncherReloadRate10Research = MissleLauncherReloadRateResearch
  10
  (ComponentResearch 500000)
  (ComponentResearch 0)
  missleLauncherReloadRate10

missleLauncherReloadRate11 :: MissleLauncherReloadRate
missleLauncherReloadRate11 =
  MissleLauncherReloadRate (ComponentName "Reload Rate 11") (ComponentRating 11)

missleLauncherReloadRate11Research :: MissleLauncherReloadRateResearch
missleLauncherReloadRate11Research = MissleLauncherReloadRateResearch
  11
  (ComponentResearch 1000000)
  (ComponentResearch 0)
  missleLauncherReloadRate11

missleLauncherReloadRate12 :: MissleLauncherReloadRate
missleLauncherReloadRate12 =
  MissleLauncherReloadRate (ComponentName "Reload Rate 12") (ComponentRating 12)

missleLauncherReloadRate12Research :: MissleLauncherReloadRateResearch
missleLauncherReloadRate12Research = MissleLauncherReloadRateResearch
  12
  (ComponentResearch 2000000)
  (ComponentResearch 0)
  missleLauncherReloadRate12

--
-- Missle Launcher Size
--
missleLauncherSize1 :: MissleLauncherSize
missleLauncherSize1 = MissleLauncherSize
  (ComponentName "Missle Launcher Size 1")
  (ComponentRating 1)

missleLauncherSize1Research :: MissleLauncherSizeResearch
missleLauncherSize1Research = MissleLauncherSizeResearch
  1
  (ComponentResearch 0)
  (ComponentResearch 0)
  missleLauncherSize1

missleLauncherSize2 :: MissleLauncherSize
missleLauncherSize2 = MissleLauncherSize
  (ComponentName "Missle Launcher Size 2")
  (ComponentRating 2)

missleLauncherSize2Research :: MissleLauncherSizeResearch
missleLauncherSize2Research = MissleLauncherSizeResearch
  2
  (ComponentResearch 0)
  (ComponentResearch 0)
  missleLauncherSize2

missleLauncherSize3 :: MissleLauncherSize
missleLauncherSize3 = MissleLauncherSize
  (ComponentName "Missle Launcher Size 3")
  (ComponentRating 3)

missleLauncherSize3Research :: MissleLauncherSizeResearch
missleLauncherSize3Research = MissleLauncherSizeResearch
  3
  (ComponentResearch 0)
  (ComponentResearch 0)
  missleLauncherSize3

missleLauncherSize4 :: MissleLauncherSize
missleLauncherSize4 = MissleLauncherSize
  (ComponentName "Missle Launcher Size 4")
  (ComponentRating 4)

missleLauncherSize4Research :: MissleLauncherSizeResearch
missleLauncherSize4Research = MissleLauncherSizeResearch
  4
  (ComponentResearch 0)
  (ComponentResearch 0)
  missleLauncherSize4

missleLauncherSize5 :: MissleLauncherSize
missleLauncherSize5 = MissleLauncherSize
  (ComponentName "Missle Launcher Size 5")
  (ComponentRating 5)

missleLauncherSize5Research :: MissleLauncherSizeResearch
missleLauncherSize5Research = MissleLauncherSizeResearch
  5
  (ComponentResearch 0)
  (ComponentResearch 0)
  missleLauncherSize5

missleLauncherSize6 :: MissleLauncherSize
missleLauncherSize6 = MissleLauncherSize
  (ComponentName "Missle Launcher Size 6")
  (ComponentRating 6)

missleLauncherSize6Research :: MissleLauncherSizeResearch
missleLauncherSize6Research = MissleLauncherSizeResearch
  6
  (ComponentResearch 0)
  (ComponentResearch 0)
  missleLauncherSize6

missleLauncherSize8 :: MissleLauncherSize
missleLauncherSize8 = MissleLauncherSize
  (ComponentName "Missle Launcher Size 8")
  (ComponentRating 8)

missleLauncherSize8Research :: MissleLauncherSizeResearch
missleLauncherSize8Research = MissleLauncherSizeResearch
  7
  (ComponentResearch 0)
  (ComponentResearch 0)
  missleLauncherSize8

missleLauncherSize10 :: MissleLauncherSize
missleLauncherSize10 = MissleLauncherSize
  (ComponentName "Missle Launcher Size 10")
  (ComponentRating 10)

missleLauncherSize10Research :: MissleLauncherSizeResearch
missleLauncherSize10Research = MissleLauncherSizeResearch
  8
  (ComponentResearch 0)
  (ComponentResearch 0)
  missleLauncherSize10

missleLauncherSize12 :: MissleLauncherSize
missleLauncherSize12 = MissleLauncherSize
  (ComponentName "Missle Launcher Size 12")
  (ComponentRating 12)

missleLauncherSize12Research :: MissleLauncherSizeResearch
missleLauncherSize12Research = MissleLauncherSizeResearch
  9
  (ComponentResearch 0)
  (ComponentResearch 0)
  missleLauncherSize12

missleLauncherSize15 :: MissleLauncherSize
missleLauncherSize15 = MissleLauncherSize
  (ComponentName "Missle Launcher Size 15")
  (ComponentRating 15)

missleLauncherSize15Research :: MissleLauncherSizeResearch
missleLauncherSize15Research = MissleLauncherSizeResearch
  10
  (ComponentResearch 0)
  (ComponentResearch 0)
  missleLauncherSize15

missleLauncherSize20 :: MissleLauncherSize
missleLauncherSize20 = MissleLauncherSize
  (ComponentName "Missle Launcher Size 20")
  (ComponentRating 20)

missleLauncherSize20Research :: MissleLauncherSizeResearch
missleLauncherSize20Research = MissleLauncherSizeResearch
  11
  (ComponentResearch 0)
  (ComponentResearch 0)
  missleLauncherSize20

missleLauncherSize25 :: MissleLauncherSize
missleLauncherSize25 = MissleLauncherSize
  (ComponentName "Missle Launcher Size 25")
  (ComponentRating 25)

missleLauncherSize25Research :: MissleLauncherSizeResearch
missleLauncherSize25Research = MissleLauncherSizeResearch
  12
  (ComponentResearch 0)
  (ComponentResearch 0)
  missleLauncherSize25

missleLauncherSize30 :: MissleLauncherSize
missleLauncherSize30 = MissleLauncherSize
  (ComponentName "Missle Launcher Size 30")
  (ComponentRating 30)

missleLauncherSize30Research :: MissleLauncherSizeResearch
missleLauncherSize30Research = MissleLauncherSizeResearch
  13
  (ComponentResearch 0)
  (ComponentResearch 0)
  missleLauncherSize30

missleLauncherSize35 :: MissleLauncherSize
missleLauncherSize35 = MissleLauncherSize
  (ComponentName "Missle Launcher Size 35")
  (ComponentRating 35)

missleLauncherSize35Research :: MissleLauncherSizeResearch
missleLauncherSize35Research = MissleLauncherSizeResearch
  14
  (ComponentResearch 0)
  (ComponentResearch 0)
  missleLauncherSize35

missleLauncherSize40 :: MissleLauncherSize
missleLauncherSize40 = MissleLauncherSize
  (ComponentName "Missle Launcher Size 40")
  (ComponentRating 40)

missleLauncherSize40Research :: MissleLauncherSizeResearch
missleLauncherSize40Research = MissleLauncherSizeResearch
  15
  (ComponentResearch 0)
  (ComponentResearch 0)
  missleLauncherSize40

missleLauncherSize45 :: MissleLauncherSize
missleLauncherSize45 = MissleLauncherSize
  (ComponentName "Missle Launcher Size 45")
  (ComponentRating 45)

missleLauncherSize45Research :: MissleLauncherSizeResearch
missleLauncherSize45Research = MissleLauncherSizeResearch
  16
  (ComponentResearch 0)
  (ComponentResearch 0)
  missleLauncherSize45

missleLauncherSize50 :: MissleLauncherSize
missleLauncherSize50 = MissleLauncherSize
  (ComponentName "Missle Launcher Size 50")
  (ComponentRating 50)

missleLauncherSize50Research :: MissleLauncherSizeResearch
missleLauncherSize50Research = MissleLauncherSizeResearch
  17
  (ComponentResearch 0)
  (ComponentResearch 0)
  missleLauncherSize50

missleLauncherSize60 :: MissleLauncherSize
missleLauncherSize60 = MissleLauncherSize
  (ComponentName "Missle Launcher Size 60")
  (ComponentRating 60)

missleLauncherSize60Research :: MissleLauncherSizeResearch
missleLauncherSize60Research = MissleLauncherSizeResearch
  18
  (ComponentResearch 0)
  (ComponentResearch 0)
  missleLauncherSize60

missleLauncherSize70 :: MissleLauncherSize
missleLauncherSize70 = MissleLauncherSize
  (ComponentName "Missle Launcher Size 70")
  (ComponentRating 70)

missleLauncherSize70Research :: MissleLauncherSizeResearch
missleLauncherSize70Research = MissleLauncherSizeResearch
  19
  (ComponentResearch 0)
  (ComponentResearch 0)
  missleLauncherSize70

missleLauncherSize80 :: MissleLauncherSize
missleLauncherSize80 = MissleLauncherSize
  (ComponentName "Missle Launcher Size 80")
  (ComponentRating 80)

missleLauncherSize80Research :: MissleLauncherSizeResearch
missleLauncherSize80Research = MissleLauncherSizeResearch
  20
  (ComponentResearch 0)
  (ComponentResearch 0)
  missleLauncherSize80

missleLauncherSize90 :: MissleLauncherSize
missleLauncherSize90 = MissleLauncherSize
  (ComponentName "Missle Launcher Size 90")
  (ComponentRating 90)

missleLauncherSize90Research :: MissleLauncherSizeResearch
missleLauncherSize90Research = MissleLauncherSizeResearch
  21
  (ComponentResearch 0)
  (ComponentResearch 0)
  missleLauncherSize90

missleLauncherSize100 :: MissleLauncherSize
missleLauncherSize100 = MissleLauncherSize
  (ComponentName "Missle Launcher Size 100")
  (ComponentRating 100)

missleLauncherSize100Research :: MissleLauncherSizeResearch
missleLauncherSize100Research = MissleLauncherSizeResearch
  22
  (ComponentResearch 0)
  (ComponentResearch 0)
  missleLauncherSize100

--
-- Missle Launcher Reduced Size
--
missleLauncherReducedSize1 :: MissleLauncherReducedSize
missleLauncherReducedSize1 = MissleLauncherReducedSize
  (ComponentName "Missle Launcher Reduced Size 1")
  (ComponentRating 1)
  (ComponentRating 1)

missleLauncherReducedSize1Research :: MissleLauncherReducedSizeResearch
missleLauncherReducedSize1Research = MissleLauncherReducedSizeResearch
  1
  (ComponentResearch 0.0)
  (ComponentResearch 0.0)
  missleLauncherReducedSize1

missleLauncherReducedSize075 :: MissleLauncherReducedSize
missleLauncherReducedSize075 = MissleLauncherReducedSize
  (ComponentName "Missle Launcher Reduced Size 25%")
  (ComponentRating 0.75)
  (ComponentRating 2)

missleLauncherReducedSize075Research :: MissleLauncherReducedSizeResearch
missleLauncherReducedSize075Research = MissleLauncherReducedSizeResearch
  2
  (ComponentResearch 1000)
  (ComponentResearch 0.0)
  missleLauncherReducedSize075

missleLauncherReducedSize05 :: MissleLauncherReducedSize
missleLauncherReducedSize05 = MissleLauncherReducedSize
  (ComponentName "Missle Launcher Reduced Size 50%")
  (ComponentRating 0.5)
  (ComponentRating 5)

missleLauncherReducedSize05Research :: MissleLauncherReducedSizeResearch
missleLauncherReducedSize05Research = MissleLauncherReducedSizeResearch
  3
  (ComponentResearch 2000)
  (ComponentResearch 0.0)
  missleLauncherReducedSize05

missleLauncherReducedSize033 :: MissleLauncherReducedSize
missleLauncherReducedSize033 = MissleLauncherReducedSize
  (ComponentName "Missle Launcher Reduced Size 66%")
  (ComponentRating 0.33)
  (ComponentRating 20)

missleLauncherReducedSize033Research :: MissleLauncherReducedSizeResearch
missleLauncherReducedSize033Research = MissleLauncherReducedSizeResearch
  4
  (ComponentResearch 3000)
  (ComponentResearch 0.0)
  missleLauncherReducedSize033

missleLauncherReducedSize025 :: MissleLauncherReducedSize
missleLauncherReducedSize025 = MissleLauncherReducedSize
  (ComponentName "Missle Launcher Reduced Size 75%")
  (ComponentRating 0.25)
  (ComponentRating 100)

missleLauncherReducedSize025Research :: MissleLauncherReducedSizeResearch
missleLauncherReducedSize025Research = MissleLauncherReducedSizeResearch
  5
  (ComponentResearch 6000)
  (ComponentResearch 0.0)
  missleLauncherReducedSize025

missleLauncherReducedSize015 :: MissleLauncherReducedSize
missleLauncherReducedSize015 = MissleLauncherReducedSize
  (ComponentName "Missle Launcher Reduced Size 85%")
  (ComponentRating 0.15)
  (ComponentRating 500)

missleLauncherReducedSize015Research :: MissleLauncherReducedSizeResearch
missleLauncherReducedSize015Research = MissleLauncherReducedSizeResearch
  6
  (ComponentResearch 10000)
  (ComponentResearch 0.0)
  missleLauncherReducedSize015

--
-- Missle Launcher Designs
--
newMissleLauncherDesign
  :: ComponentId
  -> ComponentName
  -> MissleLauncherSize
  -> MissleLauncherReloadRate
  -> MissleLauncherReducedSize
  -> MissleLauncherDesign
newMissleLauncherDesign cId name (MissleLauncherSize _ (ComponentRating size)) (MissleLauncherReloadRate _ (ComponentRating reloadRate)) (MissleLauncherReducedSize _ (ComponentRating reducedSize) (ComponentRating reloadMultiplier))
  = missleLauncherDesign where
  missleLauncherDesign = MissleLauncherDesign
    cId
    (ComponentResearch researchCost)
    (ComponentResearch 0.0)
    missleLauncher
  missleLauncher = MissleLauncher name
                                  (pack cost)
                                  (pack finalSize)
                                  (pack reloadRatePerSecond)
                                  missle
  cost                = 4 * finalSize * (1 + ((reloadRate - 1) * 0.25))
  finalSize           = size * reducedSize * 10 -- For now, launcher is 10x the weight of it's size
  researchCost        = 10 * cost
  secondsToReload     = 30 * size / reloadRate * reloadMultiplier
  reloadRatePerSecond = 1 / secondsToReload
  missle              = Missle (MissleSpeed missleSpeed)
                               (MissleRange missleRange)
                               (MissleStrength size) -- For now, missle strength is equal to launcher size
  missleSpeed = 20000 -- All missles 20,000 km/s for now
  missleRange = 50000000 -- All missles 50,000,000 km for now

updateMissle
  :: RandomGen g
  => g
  -> Seconds
  -> Ships
  -> Bodies
  -> MissleSalvo
  -> ( Ships
     , Bodies
     , Maybe MissleSalvo
     , g
     )
updateMissle gen secs ships' bodies missleSalvo = noTargetCheck where
  targetType     = view (msTarget . mtType) missleSalvo
  targetId       = view (msTarget . mtTarget) missleSalvo
  noTargetCheck = if targetType == ShipTarget && Map.member targetId ships'
    then attackShip
    else if Map.member targetId bodies
         then attackBody
         else (ships', bodies, Nothing, gen)
  attackShip =
    (newShips, bodies, maybeMissleShip, genShip)
  attackBody =
    (ships', newBodies, maybeMissleBody, genBody)
  newShips = if isJust maybeShip
    then Map.insert targetId (fromJust maybeShip) ships'
    else Map.delete targetId ships'
  (maybeShip, maybeMissleShip, genShip) = updateMissleShip
    gen
    secs
    (fromJust $ Map.lookup targetId ships')
    missleSalvo
  newBodies = Map.insert targetId newBody bodies
  (newBody, maybeMissleBody, genBody) = updateMissleBody
    gen
    secs
    (fromJust $ Map.lookup targetId bodies)
    missleSalvo

updateMissleShip
  :: RandomGen g
  => g
  -> Seconds
  -> Ship
  -> MissleSalvo
  -> (Maybe Ship, Maybe MissleSalvo, g)
updateMissleShip gen secs ship missleSalvo = rangeCheck where
  (ShipLocation sX sY)              = view sLocation ship
  (arrivedAtTarget, newMissleSalvo) = moveMissle secs sX sY missleSalvo
  (MissleRange newMissleRange)      = view msRange newMissleSalvo
  rangeCheck                        = if newMissleRange <= 0
    then (Just ship, Nothing, gen)
    else if arrivedAtTarget
      then missCheck
      else (Just ship, Just newMissleSalvo, gen)
  (maybeMissleSalvoMissCheck, missGen) = missleShipMiss gen ship missleSalvo
  missCheck                            = if isNothing maybeMissleSalvoMissCheck
    then (Just ship, Nothing, missGen)
    else shieldCheck
  (shipShieldDamaged, maybeSalvoAfterShield, shieldGen) =
    missleShipShieldDamage missGen ship (fromJust maybeMissleSalvoMissCheck)
  shieldCheck = if isNothing maybeSalvoAfterShield
    then (Just shipShieldDamaged, Nothing, shieldGen)
    else armorCheck
  (shipArmorDamaged, maybeSalvoAfterArmor, armorGen) = missleShipArmorDamage
    shieldGen
    shipShieldDamaged
    (fromJust maybeSalvoAfterShield)
  armorCheck = if isNothing maybeSalvoAfterArmor
    then (Just shipArmorDamaged, Nothing, armorGen)
    else (maybeShipDamaged, Nothing, damageGen)
  (maybeShipDamaged, damageGen) =
    missleShipDamage armorGen shipArmorDamaged (fromJust maybeSalvoAfterArmor)

updateMissleBody
  :: RandomGen g
  => g
  -> Seconds
  -> Body
  -> MissleSalvo
  -> (Body, Maybe MissleSalvo, g)
updateMissleBody gen secs bdy missleSalvo = rangeCheck where
  (BodyLocation bX bY)              = view bLocation bdy
  (arrivedAtTarget, newMissleSalvo) = moveMissle secs bX bY missleSalvo
  (MissleRange newMissleRange)      = view msRange newMissleSalvo
  rangeCheck                        = if newMissleRange <= 0
    then (bdy, Nothing, gen)
    else if arrivedAtTarget
      then (damagedBody, Nothing, damageGen)
      else (bdy, Just newMissleSalvo, gen)
  (damagedBody, damageGen) = damageBodyWithMissles gen damage bdy
  damage = sum $ map (floor . unpack) (view msMissleStrengths missleSalvo)

damageBodyWithMissles :: RandomGen g => g -> Int -> Body -> (Body, g)
damageBodyWithMissles gen damage bdy = (newBody, newGen) where
  (randomSelection, gen'  ) = randomR (0 :: Int, 3 :: Int) gen
  (newBody        , newGen) = case randomSelection of
    0 -> destroyBodyShipyards gen' damage bdy
    1 -> destroyBodyFuelReserves gen' damage bdy
    2 -> destroyBodyInstallments gen' damage bdy
    3 -> destroyBodyMinerals gen' damage bdy
    _ -> (bdy, gen')

moveMissle :: Seconds -> Double -> Double -> MissleSalvo -> (Bool, MissleSalvo)
moveMissle secs moveX moveY missleSalvo = newMissleSalvo where
  newMissleSalvo = if overshot
    then
      ( True
      , missleSalvo { _msLocation = MissleSalvoLocation moveX moveY
                    , _msRange    = MissleRange (range - distanceToTravel)
                    }
      )
    else
      ( False
      , missleSalvo { _msLocation = MissleSalvoLocation newX newY
                    , _msRange = MissleRange (range - missleSalvoTravelDistance)
                    }
      )
  newX = locationX + cos angle * missleSalvoTravelDistance
  newY = locationY + sin angle * missleSalvoTravelDistance
  (MissleSpeed speed                      ) = view msSpeed missleSalvo
  (MissleRange range                      ) = view msRange missleSalvo
  (MissleSalvoLocation locationX locationY) = view msLocation missleSalvo
  missleSalvoMaxTravelDistance = speed * fromIntegral secs
  missleSalvoTravelDistance    = minimum [range, missleSalvoMaxTravelDistance]
  distanceToTravel =
    sqrt $ ((moveX - locationX) ** 2) + ((moveY - locationY) ** 2)
  overshot = missleSalvoTravelDistance >= distanceToTravel
  angle    = atan2 (moveY - locationY) (moveX - locationX) :: Double

missleShipMiss :: g -> Ship -> MissleSalvo -> (Maybe MissleSalvo, g)
missleShipMiss gen _ missleSalvo = (maybeMissleSalvo, gen)
  where
  -- TODO: Should take into account missle speed, target speed, and target size.
  -- https://docs.google.com/spreadsheets/d/1mQ3PJGxqgK4fvWSdjWkSPt3QebO2jLlcngxNFTiIdys/edit#gid=917907050
  -- TODO: For now they all hit, in the future, I have to roll for each missle in the salvo and adjust salvo accordingly.
        maybeMissleSalvo = Just missleSalvo

missleShipShieldDamage
  :: g -> Ship -> MissleSalvo -> (Ship, Maybe MissleSalvo, g)
missleShipShieldDamage gen ship salvo = shieldCheck where
  missleStrengths  = view msMissleStrengths salvo
  maybeShipShields = view sShields ship
  shieldsEnabled   = view ssEnabled shipShields
  shieldCheck      = if isJust maybeShipShields && shieldsEnabled
    then (newShip, maybeSalvo, gen)
    else (ship, Just salvo, gen)
  shipShields                      = fromJust maybeShipShields
  (ComponentRating shieldStrength) = view ssCurrentCapacity shipShields
  (newShieldStrength, maybeNewMissleStrengths) =
    calculateMissleShieldDamage shieldStrength missleStrengths
  newShip = ship
    { _sShields = Just $ shipShields
                    { _ssCurrentCapacity = ComponentRating newShieldStrength
                    }
    }
  maybeSalvo = if isNothing maybeNewMissleStrengths
    then Nothing
    else Just $ salvo { _msMissleStrengths = fromJust maybeNewMissleStrengths }

calculateMissleShieldDamage
  :: Double -> [MissleStrength] -> (Double, Maybe [MissleStrength])
calculateMissleShieldDamage shieldStrength [] = (shieldStrength, Nothing)
calculateMissleShieldDamage shieldStrength ((MissleStrength ms) : mss) =
  damageCalculated where
  damageCalculated = if newShieldStrength < 1
    then (0, Just $ (MissleStrength (ms - shieldStrength) : mss))
    else calculateMissleShieldDamage newShieldStrength mss
  newShieldStrength = shieldStrength - ms

missleShipArmorDamage
  :: RandomGen g => g -> Ship -> MissleSalvo -> (Ship, Maybe MissleSalvo, g)
missleShipArmorDamage gen ship salvo = (newShip, maybeSalvo, newGen) where
  missleStrengths                 = view msMissleStrengths salvo
  (ShipArmor shipArmor armorGrid) = view sArmor ship
  (gen'        , newGen        )  = split gen
  (newArmorGrid, misslesThrough)  = foldl
    updateArmorGridMissleDamage
    (armorGrid, [])
    (zip missleStrengths (randomRs (0, length (armorGrid !! 0) - 1) gen'))
  newShip    = ship { _sArmor = ShipArmor shipArmor newArmorGrid }
  maybeSalvo = if length misslesThrough > 0
    then Just $ salvo { _msMissleStrengths = misslesThrough }
    else Nothing

missleShipDamage :: RandomGen g => g -> Ship -> MissleSalvo -> (Maybe Ship, g)
missleShipDamage gen shp salvo = (maybeShip, newGen)
 where
  (maybeShip, newGen) = handleShipDamage gen shp salvoDamage
  salvoDamage         = view msMissleStrengths salvo

handleShipDamage
  :: RandomGen g => g -> Ship -> [MissleStrength] -> (Maybe Ship, g)
handleShipDamage gen shp []                          = (Just shp, gen)
handleShipDamage gen shp ((MissleStrength ms) : mss) = (maybeShp, newGen) where
  (ShipSize shipSize)     = view (sDesign . sdSize) shp
  (destroyChance, gen'  ) = randomR (0, shipSize) gen
  (newShp       , gen'' ) = destroyShipComponent gen' ms shp
  (maybeShp     , newGen) = if destroyChance < ms
    then (Nothing, gen'')
    else handleShipDamage gen'' newShp mss

-- TODO: Should maybe also destroy ship cargo?
destroyShipComponent :: RandomGen g => g -> Double -> Ship -> (Ship, g)
destroyShipComponent gen damage shp = (newShp, newGen) where
  (randomComponent, gen'  ) = randomR (0 :: Int, 2 :: Int) gen
  (newShp         , newGen) = case randomComponent of
    0 -> destroyShipFuel gen' damage shp
    1 -> destroyShipMissleLauncher gen' damage shp
    2 -> destroyShipShields gen' damage shp
    _ -> (shp, gen')

destroyShipFuel :: g -> Double -> Ship -> (Ship, g)
destroyShipFuel gen _ shp = (newShp, gen) where
  fuelStorages           = view (sDesign . sdFuelStorages) shp
  numFuelStorages        = length fuelStorages
  maxFuel = (sum $ map (unpack . view fsRating) fuelStorages) :: Double
  (ShipFuel currentFuel) = view sFuel shp
  fuelDamage             = maxFuel / fromIntegral numFuelStorages
  newFuel                = maximum [0, currentFuel - fuelDamage]
  newShp                 = set sFuel (ShipFuel newFuel) shp

destroyShipMissleLauncher :: g -> Double -> Ship -> (Ship, g)
destroyShipMissleLauncher gen _ shp = (newShp, gen) where
  maybeShipMissleLaunchers = view sShipMissleLaunchers shp
  newShp =
    if isNothing maybeShipMissleLaunchers then shp else damagedMissleLaunchers
  damagedMissleLaunchers = if numMissleLaunchers > 1
    then shp { _sShipMissleLaunchers = Just newShipMissleLaunchers }
    else shp { _sShipMissleLaunchers = Nothing }
  currentShipMissleLaunchers = fromJust maybeShipMissleLaunchers
  currentSalvo               = view smlSalvo currentShipMissleLaunchers
  currentMissleStrengths     = view msMissleStrengths currentSalvo
  numMissleLaunchers         = length currentMissleStrengths
  newMissleStrengths         = tail currentMissleStrengths
  newSalvo = currentSalvo { _msMissleStrengths = newMissleStrengths }
  newShipMissleLaunchers = currentShipMissleLaunchers { _smlSalvo = newSalvo }

destroyShipShields :: g -> Double -> Ship -> (Ship, g)
destroyShipShields gen damage shp = (newShp, gen) where
  maybeShipShields   = view sShields shp
  newShp = if isNothing maybeShipShields then shp else damagedShipShields
  damagedShipShields = shp { _sShields = damagedShields }
  damagedShields     = if maxCapacity < damage
    then Nothing
    else Just
      $ currentShields { _ssMaxCapacity = ComponentRating newMaxCapacity }
  currentShields                = fromJust maybeShipShields
  (ComponentRating maxCapacity) = view ssMaxCapacity currentShields
  newMaxCapacity                = maxCapacity - damage

destroyBodyShipyards :: RandomGen g => g -> Int -> Body -> (Body, g)
destroyBodyShipyards gen damage bdy = (newBody, newGen) where
  (randomChance, gen'  ) = randomR (0, 10000) gen
  (newBody     , newGen) = if randomChance < damage && numShipyards > 0
    then (bdy { _bShipyards = newShipyards }, gen'')
    else (bdy, gen')
  numShipyards   = Map.size bodyShipyards
  (syIdx, gen'') = randomR (0, numShipyards - 1) gen'
  bodyShipyards  = view bShipyards bdy
  syKey          = (Map.keys bodyShipyards) !! syIdx
  newShipyards =
    if numShipyards > 0 then Map.delete syKey bodyShipyards else bodyShipyards

destroyBodyFuelReserves :: g -> Int -> Body -> (Body, g)
destroyBodyFuelReserves gen damage bdy = (newBody, gen) where
  newBody                = bdy { _bFuelReserves = newFuelReserves }
  newFuelReserves        = FuelReserves newFuel
  newFuel                = maximum [0, oldFuel - fromIntegral fuelDamage]
  (FuelReserves oldFuel) = view bFuelReserves bdy
  fuelDamage             = 1000 * damage

destroyBodyInstallments :: RandomGen g => g -> Int -> Body -> (Body, g)
destroyBodyInstallments gen _ bdy = (newBody, newGen) where
  currentInstallments         = view bInstallments bdy
  numInstallmentTypes         = Map.size currentInstallments
  (instIdx, newGen)           = randomR (0, numInstallmentTypes - 1) gen
  instKey                     = (Map.keys currentInstallments) !! instIdx
  installmentAffected = fromJust $ Map.lookup instKey currentInstallments
  newInstallments = Map.insert instKey newInstallment currentInstallments
  newInstallment = installmentAffected { _isCount = InstallmentCount newCount }
  (InstallmentCount oldCount) = view isCount installmentAffected
  newCount                    = maximum [0, oldCount - 1]
  newBody                     = if numInstallmentTypes > 0
    then bdy { _bInstallments = newInstallments }
    else bdy { _bInstallments = noInstallments }

destroyBodyMinerals :: RandomGen g => g -> Int -> Body -> (Body, g)
destroyBodyMinerals gen damage bdy = (newBody, newGen) where
  currentMinerals  = view bMinerals bdy
  numMineralTypes  = Map.size currentMinerals
  (minIdx, newGen) = randomR (0, numMineralTypes - 1) gen
  minKey           = (Map.keys currentMinerals) !! minIdx
  minAffected      = fromJust $ Map.lookup minKey currentMinerals
  newMinerals      = Map.insert minKey newMineral currentMinerals
  newMineral = minAffected { _mCount     = MineralCount newMineralCount
                           , _mStockpile = MineralCount newMineralStockpile
                           }
  (MineralCount oldMineralCount) = view mCount minAffected
  (MineralCount oldMineralStockpile) = view mStockpile minAffected
  newMineralCount = maximum [0, oldMineralCount - fromIntegral (damage * 100)]
  newMineralStockpile = maximum [0, oldMineralStockpile - fromIntegral damage]
  newBody = if numMineralTypes > 1
    then bdy { _bMinerals = newMinerals }
    else bdy { _bMinerals = Map.empty }
