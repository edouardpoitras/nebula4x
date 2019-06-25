module Nebula4x.Component.Armor where

import           Data.List
import qualified Data.Map.Strict               as Map

import           Nebula4x.Types

armorMineralCostRatio :: MineralCost
armorMineralCostRatio = Map.fromList [(Duranium, 1)]

conventionalArmor :: Armor
conventionalArmor = Armor (ComponentName "Conventional") (ComponentRating 2)

conventionalArmorResearch :: ArmorResearch
conventionalArmorResearch =
  ArmorResearch 1 (ComponentResearch 0) (ComponentResearch 0) conventionalArmor

duraniumArmor :: Armor
duraniumArmor = Armor (ComponentName "Duranium") (ComponentRating 5)

duraniumArmorResearch :: ArmorResearch
duraniumArmorResearch =
  ArmorResearch 2 (ComponentResearch 500) (ComponentResearch 0) duraniumArmor

highDensityDuraniumArmor :: Armor
highDensityDuraniumArmor =
  Armor (ComponentName "High Density Duranium") (ComponentRating 6)

highDensityDuraniumArmorResearch :: ArmorResearch
highDensityDuraniumArmorResearch = ArmorResearch 3
                                                 (ComponentResearch 2500)
                                                 (ComponentResearch 0)
                                                 highDensityDuraniumArmor

compositeArmor :: Armor
compositeArmor = Armor (ComponentName "Composite") (ComponentRating 8)

compositeArmorResearch :: ArmorResearch
compositeArmorResearch =
  ArmorResearch 4 (ComponentResearch 5000) (ComponentResearch 0) compositeArmor

ceramicCompositeArmor :: Armor
ceramicCompositeArmor =
  Armor (ComponentName "Ceramic Composite") (ComponentRating 10)

ceramicCompositeArmorResearch :: ArmorResearch
ceramicCompositeArmorResearch = ArmorResearch 5
                                              (ComponentResearch 10000)
                                              (ComponentResearch 0)
                                              ceramicCompositeArmor

laminateCompositeArmor :: Armor
laminateCompositeArmor =
  Armor (ComponentName "Laminate Composite") (ComponentRating 12)

laminateCompositeArmorResearch :: ArmorResearch
laminateCompositeArmorResearch = ArmorResearch 6
                                               (ComponentResearch 20000)
                                               (ComponentResearch 0)
                                               laminateCompositeArmor

compressedCarbonArmor :: Armor
compressedCarbonArmor =
  Armor (ComponentName "Compressed Carbon") (ComponentRating 15)

compressedCarbonArmorResearch :: ArmorResearch
compressedCarbonArmorResearch = ArmorResearch 7
                                              (ComponentResearch 40000)
                                              (ComponentResearch 0)
                                              compressedCarbonArmor

biphaseCarbideArmor :: Armor
biphaseCarbideArmor =
  Armor (ComponentName "Biphase Carbide") (ComponentRating 18)

biphaseCarbideArmorResearch :: ArmorResearch
biphaseCarbideArmorResearch = ArmorResearch 8
                                            (ComponentResearch 80000)
                                            (ComponentResearch 0)
                                            biphaseCarbideArmor

crystallineCompositeArmor :: Armor
crystallineCompositeArmor =
  Armor (ComponentName "Crystalline Composite") (ComponentRating 21)

crystallineCompositeArmorResearch :: ArmorResearch
crystallineCompositeArmorResearch = ArmorResearch 9
                                                  (ComponentResearch 150000)
                                                  (ComponentResearch 0)
                                                  crystallineCompositeArmor

superdenseArmor :: Armor
superdenseArmor = Armor (ComponentName "Superdense") (ComponentRating 25)

superdenseArmorResearch :: ArmorResearch
superdenseArmorResearch = ArmorResearch 10
                                        (ComponentResearch 300000)
                                        (ComponentResearch 0)
                                        superdenseArmor

bondedSuperdenseArmor :: Armor
bondedSuperdenseArmor =
  Armor (ComponentName "Bonded Superdense") (ComponentRating 30)

bondedSuperdenseArmorResearch :: ArmorResearch
bondedSuperdenseArmorResearch = ArmorResearch 11
                                              (ComponentResearch 600000)
                                              (ComponentResearch 0)
                                              bondedSuperdenseArmor

coherentSuperdenseArmor :: Armor
coherentSuperdenseArmor =
  Armor (ComponentName "Coherent Superdense") (ComponentRating 36)

coherentSuperdenseArmorResearch :: ArmorResearch
coherentSuperdenseArmorResearch = ArmorResearch 12
                                                (ComponentResearch 1250000)
                                                (ComponentResearch 0)
                                                coherentSuperdenseArmor

collapsiumArmor :: Armor
collapsiumArmor = Armor (ComponentName "Collapsium") (ComponentRating 45)

collapsiumArmorResearch :: ArmorResearch
collapsiumArmorResearch = ArmorResearch 13
                                        (ComponentResearch 2500000)
                                        (ComponentResearch 0)
                                        collapsiumArmor

updateArmorGridMissleDamage
  :: (ArmorGrid, [MissleStrength])
  -> (MissleStrength, Int)
  -> (ArmorGrid, [MissleStrength])
updateArmorGridMissleDamage (armorGrid, misslesThrough) (MissleStrength currentMissle, colIndex)
  = (newArmorGrid, newMisslesThrough) where
  armorDamageGrid = getArmorDamageGrid MissleDamage (floor currentMissle)
  (newArmorGrid, newMissleStrength) =
    performArmorDamage colIndex armorGrid armorDamageGrid
  newMisslesThrough = if newMissleStrength > 0
    then (MissleStrength newMissleStrength) : misslesThrough
    else misslesThrough

updateArmorGridLaserDamage :: ArmorGrid -> Int -> Int -> (ArmorGrid, Int)
updateArmorGridLaserDamage armorGrid colIndex damage =
  (newArmorGrid, floor newDamage) where
  armorDamageGrid = getArmorDamageGrid LaserDamage damage
  (newArmorGrid, newDamage) =
    performArmorDamage colIndex armorGrid armorDamageGrid

getArmorDamageGrid :: DamageType -> Int -> ArmorDamageGrid
getArmorDamageGrid damageType strength = newArmorDamageGrid where
  armorDamageGrid = case damageType of
    MissleDamage ->
      iterate (addMissleDamageToArmorDamageGrid 0) emptyGrid !! strength
    LaserDamage ->
      iterate (addLaserDamageToArmorDamageGrid 0) emptyGrid !! strength
  emptyGrid          = replicate numRows []
  numRows            = floor $ (sqrt (fromIntegral strength) :: Double)
  newArmorDamageGrid = case damageType of
    MissleDamage -> padArmorDamageGrid MissleDamage armorDamageGrid
    LaserDamage  -> padArmorDamageGrid LaserDamage (transpose armorDamageGrid)

addMissleDamageToArmorDamageGrid :: Int -> ArmorDamageGrid -> ArmorDamageGrid
addMissleDamageToArmorDamageGrid currentRow armorDamageGrid =
  newArmorDamageGrid where
  armorDamageGridNumRows    = length armorDamageGrid
  currentArmorDamageGridRow = armorDamageGrid !! currentRow
  currentRowLength          = length currentArmorDamageGridRow
  nextRow                   = currentRow + 1
  nextArmorDamageGridRow    = armorDamageGrid !! nextRow
  nextRowLength             = length nextArmorDamageGridRow
  newArmorDamageGrid =
    if nextRow == armorDamageGridNumRows || nextRowLength > currentRowLength + 2
      then addDamageToArmorDamageRowAt currentRow armorDamageGrid
      else addMissleDamageToArmorDamageGrid nextRow armorDamageGrid

addLaserDamageToArmorDamageGrid :: Int -> ArmorDamageGrid -> ArmorDamageGrid
addLaserDamageToArmorDamageGrid currentRow armorDamageGrid = newArmorDamageGrid where
  armorDamageGridNumRows    = length armorDamageGrid
  currentArmorDamageGridRow = armorDamageGrid !! currentRow
  currentRowLength          = length currentArmorDamageGridRow
  nextRow                   = currentRow + 1
  nextArmorDamageGridRow    = armorDamageGrid !! nextRow
  nextRowLength             = length nextArmorDamageGridRow
  newArmorDamageGrid =
    if nextRow == armorDamageGridNumRows || nextRowLength > currentRowLength + 1
      then addDamageToArmorDamageRowAt currentRow armorDamageGrid
      else addLaserDamageToArmorDamageGrid nextRow armorDamageGrid

addDamageToArmorDamageRowAt :: Int -> ArmorDamageGrid -> ArmorDamageGrid
addDamageToArmorDamageRowAt idx grid = newGrid where
  (gridTop, gridBottom) = splitAt idx grid
  oldGridRow            = head gridBottom
  newGrid               = gridTop ++ [newGridRow] ++ tail gridBottom
  newGridRow            = ArmorDamage : oldGridRow

padArmorDamageGrid :: DamageType -> ArmorDamageGrid -> ArmorDamageGrid
padArmorDamageGrid MissleDamage armorDamageGrid = newArmorDamageGrid where
  newArmorDamageGrid = zipWith
    (\currentRow currentIndex ->
      replicate currentIndex NoArmorDamage ++ currentRow
    )
    armorDamageGrid
    (reverse [0 .. length armorDamageGrid - 1])
padArmorDamageGrid LaserDamage armorDamageGrid = newArmorDamageGrid where
  newArmorDamageGrid = zipWith
    (\currentRow currentIndex ->
      replicate (div currentIndex 3) NoArmorDamage ++ currentRow
    )
    armorDamageGrid
    [0 .. length armorDamageGrid - 1]

performArmorDamage :: Int -> ArmorGrid -> ArmorDamageGrid -> (ArmorGrid, Double)
performArmorDamage column armorGrid armorDamageGrid =
  (newArmorGrid, newStrength)
 where
  (newArmorGrid, newStrength) =
    foldl (performArmorRowDamage column) (armorGrid, 0) armorDamageGrid

performArmorRowDamage
  :: Int -> (ArmorGrid, Double) -> ArmorDamageRow -> (ArmorGrid, Double)
performArmorRowDamage column (armorGrid, accumulatedDamage) rowDamage =
  (newArmorGrid, newAccumulatedDamage)
 where
  (newArmorGrid, newRowDamage) = foldl (performArmorRowDamage' column)
                                       (armorGrid, rowDamage)
                                       [0 .. (length armorGrid - 1)]
  newAccumulatedDamage = accumulatedDamage
    + fromIntegral (length $ filter ((==) ArmorDamage) newRowDamage)

performArmorRowDamage'
  :: Int -> (ArmorGrid, ArmorDamageRow) -> Int -> (ArmorGrid, ArmorDamageRow)
performArmorRowDamage' column (armorGrid, armorRowDamage) rowIndex =
  (newArmorGrid, newArmorRowDamage)
 where
  (newArmorGrid, newArmorRowDamage) = foldl (performArmorTileDamage rowIndex)
                                            (armorGrid, [])
                                            (zip armorRowDamage [column ..])

performArmorTileDamage
  :: Int
  -> (ArmorGrid, ArmorDamageRow)
  -> (ArmorDamageTile, Int)
  -> (ArmorGrid, ArmorDamageRow)
performArmorTileDamage _ (armorGrid, armorRowDamage) (NoArmorDamage, _) =
  (armorGrid, armorRowDamage ++ [NoArmorDamage])
performArmorTileDamage row (armorGrid, armorRowDamage) (ArmorDamage, col) =
  (newArmorGrid, newArmorRowDamage) where
  colLength = length (armorGrid !! row)
  adjustedCol = if col >= colLength then mod col colLength else col
  (newArmorGrid, newArmorRowDamage) = case (armorGrid !! row) !! adjustedCol of
    ArmorIntact ->
      ( damageArmorTile row adjustedCol armorGrid
      , armorRowDamage ++ [NoArmorDamage]
      )
    ArmorDamaged -> (armorGrid, armorRowDamage ++ [ArmorDamage])

damageArmorTile :: Int -> Int -> ArmorGrid -> ArmorGrid
damageArmorTile row col armorGrid = newArmorGrid where
  (gridTop, gridBottom) = splitAt row armorGrid
  oldGridRow            = head gridBottom
  (rowLeft, rowRight)   = splitAt col oldGridRow
  newGridRow            = rowLeft ++ [ArmorDamaged] ++ tail rowRight
  newArmorGrid          = gridTop ++ [newGridRow] ++ tail gridBottom
