{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Nebula4x.Mineral where

import           Control.Lens
import           Control.Newtype.Generics
import qualified Data.Map.Strict          as Map
import           Data.Maybe
import           System.Random

import           Nebula4x.Types

noMinerals :: Minerals
noMinerals = Map.empty

generateBodyMinerals :: RandomGen g => g -> BodyConfig -> (Minerals, g)
generateBodyMinerals g bConf = (bodyMinerals, g'')
  where
    generationChance = view bcMineralGenerationChance bConf
    mineralAmounts = view bcMineralAmounts bConf
    accessModifier = view bcAccessibilityModifier bConf
    possibleElements = Map.keys mineralAmounts
    (bodyElements, g') = rollElements g generationChance possibleElements
    (bodyMinerals, g'') =
      rollMinerals g' mineralAmounts accessModifier bodyElements Map.empty

rollElements ::
     RandomGen g => g -> GenerationChance -> Elements -> (Elements, g)
rollElements g genChance elems = rollElements' g genChance elems []

rollElements' ::
     RandomGen g
  => g
  -> GenerationChance
  -> Elements
  -> Elements
  -> (Elements, g)
rollElements' g _ [] generated = (generated, g)
rollElements' g (GenerationChance genC) [elemnt] generated =
  (newElements, newGen)
  where
    (genNum, g') = randomR (0.0, 1.0) g
    (newElements, newGen) =
      if genC >= genNum
        then (generated ++ [elemnt], g')
        else (generated, g')
rollElements' g gc@(GenerationChance genC) (elemnt:elemnts) generated =
  (newElements, newGen)
  where
    (genNum, g') = randomR (0.0, 1.0) g
    (newElements, newGen) =
      if genC >= genNum
        then rollElements' g' gc elemnts (generated ++ [elemnt])
        else rollElements' g' gc elemnts generated

rollMinerals ::
     RandomGen g
  => g
  -> MineralAmounts
  -> Range Double
  -> Elements
  -> Minerals
  -> (Minerals, g)
rollMinerals g _ _ [] generated = (generated, g)
rollMinerals g minAmounts accessModifier [elemnt] generated =
  (newMinerals, newGen)
  where
    mineralCounts = Map.lookup elemnt minAmounts
    (newMinerals, newGen) =
      if isNothing mineralCounts
        then (generated, g)
        else (Map.insert elemnt mineral generated, g')
    (mineral, g') =
      generateMinerals g elemnt accessModifier (fromJust mineralCounts)
rollMinerals g minAmounts accessModifier (elemnt:elemnts) generated =
  (newMinerals, newGen)
  where
    mineralCounts = Map.lookup elemnt minAmounts
    (newMinerals, newGen) =
      if isNothing mineralCounts
        then (generated, g)
        else rollMinerals
               g'
               minAmounts
               accessModifier
               elemnts
               (Map.insert elemnt mineral generated)
    (mineral, g') =
      generateMinerals g elemnt accessModifier (fromJust mineralCounts)

generateMinerals ::
     RandomGen g => g -> Element -> Range Double -> Range Double -> (Mineral, g)
generateMinerals g elemnt (Range minAcc maxAcc) (Range minMins maxMins) =
  mineral
  where
    mineral =
      ( Mineral
          elemnt
          (Accessibility accessibility)
          (MineralCount amount)
          (MineralCount 0)
      , g''')
    (baseAccess, g') = randomR (0.1, 1.0) g
    (accessModifier, g'') = randomR (minAcc, maxAcc) g'
    access = baseAccess * accessModifier
    accessibility =
      if access > 1.0
        then 1.0
        else if access < 0.1
               then 0.1
               else access
    (amount, g''') = randomR (minMins, maxMins) g''

toMineralCost :: Minerals -> MineralCost
toMineralCost = Map.map (unpack . view mStockpile)

updateMinerals :: MineralCost -> Minerals -> Minerals
updateMinerals funds minerals = newMinerals
  where
    newMinerals = Map.map (updateMineral funds) minerals

updateMineral :: MineralCost -> Mineral -> Mineral
updateMineral funds m@(Mineral elemnt _ _ _) = newMineral
  where
    maybeMineral = Map.lookup elemnt funds :: Maybe Double
    newMineral =
      case maybeMineral of
        Just amount -> set mStockpile (MineralCount amount) m
        Nothing     -> m

canAfford :: MineralCost -> AvailableMinerals -> Bool
canAfford = Map.isSubmapOfBy (<=)

expendMinerals :: MineralCost -> AvailableMinerals -> MineralCost
expendMinerals cost funds = Map.unionWith (-) funds cost

mineralStackCargoSize :: MineralStack -> Double
mineralStackCargoSize (MineralStack _ (MineralCount count)) = count

mineralStacksCargoSize :: MineralStacks -> Double
mineralStacksCargoSize = Map.foldr (\ms acc -> mineralStackCargoSize ms + acc) 0
