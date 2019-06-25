{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Nebula4x.Installment where

import           Control.Lens
import           Control.Newtype.Generics
                                         hiding ( over )
import qualified Data.Map.Strict               as Map

import           Nebula4x.Mineral
import           Nebula4x.Shipyard
import           Nebula4x.Time
import           Nebula4x.Types
import           Nebula4x.Utils

installmentCargoSize :: Installment -> InstallmentSize
installmentCargoSize = view iSize

installmentsCargoSize :: Installments -> InstallmentSize
installmentsCargoSize insts = installmentsSize
 where
  installmentsSize = InstallmentSize totalSize
  totalSize        = Map.foldr tallyInstallmentSize 0 insts
  tallyInstallmentSize instStack count =
    unpack (installmentStackCargoSize instStack) + count

installmentStackCargoSize :: InstallmentStack -> InstallmentSize
installmentStackCargoSize (InstallmentStack (InstallmentCount count) inst) =
  InstallmentSize (count * (unpack $ view iSize inst))

installmentStackRating :: InstallmentStack -> InstallmentRating
installmentStackRating (InstallmentStack (InstallmentCount cnt) inst) =
  InstallmentRating (cnt * (unpack $ view iRating inst))

installmentCost :: Installment -> InstallmentCost
installmentCost = view iCost

installmentId :: Installment -> InstallmentId
installmentId = view iId

--
-- Installment - Mines
--
-- Cost ratio of building mines.
mineMineralCostRatio :: MineralCost
mineMineralCostRatio = Map.fromList [(Duranium, 0.5), (Corundium, 0.5)]

miningRate :: Installment -> InstallmentRating
miningRate (Mine _ _ _ _ (InstallmentRating r)) = InstallmentRating r
miningRate _ = InstallmentRating 0

stackMiningRate :: InstallmentStack -> InstallmentRating
stackMiningRate (InstallmentStack (InstallmentCount cnt) (Mine _ _ _ _ (InstallmentRating r)))
  = InstallmentRating (cnt * r)
stackMiningRate _ = InstallmentRating 0

totalMiningRate :: Installments -> InstallmentRating
totalMiningRate insts = miningRate'
 where
  miningRate' = InstallmentRating totalRate
  totalRate   = Map.foldr tallyInstallmentRating 0 insts
  tallyInstallmentRating instStack count =
    unpack (stackMiningRate instStack) + count

-- Helper function to mine an element given available installments.
mineMineral :: Seconds -> Installments -> Mineral -> Mineral
mineMineral secs insts min'@(Mineral _ (Accessibility access) (MineralCount count) (MineralCount stockpile))
  = newMineral
 where
  newMineral = if count > 0
    then min' { _mCount = newCount', _mStockpile = newStockpile }
    else min'
  InstallmentRating miningRate' = totalMiningRate insts
  mineralsPerSecond = miningRate' / (fromIntegral yearInSeconds) * access
  mineralsMined = minimum [count, mineralsPerSecond * (fromIntegral secs)]
  newStockpile = MineralCount (stockpile + mineralsMined)
  newCount = count - mineralsMined
  newCount' = if newCount > 0 then MineralCount newCount else MineralCount 0

mineMinerals :: Seconds -> Installments -> Minerals -> Minerals
mineMinerals secs insts = Map.map (mineMineral secs insts)

--
-- Installment - Research Labs
--
-- Cost ratio of building research labs.
researchLabMineralCostRatio :: MineralCost
researchLabMineralCostRatio = Map.fromList [(Duranium, 0.5), (Mercassium, 0.5)]

researchRate :: Installment -> InstallmentRating
researchRate (ResearchLab _ _ _ _ (InstallmentRating r)) = InstallmentRating r
researchRate _ = InstallmentRating 0

stackResearchRate :: InstallmentStack -> InstallmentRating
stackResearchRate (InstallmentStack (InstallmentCount cnt) (ResearchLab _ _ _ _ (InstallmentRating r)))
  = InstallmentRating (cnt * r)
stackResearchRate _ = InstallmentRating 0

researchLabCount :: ResearchStatus a -> Int
researchLabCount researchStatus =
  Map.foldr (+) 0 (view researchLabs researchStatus)

--
-- Installment - Fuel Refineries
--
-- Cost ratio of building fuel refineries.
fuelRefineryMineralCostRatio :: MineralCost
fuelRefineryMineralCostRatio =
  Map.fromList [(Duranium, 0.25), (Boronide, 0.75)]

refiningRate :: Installment -> InstallmentRating
refiningRate (FuelRefinery _ _ _ _ (InstallmentRating r)) = InstallmentRating r
refiningRate _ = InstallmentRating 0

stackRefiningRate :: InstallmentStack -> InstallmentRating
stackRefiningRate (InstallmentStack (InstallmentCount cnt) (FuelRefinery _ _ _ _ (InstallmentRating r)))
  = InstallmentRating (cnt * r)
stackRefiningRate _ = InstallmentRating 0

totalRefiningRate :: Installments -> InstallmentRating
totalRefiningRate insts = totalRate
 where
  totalRate = InstallmentRating tRate
  tRate     = Map.foldr tallyInstallmentRating 0 insts
  tallyInstallmentRating instStack count =
    unpack (stackRefiningRate instStack) + count

-- Helper function to refine fuel on body
-- Default to 1000L per Sorium for now.
refineFuel :: Seconds -> Body -> Body
refineFuel _ bdy@(Body _ _ _ _ _ _ _ _ _ _ (ActiveRefining False) _ _ _ _ _) =
  bdy
refineFuel secs bdy@(Body _ _ _ _ _ _ _ _ _ (FuelReserves reserves) _ installments minerals _ _ _)
  = newBody
 where
  newBody = bdy { _bFuelReserves = newFuelReserves, _bMinerals = newMinerals }
  newFuelReserves = FuelReserves (reserves + finalFuelCreated)
  newMinerals     = Map.insert Sorium newSoriumMineral minerals
  fuelPerSorium   = 1000
  (InstallmentRating yearlyRefiningRate) = totalRefiningRate installments
  refRate         = yearlyRefiningRate / fromIntegral yearInSeconds
  soriumMineral   = Map.findWithDefault
    (Mineral Sorium (Accessibility 0) (MineralCount 0) (MineralCount 0))
    Sorium
    minerals
  availableSorium     = unpack $ view mStockpile soriumMineral
  toBeRefined         = refRate * fromIntegral secs
  finalSoriumConsumed = minimum [availableSorium, toBeRefined]
  finalFuelCreated    = finalSoriumConsumed * fuelPerSorium
  newSoriumMineral    = set
    mStockpile
    (MineralCount $ availableSorium - finalSoriumConsumed)
    soriumMineral

--
-- Installments - Construction Factories
--
-- Cost ratio of building construction factories.
constructionFactoryMineralCostRatio :: MineralCost
constructionFactoryMineralCostRatio =
  Map.fromList [(Duranium, 0.5), (Tritanium, 0.25), (Vendarite, 0.25)]

constructionRate :: Installment -> InstallmentRating
constructionRate (ConstructionFactory _ _ _ _ (InstallmentRating r)) =
  InstallmentRating r
constructionRate _ = InstallmentRating 0

stackConstructionRate :: InstallmentStack -> InstallmentRating
stackConstructionRate (InstallmentStack (InstallmentCount cnt) (ConstructionFactory _ _ _ _ (InstallmentRating r)))
  = InstallmentRating (cnt * r)
stackConstructionRate _ = InstallmentRating 0

totalConstructionRate :: Installments -> InstallmentRating
totalConstructionRate insts = totalRate
 where
  totalRate = InstallmentRating tRate
  tRate     = Map.foldr tallyInstallmentRating 0 insts
  tallyInstallmentRating instStack count =
    unpack (stackConstructionRate instStack) + count

--
-- Installments - Mass Drivers
--
-- Cost ratio of building mass drivers.
massDriverMineralCostRatio :: MineralCost
massDriverMineralCostRatio =
  Map.fromList [(Duranium, 0.35), (Neutronium, 0.35), (Boronide, 0.3)]

massDriverRate :: Installment -> InstallmentRating
massDriverRate (MassDriver _ _ _ _ (InstallmentRating r)) = InstallmentRating r
massDriverRate _ = InstallmentRating 0

stackMassDriverRate :: InstallmentStack -> InstallmentRating
stackMassDriverRate (InstallmentStack (InstallmentCount cnt) (MassDriver _ _ _ _ (InstallmentRating r)))
  = InstallmentRating (cnt * r)
stackMassDriverRate _ = InstallmentRating 0

totalMassDriverRate :: Installments -> InstallmentRating
totalMassDriverRate insts = totalRate
 where
  totalRate = InstallmentRating tRate
  tRate     = Map.foldr tallyInstallmentRating 0 insts
  tallyInstallmentRating instStack count =
    unpack (stackMassDriverRate instStack) + count

--
-- Installments - Shipyards
--
-- Cost ratio of building commercial shipyards.
commercialShipyardMineralCostRatio :: MineralCost
commercialShipyardMineralCostRatio =
  Map.fromList [(Duranium, 0.5), (Neutronium, 0.5)]

-- Cost ratio of building naval shipyards.
navalShipyardMineralCostRatio :: MineralCost
navalShipyardMineralCostRatio =
  Map.fromList [(Duranium, 0.5), (Neutronium, 0.5)]

processFactories
  :: Seconds -> InstallmentProduction -> Body -> (InstallmentProduction, Body)
processFactories seconds instProd body = (newInstProd, newBody)
 where
  currentAllocation = (unpack $ view ipAllocation instProd) / 100
  funds = toMineralCost (view bMinerals body)
  totalCost = Map.map (* productionGenerated) (getMineralCostRatio instProd)
  (InstallmentRating yearlyConstructionRate) =
    totalConstructionRate (view bInstallments body)
  productionRate =
    yearlyConstructionRate / (fromIntegral yearInSeconds) * currentAllocation
  secs                = fromIntegral seconds :: Double
  productionGenerated = productionRate * secs
  (newInstProd, newBody) =
    if currentAllocation > 0.0 && canAfford totalCost funds
      then (nInstProd, nBody)
      else (instProd, body)
  inst                = view ipInstallment instProd
  instId              = installmentId inst
  instCost            = unpack $ installmentCost inst
  instGenerated       = (productionGenerated / instCost)
  existingProgress    = (unpack $ view ipProgress instProd)
  newNum = fromIntegral $ (floor (instGenerated + existingProgress) :: Int)
  newExistingProgress = (instGenerated + existingProgress) - newNum
  nInstProd           = set ipProgress (pack $ newExistingProgress) instProd
  newFunds            = expendMinerals totalCost funds
  nBody               = over bMinerals (updateMinerals newFunds) nBody'
  nBody'              = if newNum < 1 then body else nBody''
  nBody''             = case inst of
    (CommercialShipyardComplex _ _ _ _ _) -> addBodyCommercialShipyard body
    (NavalShipyardComplex _ _ _ _ _) -> addBodyNavalShipyard body
    _ -> if Map.member instId (view bInstallments body)
      then over (bInstallments . at' instId . isCount)
                (pack . (+ newNum) . unpack)
                body
      else set (bInstallments . at'' instId) (Just newInstallment) body -- Special logic for shipyard complex
  newInstallment = InstallmentStack (InstallmentCount newNum) inst

getMineralCostRatio :: InstallmentProduction -> MineralCost
getMineralCostRatio (InstallmentProduction _ _ (Mine _ _ _ _ _)) =
  mineMineralCostRatio
getMineralCostRatio (InstallmentProduction _ _ (ResearchLab _ _ _ _ _)) =
  researchLabMineralCostRatio
getMineralCostRatio (InstallmentProduction _ _ (FuelRefinery _ _ _ _ _)) =
  fuelRefineryMineralCostRatio
getMineralCostRatio (InstallmentProduction _ _ (ConstructionFactory _ _ _ _ _))
  = constructionFactoryMineralCostRatio
getMineralCostRatio (InstallmentProduction _ _ (MassDriver _ _ _ _ _)) =
  massDriverMineralCostRatio
getMineralCostRatio (InstallmentProduction _ _ (CommercialShipyardComplex _ _ _ _ _))
  = commercialShipyardMineralCostRatio
getMineralCostRatio (InstallmentProduction _ _ (NavalShipyardComplex _ _ _ _ _))
  = navalShipyardMineralCostRatio

--
-- All installments
--
basicMine :: Installment
basicMine = Mine 1
                 (InstallmentName "Basic Mine")
                 (InstallmentSize 10000)
                 (InstallmentCost 120)
                 (InstallmentRating 10)

basicMineResearch :: InstallmentResearch
basicMineResearch =
  InstallmentResearch 1 (ComponentResearch 0) (ComponentResearch 0) basicMine

improvedMine :: Installment
improvedMine = Mine 2
                    (InstallmentName "Improved Mine")
                    (InstallmentSize 20000)
                    (InstallmentCost 250)
                    (InstallmentRating 25)

improvedMineResearch :: InstallmentResearch
improvedMineResearch = InstallmentResearch 2
                                           (ComponentResearch 1000)
                                           (ComponentResearch 0)
                                           improvedMine

advancedMine :: Installment
advancedMine = Mine 3
                    (InstallmentName "Advanced Mine")
                    (InstallmentSize 40000)
                    (InstallmentCost 500)
                    (InstallmentRating 60)

advancedMineResearch :: InstallmentResearch
advancedMineResearch = InstallmentResearch 3
                                           (ComponentResearch 10000)
                                           (ComponentResearch 0)
                                           advancedMine

basicResearchLab :: Installment
basicResearchLab = ResearchLab 4
                               (InstallmentName "Research Lab")
                               (InstallmentSize 200000)
                               (InstallmentCost 1000)
                               (InstallmentRating 100) -- RP per year

basicResearchLabResearch :: InstallmentResearch
basicResearchLabResearch = InstallmentResearch 4
                                               (ComponentResearch 0)
                                               (ComponentResearch 0)
                                               basicResearchLab

-- Not using any other research lab type as it will require a refactor of the researchLabs field in the research data structure.
improvedResearchLab :: Installment
improvedResearchLab = ResearchLab 5
                                  (InstallmentName "Improved Research Lab")
                                  (InstallmentSize 500000)
                                  (InstallmentCost 3000)
                                  (InstallmentRating 400) -- RP per year

improvedResearchLabResearch :: InstallmentResearch
improvedResearchLabResearch = InstallmentResearch 5
                                                  (ComponentResearch 100000)
                                                  (ComponentResearch 0)
                                                  improvedResearchLab

advancedResearchLab :: Installment
advancedResearchLab = ResearchLab 6
                                  (InstallmentName "Advanced Research Lab")
                                  (InstallmentSize 1000000)
                                  (InstallmentCost 10000)
                                  (InstallmentRating 2000) -- RP per year

advancedResearchLabResearch :: InstallmentResearch
advancedResearchLabResearch = InstallmentResearch 6
                                                  (ComponentResearch 1000000)
                                                  (ComponentResearch 0)
                                                  advancedResearchLab

basicFuelRefinery :: Installment
basicFuelRefinery = FuelRefinery 7
                                 (InstallmentName "Basic Fuel Refinery")
                                 (InstallmentSize 10000)
                                 (InstallmentCost 100)
                                 (InstallmentRating 10) -- Consumed Sorium per year (at 1000L per Sorium)

basicFuelRefineryResearch :: InstallmentResearch
basicFuelRefineryResearch = InstallmentResearch 7
                                                (ComponentResearch 0)
                                                (ComponentResearch 0)
                                                basicFuelRefinery

improvedFuelRefinery :: Installment
improvedFuelRefinery = FuelRefinery
  8
  (InstallmentName "Improved Fuel Refinery")
  (InstallmentSize 20000)
  (InstallmentCost 250)
  (InstallmentRating 30)

improvedFuelRefineryResearch :: InstallmentResearch
improvedFuelRefineryResearch = InstallmentResearch 8
                                                   (ComponentResearch 1000)
                                                   (ComponentResearch 0)
                                                   improvedFuelRefinery

advancedFuelRefinery :: Installment
advancedFuelRefinery = FuelRefinery
  9
  (InstallmentName "Advanced Fuel Refinery")
  (InstallmentSize 50000)
  (InstallmentCost 500)
  (InstallmentRating 100)

advancedFuelRefineryResearch :: InstallmentResearch
advancedFuelRefineryResearch = InstallmentResearch 9
                                                   (ComponentResearch 10000)
                                                   (ComponentResearch 0)
                                                   advancedFuelRefinery

basicConstructionFactory :: Installment
basicConstructionFactory = ConstructionFactory
  10
  (InstallmentName "Basic Construction Factory")
  (InstallmentSize 10000)
  (InstallmentCost 100)
  (InstallmentRating 5) -- BP per year

basicConstructionFactoryResearch :: InstallmentResearch
basicConstructionFactoryResearch = InstallmentResearch
  10
  (ComponentResearch 0)
  (ComponentResearch 0)
  basicConstructionFactory

improvedConstructionFactory :: Installment
improvedConstructionFactory = ConstructionFactory
  11
  (InstallmentName "Improved Construction Factory")
  (InstallmentSize 20000)
  (InstallmentCost 250)
  (InstallmentRating 20)

improvedConstructionFactoryResearch :: InstallmentResearch
improvedConstructionFactoryResearch = InstallmentResearch
  11
  (ComponentResearch 5000)
  (ComponentResearch 0)
  improvedConstructionFactory

advancedConstructionFactory :: Installment
advancedConstructionFactory = ConstructionFactory
  12
  (InstallmentName "Advanced Construction Factory")
  (InstallmentSize 50000)
  (InstallmentCost 1000)
  (InstallmentRating 100)

advancedConstructionFactoryResearch :: InstallmentResearch
advancedConstructionFactoryResearch = InstallmentResearch
  12
  (ComponentResearch 20000)
  (ComponentResearch 0)
  advancedConstructionFactory

basicMassDriver :: Installment
basicMassDriver = MassDriver 13
                             (InstallmentName "Basic Mass Driver")
                             (InstallmentSize 5000)
                             (InstallmentCost 100)
                             (InstallmentRating 2000)

basicMassDriverResearch :: InstallmentResearch
basicMassDriverResearch = InstallmentResearch 13
                                              (ComponentResearch 0)
                                              (ComponentResearch 0)
                                              basicMassDriver

improvedMassDriver :: Installment
improvedMassDriver = ConstructionFactory
  14
  (InstallmentName "Improved Mass Driver")
  (InstallmentSize 20000)
  (InstallmentCost 400)
  (InstallmentRating 10000)

improvedMassDriverResearch :: InstallmentResearch
improvedMassDriverResearch = InstallmentResearch 14
                                                 (ComponentResearch 5000)
                                                 (ComponentResearch 0)
                                                 improvedMassDriver

advancedMassDriver :: Installment
advancedMassDriver = ConstructionFactory
  15
  (InstallmentName "Advanced Mass Driver")
  (InstallmentSize 80000)
  (InstallmentCost 1600)
  (InstallmentRating 50000)

advancedMassDriverResearch :: InstallmentResearch
advancedMassDriverResearch = InstallmentResearch 15
                                                 (ComponentResearch 20000)
                                                 (ComponentResearch 0)
                                                 advancedMassDriver

commercialShipyard :: Installment
commercialShipyard = CommercialShipyardComplex
  16
  (InstallmentName "Commercial Shipyard Complex")
  (InstallmentSize 0)
  (InstallmentCost 2400)
  (InstallmentRating 0)

commercialShipyardResearch :: InstallmentResearch
commercialShipyardResearch = InstallmentResearch 16
                                                 (ComponentResearch 0)
                                                 (ComponentResearch 0)
                                                 commercialShipyard

navalShipyard :: Installment
navalShipyard = NavalShipyardComplex
  17
  (InstallmentName "Naval Shipyard Complex")
  (InstallmentSize 0)
  (InstallmentCost 2400)
  (InstallmentRating 0)

navalShipyardResearch :: InstallmentResearch
navalShipyardResearch = InstallmentResearch 17
                                            (ComponentResearch 0)
                                            (ComponentResearch 0)
                                            navalShipyard

availableInstallments :: Map.Map Int Installment
availableInstallments = Map.fromList
  [ (1 , basicMine)
  , (2 , improvedMine)
  , (3 , advancedMine)
  , (4 , basicResearchLab)
  , (5 , improvedResearchLab)
  , (6 , advancedResearchLab)
  , (7 , basicFuelRefinery)
  , (8 , improvedFuelRefinery)
  , (9 , advancedFuelRefinery)
  , (10, basicConstructionFactory)
  , (11, improvedConstructionFactory)
  , (12, advancedConstructionFactory)
  , (13, basicMassDriver)
  , (14, improvedMassDriver)
  , (15, advancedMassDriver)
  , (16, commercialShipyard)
  , (17, navalShipyard)
  ]
