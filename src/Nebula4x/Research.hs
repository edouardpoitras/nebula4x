module Nebula4x.Research where

import           Control.Lens
import           Control.Newtype.Generics
                                         hiding ( over )
import           Data.List                     as L
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           Data.Ord

import           Nebula4x.Component.Armor
import           Nebula4x.Component.Engine
import           Nebula4x.Component.FuelStorage
import           Nebula4x.Component.Laser
import           Nebula4x.Component.MissleLauncher
import           Nebula4x.Component.Sensor
import           Nebula4x.Component.Shield
import           Nebula4x.Component.Transport
import           Nebula4x.Installment
import           Nebula4x.Time
import           Nebula4x.Types
import           Nebula4x.Utils

startingRaceResearch :: RaceResearch
startingRaceResearch = Map.empty

startingResearch :: Research
startingResearch = Research startingShipDesigns
                            startingEngineDesigns
                            startingEngineTechnologyResearch
                            startingPowerEfficiencyResearch
                            startingFuelConsumptionResearch
                            startingEngineSizeResearch
                            startingMissleLauncherDesigns
                            startingMissleLauncherSizeResearch
                            startingMissleLauncherReloadRateResearch
                            startingMissleLauncherReducedSizeResearch
                            startingLaserDesigns
                            startingLaserFocalSizeResearch
                            startingLaserWavelengthResearch
                            startingLaserRechargeRateResearch
                            startingLaserReducedSizeResearch
                            startingArmorResearch
                            startingShieldResearch
                            startingFuelStorageResearch
                            startingCargoHandlingResearch
                            startingCargoHoldResearch
                            startingJumpGateResearch
                            startingGeologicalSensorResearch
                            startingGravitationalSensorResearch
                            startingMineResearch
                            startingResearchLabResearch
                            startingFuelRefineryResearch
                            startingConstructionFactoryResearch
                            startingMassDriverResearch
                            startingCommercialShipyardResearch
                            startingNavalShipyardResearch

startingShipDesigns :: ResearchStatus ShipDesign
startingShipDesigns = ResearchStatus Map.empty Nothing Map.empty Map.empty

startingEngineDesigns :: ResearchStatus EngineDesign
startingEngineDesigns = ResearchStatus Map.empty Nothing Map.empty Map.empty

startingEngineTechnologyResearch :: ResearchStatus EngineTechnologyResearch
startingEngineTechnologyResearch = ResearchStatus
  (Map.fromList
    [ ( view etrId conventionalEngineTechnologyResearch
      , conventionalEngineTechnologyResearch
      )
    ]
  )
  (Just nuclearThermalEngineTechnologyResearch)
  Map.empty
  (Map.fromList
    [ ( view etrId nuclearPulseEngineTechnologyResearch
      , nuclearPulseEngineTechnologyResearch
      )
    , (view etrId ionEngineTechnologyResearch, ionEngineTechnologyResearch)
    , ( view etrId magnetoPlasmaEngineTechnologyResearch
      , magnetoPlasmaEngineTechnologyResearch
      )
    , ( view etrId internalConfinementFusionEngineTechnologyResearch
      , internalConfinementFusionEngineTechnologyResearch
      )
    , ( view etrId magneticConfinementFusionEngineTechnologyResearch
      , magneticConfinementFusionEngineTechnologyResearch
      )
    , ( view etrId inertialConfinementFusionEngineTechnologyResearch
      , inertialConfinementFusionEngineTechnologyResearch
      )
    , ( view etrId solidCoreAntiMatterEngineTechnologyResearch
      , solidCoreAntiMatterEngineTechnologyResearch
      )
    , ( view etrId gasCoreAntiMatterEngineTechnologyResearch
      , gasCoreAntiMatterEngineTechnologyResearch
      )
    , ( view etrId plasmaCoreAntiMatterEngineTechnologyResearch
      , plasmaCoreAntiMatterEngineTechnologyResearch
      )
    , ( view etrId beamCoreAntiMatterEngineTechnologyResearch
      , beamCoreAntiMatterEngineTechnologyResearch
      )
    , ( view etrId photonicEngineTechnologyResearch
      , photonicEngineTechnologyResearch
      )
    ]
  )

startingPowerEfficiencyResearch
  :: ResearchStatus PowerEfficiencyModifierResearch
startingPowerEfficiencyResearch = ResearchStatus
  (Map.fromList
    [ ( view pemrId power1Efficiency1ModifierResearch
      , power1Efficiency1ModifierResearch
      )
    , ( view pemrId power05Efficiency018ModifierResearch
      , power05Efficiency018ModifierResearch
      )
    ]
  )
  (Just power04Efficiency01ModifierResearch)
  Map.empty
  (Map.fromList
    [ ( view pemrId power125Efficiency175ModifierResearch
      , power125Efficiency175ModifierResearch
      )
    , ( view pemrId power04Efficiency01ModifierResearch
      , power04Efficiency01ModifierResearch
      )
    , ( view pemrId power15Efficiency275ModifierResearch
      , power15Efficiency275ModifierResearch
      )
    , ( view pemrId power03Efficiency005ModifierResearch
      , power03Efficiency005ModifierResearch
      )
    , ( view pemrId power175Efficiency4ModifierResearch
      , power175Efficiency4ModifierResearch
      )
    , ( view pemrId power025Efficiency003ModifierResearch
      , power025Efficiency003ModifierResearch
      )
    , ( view pemrId power2Efficiency55ModifierResearch
      , power2Efficiency55ModifierResearch
      )
    , ( view pemrId power02Efficiency002ModifierResearch
      , power02Efficiency002ModifierResearch
      )
    , ( view pemrId power25Efficiency10ModifierResearch
      , power25Efficiency10ModifierResearch
      )
    , ( view pemrId power015Efficiency001ModifierResearch
      , power015Efficiency001ModifierResearch
      )
    , ( view pemrId power3Efficiency15ModifierResearch
      , power3Efficiency15ModifierResearch
      )
    , ( view pemrId power01Efficiency0001ModifierResearch
      , power01Efficiency0001ModifierResearch
      )
    ]
  )

startingFuelConsumptionResearch :: ResearchStatus FuelConsumptionResearch
startingFuelConsumptionResearch = ResearchStatus
  (Map.fromList
    [(view fcrId fuelConsumption1Research, fuelConsumption1Research)]
  )
  (Just fuelConsumption09Research)
  Map.empty
  (Map.fromList
    [ (view fcrId fuelConsumption08Research  , fuelConsumption08Research)
    , (view fcrId fuelConsumption07Research  , fuelConsumption07Research)
    , (view fcrId fuelConsumption06Research  , fuelConsumption06Research)
    , (view fcrId fuelConsumption05Research  , fuelConsumption05Research)
    , (view fcrId fuelConsumption04Research  , fuelConsumption04Research)
    , (view fcrId fuelConsumption03Research  , fuelConsumption03Research)
    , (view fcrId fuelConsumption025Research , fuelConsumption025Research)
    , (view fcrId fuelConsumption02Research  , fuelConsumption02Research)
    , (view fcrId fuelConsumption016Research , fuelConsumption016Research)
    , (view fcrId fuelConsumption0125Research, fuelConsumption0125Research)
    , (view fcrId fuelConsumption01Research  , fuelConsumption01Research)
    ]
  )

-- For now, all unlocked.
startingEngineSizeResearch :: ResearchStatus EngineSizeResearch
startingEngineSizeResearch = ResearchStatus
  (Map.fromList
    [ (view esrId engineSize1Research , engineSize1Research)
    , (view esrId engineSize5Research , engineSize5Research)
    , (view esrId engineSize10Research, engineSize10Research)
    , (view esrId engineSize15Research, engineSize15Research)
    , (view esrId engineSize20Research, engineSize20Research)
    , (view esrId engineSize25Research, engineSize25Research)
    , (view esrId engineSize30Research, engineSize30Research)
    , (view esrId engineSize35Research, engineSize35Research)
    , (view esrId engineSize40Research, engineSize40Research)
    , (view esrId engineSize45Research, engineSize45Research)
    , (view esrId engineSize50Research, engineSize50Research)
    ]
  )
  Nothing
  Map.empty
  Map.empty

startingMissleLauncherDesigns :: ResearchStatus MissleLauncherDesign
startingMissleLauncherDesigns =
  ResearchStatus Map.empty Nothing Map.empty Map.empty

-- For now, all unlocked.
startingMissleLauncherSizeResearch :: ResearchStatus MissleLauncherSizeResearch
startingMissleLauncherSizeResearch = ResearchStatus
  (Map.fromList
    [ (view mlsrId missleLauncherSize1Research  , missleLauncherSize1Research)
    , (view mlsrId missleLauncherSize2Research  , missleLauncherSize2Research)
    , (view mlsrId missleLauncherSize3Research  , missleLauncherSize3Research)
    , (view mlsrId missleLauncherSize4Research  , missleLauncherSize4Research)
    , (view mlsrId missleLauncherSize5Research  , missleLauncherSize5Research)
    , (view mlsrId missleLauncherSize6Research  , missleLauncherSize6Research)
    , (view mlsrId missleLauncherSize8Research  , missleLauncherSize8Research)
    , (view mlsrId missleLauncherSize10Research , missleLauncherSize10Research)
    , (view mlsrId missleLauncherSize12Research , missleLauncherSize12Research)
    , (view mlsrId missleLauncherSize15Research , missleLauncherSize15Research)
    , (view mlsrId missleLauncherSize20Research , missleLauncherSize20Research)
    , (view mlsrId missleLauncherSize25Research , missleLauncherSize25Research)
    , (view mlsrId missleLauncherSize30Research , missleLauncherSize30Research)
    , (view mlsrId missleLauncherSize35Research , missleLauncherSize35Research)
    , (view mlsrId missleLauncherSize40Research , missleLauncherSize40Research)
    , (view mlsrId missleLauncherSize45Research , missleLauncherSize45Research)
    , (view mlsrId missleLauncherSize50Research , missleLauncherSize50Research)
    , (view mlsrId missleLauncherSize60Research , missleLauncherSize60Research)
    , (view mlsrId missleLauncherSize70Research , missleLauncherSize70Research)
    , (view mlsrId missleLauncherSize80Research , missleLauncherSize80Research)
    , (view mlsrId missleLauncherSize90Research , missleLauncherSize90Research)
    , (view mlsrId missleLauncherSize100Research, missleLauncherSize100Research)
    ]
  )
  Nothing
  Map.empty
  Map.empty

startingMissleLauncherReloadRateResearch
  :: ResearchStatus MissleLauncherReloadRateResearch
startingMissleLauncherReloadRateResearch = ResearchStatus
  (Map.fromList
    [ ( view mlrrrId missleLauncherReloadRate1Research
      , missleLauncherReloadRate1Research
      )
    ]
  )
  (Just missleLauncherReloadRate2Research)
  Map.empty
  (Map.fromList
    [ ( view mlrrrId missleLauncherReloadRate3Research
      , missleLauncherReloadRate3Research
      )
    , ( view mlrrrId missleLauncherReloadRate4Research
      , missleLauncherReloadRate4Research
      )
    , ( view mlrrrId missleLauncherReloadRate5Research
      , missleLauncherReloadRate5Research
      )
    , ( view mlrrrId missleLauncherReloadRate6Research
      , missleLauncherReloadRate6Research
      )
    , ( view mlrrrId missleLauncherReloadRate7Research
      , missleLauncherReloadRate7Research
      )
    , ( view mlrrrId missleLauncherReloadRate8Research
      , missleLauncherReloadRate8Research
      )
    , ( view mlrrrId missleLauncherReloadRate9Research
      , missleLauncherReloadRate9Research
      )
    , ( view mlrrrId missleLauncherReloadRate10Research
      , missleLauncherReloadRate10Research
      )
    , ( view mlrrrId missleLauncherReloadRate11Research
      , missleLauncherReloadRate11Research
      )
    , ( view mlrrrId missleLauncherReloadRate12Research
      , missleLauncherReloadRate12Research
      )
    ]
  )

startingMissleLauncherReducedSizeResearch
  :: ResearchStatus MissleLauncherReducedSizeResearch
startingMissleLauncherReducedSizeResearch = ResearchStatus
  (Map.fromList
    [ ( view mlrsrId missleLauncherReducedSize1Research
      , missleLauncherReducedSize1Research
      )
    ]
  )
  (Just missleLauncherReducedSize075Research)
  Map.empty
  (Map.fromList
    [ ( view mlrsrId missleLauncherReducedSize05Research
      , missleLauncherReducedSize05Research
      )
    , ( view mlrsrId missleLauncherReducedSize033Research
      , missleLauncherReducedSize033Research
      )
    , ( view mlrsrId missleLauncherReducedSize025Research
      , missleLauncherReducedSize025Research
      )
    , ( view mlrsrId missleLauncherReducedSize015Research
      , missleLauncherReducedSize015Research
      )
    ]
  )

startingLaserDesigns :: ResearchStatus LaserDesign
startingLaserDesigns = ResearchStatus Map.empty Nothing Map.empty Map.empty

startingLaserFocalSizeResearch :: ResearchStatus LaserFocalSizeResearch
startingLaserFocalSizeResearch = ResearchStatus
  (Map.fromList
    [(view lfsrId laserFocalSize10Research, laserFocalSize10Research)]
  )
  (Just laserFocalSize12Research)
  Map.empty
  (Map.fromList
    [ (view lfsrId laserFocalSize15Research, laserFocalSize15Research)
    , (view lfsrId laserFocalSize20Research, laserFocalSize20Research)
    , (view lfsrId laserFocalSize25Research, laserFocalSize25Research)
    , (view lfsrId laserFocalSize30Research, laserFocalSize30Research)
    , (view lfsrId laserFocalSize35Research, laserFocalSize35Research)
    , (view lfsrId laserFocalSize40Research, laserFocalSize40Research)
    , (view lfsrId laserFocalSize50Research, laserFocalSize50Research)
    , (view lfsrId laserFocalSize60Research, laserFocalSize60Research)
    , (view lfsrId laserFocalSize70Research, laserFocalSize70Research)
    , (view lfsrId laserFocalSize80Research, laserFocalSize80Research)
    ]
  )

startingLaserWavelengthResearch :: ResearchStatus LaserWavelengthResearch
startingLaserWavelengthResearch = ResearchStatus
  (Map.fromList
    [ ( view lwrId laserWavelengthInfraredResearch
      , laserWavelengthInfraredResearch
      )
    ]
  )
  (Just laserWavelengthVisibleLightResearch)
  Map.empty
  (Map.fromList
    [ ( view lwrId laserWavelengthNearUltravioletResearch
      , laserWavelengthNearUltravioletResearch
      )
    , ( view lwrId laserWavelengthUltravioletResearch
      , laserWavelengthUltravioletResearch
      )
    , ( view lwrId laserWavelengthFarUltravioletResearch
      , laserWavelengthFarUltravioletResearch
      )
    , ( view lwrId laserWavelengthSoftXRayResearch
      , laserWavelengthSoftXRayResearch
      )
    , (view lwrId laserWavelengthXRayResearch, laserWavelengthXRayResearch)
    , ( view lwrId laserWavelengthFarXRayResearch
      , laserWavelengthFarXRayResearch
      )
    , ( view lwrId laserWavelengthExtremeXRayResearch
      , laserWavelengthExtremeXRayResearch
      )
    , ( view lwrId laserWavelengthNearGammaRayResearch
      , laserWavelengthNearGammaRayResearch
      )
    , ( view lwrId laserWavelengthGammaRayResearch
      , laserWavelengthGammaRayResearch
      )
    , ( view lwrId laserWavelengthFarGammaRayResearch
      , laserWavelengthFarGammaRayResearch
      )
    ]
  )

startingLaserRechargeRateResearch :: ResearchStatus LaserRechargeRateResearch
startingLaserRechargeRateResearch = ResearchStatus
  (Map.fromList
    [(view lrrrId laserRechargeRate1Research, laserRechargeRate1Research)]
  )
  (Just laserRechargeRate2Research)
  Map.empty
  (Map.fromList
    [ (view lrrrId laserRechargeRate3Research , laserRechargeRate3Research)
    , (view lrrrId laserRechargeRate4Research , laserRechargeRate4Research)
    , (view lrrrId laserRechargeRate5Research , laserRechargeRate5Research)
    , (view lrrrId laserRechargeRate6Research , laserRechargeRate6Research)
    , (view lrrrId laserRechargeRate8Research , laserRechargeRate8Research)
    , (view lrrrId laserRechargeRate10Research, laserRechargeRate10Research)
    , (view lrrrId laserRechargeRate12Research, laserRechargeRate12Research)
    , (view lrrrId laserRechargeRate16Research, laserRechargeRate16Research)
    , (view lrrrId laserRechargeRate20Research, laserRechargeRate20Research)
    , (view lrrrId laserRechargeRate25Research, laserRechargeRate25Research)
    ]
  )

startingLaserReducedSizeResearch :: ResearchStatus LaserReducedSizeResearch
startingLaserReducedSizeResearch = ResearchStatus
  (Map.fromList
    [(view lrsrId laserReducedSize1Research, laserReducedSize1Research)]
  )
  (Just laserReducedSize075Research)
  Map.empty
  (Map.fromList
    [ (view lrsrId laserReducedSize05Research , laserReducedSize05Research)
    , (view lrsrId laserReducedSize025Research, laserReducedSize025Research)
    ]
  )

startingArmorResearch :: ResearchStatus ArmorResearch
startingArmorResearch = ResearchStatus
  (Map.fromList
    [(view arId conventionalArmorResearch, conventionalArmorResearch)]
  )
  (Just duraniumArmorResearch)
  Map.empty
  (Map.fromList
    [ ( view arId highDensityDuraniumArmorResearch
      , highDensityDuraniumArmorResearch
      )
    , (view arId compositeArmorResearch        , compositeArmorResearch)
    , (view arId ceramicCompositeArmorResearch , ceramicCompositeArmorResearch)
    , (view arId laminateCompositeArmorResearch, laminateCompositeArmorResearch)
    , (view arId compressedCarbonArmorResearch , compressedCarbonArmorResearch)
    , (view arId biphaseCarbideArmorResearch   , biphaseCarbideArmorResearch)
    , ( view arId crystallineCompositeArmorResearch
      , crystallineCompositeArmorResearch
      )
    , (view arId superdenseArmorResearch      , superdenseArmorResearch)
    , (view arId bondedSuperdenseArmorResearch, bondedSuperdenseArmorResearch)
    , ( view arId coherentSuperdenseArmorResearch
      , coherentSuperdenseArmorResearch
      )
    , (view arId collapsiumArmorResearch, collapsiumArmorResearch)
    ]
  )

startingShieldResearch :: ResearchStatus ShieldResearch
startingShieldResearch = ResearchStatus
  Map.empty
  (Just alphaShieldResearch)
  Map.empty
  (Map.fromList
    [ (view shrId betaShieldResearch   , betaShieldResearch)
    , (view shrId gammaShieldResearch  , gammaShieldResearch)
    , (view shrId deltaShieldResearch  , deltaShieldResearch)
    , (view shrId epsilonShieldResearch, epsilonShieldResearch)
    , (view shrId thetaShieldResearch  , thetaShieldResearch)
    , (view shrId xiShieldResearch     , xiShieldResearch)
    , (view shrId omicronShieldResearch, omicronShieldResearch)
    , (view shrId sigmaShieldResearch  , sigmaShieldResearch)
    , (view shrId tauShieldResearch    , tauShieldResearch)
    , (view shrId psiShieldResearch    , psiShieldResearch)
    , (view shrId omegaShieldResearch  , omegaShieldResearch)
    ]
  )

startingFuelStorageResearch :: ResearchStatus FuelStorageResearch
startingFuelStorageResearch = ResearchStatus
  (Map.fromList
    [(view fsrId fuelStorageBasicResearch, fuelStorageBasicResearch)]
  )
  (Just fuelStorageLargeResearch)
  Map.empty
  (Map.fromList
    [ (view fsrId fuelStorageTinyResearch      , fuelStorageTinyResearch)
    , (view fsrId fuelStorageSmallResearch     , fuelStorageSmallResearch)
    , (view fsrId fuelStorageVeryLargeResearch , fuelStorageVeryLargeResearch)
    , (view fsrId fuelStorageUltraLargeResearch, fuelStorageUltraLargeResearch)
    ]
  )

startingCargoHandlingResearch :: ResearchStatus CargoHandlingResearch
startingCargoHandlingResearch = ResearchStatus
  (Map.fromList
    [ ( view chsrId slowCargoHandlingSystemResearch
      , slowCargoHandlingSystemResearch
      )
    ]
  )
  (Just standardCargoHandlingSystemResearch)
  Map.empty
  (Map.fromList
    [ ( view chsrId improvedCargoHandlingSystemResearch
      , improvedCargoHandlingSystemResearch
      )
    , ( view chsrId advancedCargoHandlingSystemResearch
      , advancedCargoHandlingSystemResearch
      )
    , ( view chsrId gravAssistedCargoHandlingSystemResearch
      , gravAssistedCargoHandlingSystemResearch
      )
    ]
  )

startingCargoHoldResearch :: ResearchStatus CargoHoldResearch
startingCargoHoldResearch = ResearchStatus
  (Map.fromList [(view chrId smallCargoHoldResearch, smallCargoHoldResearch)])
  (Just standardCargoHoldResearch)
  Map.empty
  (Map.fromList
    [ (view chrId largeCargoHoldResearch  , largeCargoHoldResearch)
    , (view chrId massiveCargoHoldResearch, massiveCargoHoldResearch)
    ]
  )

startingJumpGateResearch :: ResearchStatus JumpGateResearch
startingJumpGateResearch = ResearchStatus
  Map.empty
  (Just smallJumpGateResearch)
  Map.empty
  (Map.fromList
    [ (view jgrId mediumJumpGateResearch, mediumJumpGateResearch)
    , (view jgrId largeJumpGateResearch , largeJumpGateResearch)
    ]
  )

startingGeologicalSensorResearch :: ResearchStatus SensorResearch
startingGeologicalSensorResearch = ResearchStatus
  (Map.fromList
    [(view srId basicGeologicalSensorResearch, basicGeologicalSensorResearch)]
  )
  (Just improvedGeologicalSensorResearch)
  Map.empty
  (Map.fromList
    [ ( view srId advancedGeologicalSensorResearch
      , advancedGeologicalSensorResearch
      )
    ]
  )

startingGravitationalSensorResearch :: ResearchStatus SensorResearch
startingGravitationalSensorResearch = ResearchStatus
  (Map.fromList
    [ ( view srId basicGravitationalSensorResearch
      , basicGravitationalSensorResearch
      )
    ]
  )
  (Just improvedGravitationalSensorResearch)
  Map.empty
  (Map.fromList
    [ ( view srId advancedGravitationalSensorResearch
      , advancedGravitationalSensorResearch
      )
    ]
  )

startingMineResearch :: ResearchStatus InstallmentResearch
startingMineResearch = ResearchStatus
  (Map.fromList [(view irId basicMineResearch, basicMineResearch)])
  (Just improvedMineResearch)
  Map.empty
  (Map.fromList [(view irId advancedMineResearch, advancedMineResearch)])

startingResearchLabResearch :: ResearchStatus InstallmentResearch
startingResearchLabResearch = ResearchStatus
  (Map.fromList [(view irId basicResearchLabResearch, basicResearchLabResearch)]
  )
  Nothing
  Map.empty
  Map.empty

startingFuelRefineryResearch :: ResearchStatus InstallmentResearch
startingFuelRefineryResearch = ResearchStatus
  (Map.fromList
    [(view irId basicFuelRefineryResearch, basicFuelRefineryResearch)]
  )
  (Just improvedFuelRefineryResearch)
  Map.empty
  (Map.fromList
    [(view irId advancedFuelRefineryResearch, advancedFuelRefineryResearch)]
  )

startingConstructionFactoryResearch :: ResearchStatus InstallmentResearch
startingConstructionFactoryResearch = ResearchStatus
  (Map.fromList
    [ ( view irId basicConstructionFactoryResearch
      , basicConstructionFactoryResearch
      )
    ]
  )
  (Just improvedConstructionFactoryResearch)
  Map.empty
  (Map.fromList
    [ ( view irId advancedConstructionFactoryResearch
      , advancedConstructionFactoryResearch
      )
    ]
  )

startingMassDriverResearch :: ResearchStatus InstallmentResearch
startingMassDriverResearch = ResearchStatus
  (Map.fromList [(view irId basicMassDriverResearch, basicMassDriverResearch)])
  (Just improvedMassDriverResearch)
  Map.empty
  (Map.fromList
    [(view irId advancedMassDriverResearch, advancedMassDriverResearch)]
  )

startingCommercialShipyardResearch :: ResearchStatus InstallmentResearch
startingCommercialShipyardResearch = ResearchStatus
  (Map.fromList
    [(view irId commercialShipyardResearch, commercialShipyardResearch)]
  )
  Nothing
  Map.empty
  Map.empty

startingNavalShipyardResearch :: ResearchStatus InstallmentResearch
startingNavalShipyardResearch = ResearchStatus
  (Map.fromList [(view irId navalShipyardResearch, navalShipyardResearch)])
  Nothing
  Map.empty
  Map.empty

unlockAllRaceResearch :: RaceId -> GameState -> GameState
unlockAllRaceResearch rid gs = newGameState where
  maybeRaceResearch = Map.lookup rid (view research gs)
  newGameState      = if isJust maybeRaceResearch then unlockedGameState gs else gs
  unlockedGameState =
    over (research . at' rid . rShipDesigns) (unlockAllResearchStatus sdId)
      . over (research . at' rid . rEngineDesigns)
             (unlockAllResearchStatus edId)
      . over (research . at' rid . rEngineTechnology)
             (unlockAllResearchStatus etrId)
      . over (research . at' rid . rEngineModifier)
             (unlockAllResearchStatus pemrId)
      . over (research . at' rid . rEngineFuelConsumption)
             (unlockAllResearchStatus fcrId)
      . over (research . at' rid . rEngineSize)
             (unlockAllResearchStatus esrId)
      . over (research . at' rid . rMissleLauncherDesigns)
             (unlockAllResearchStatus mldId)
      . over (research . at' rid . rMissleLauncherSize)
             (unlockAllResearchStatus mlsrId)
      . over (research . at' rid . rMissleLauncherReloadRate)
             (unlockAllResearchStatus mlrrrId)
      . over (research . at' rid . rMissleLauncherReducedSize)
             (unlockAllResearchStatus mlrsrId)
      . over (research . at' rid . rLaserDesigns)
             (unlockAllResearchStatus ldId)
      . over (research . at' rid . rLaserFocalSize)
             (unlockAllResearchStatus lfsrId)
      . over (research . at' rid . rLaserWavelength)
             (unlockAllResearchStatus lwrId)
      . over (research . at' rid . rLaserRechargeRate)
             (unlockAllResearchStatus lrrrId)
      . over (research . at' rid . rLaserReducedSize)
             (unlockAllResearchStatus lrsrId)
      . over (research . at' rid . rArmor)
             (unlockAllResearchStatus arId)
      . over (research . at' rid . rShield)
             (unlockAllResearchStatus shrId)
      . over (research . at' rid . rFuelStorage)
             (unlockAllResearchStatus fsrId)
      . over (research . at' rid . rCargoHandling)
             (unlockAllResearchStatus chsrId)
      . over (research . at' rid . rCargoHold)
             (unlockAllResearchStatus chrId)
      . over (research . at' rid . rJumpGate)
             (unlockAllResearchStatus jgrId)
      . over (research . at' rid . rGeologicalSensor)
             (unlockAllResearchStatus srId)
      . over (research . at' rid . rGravitationalSensor)
             (unlockAllResearchStatus srId)
      . over (research . at' rid . rMines)
             (unlockAllResearchStatus irId)
      . over (research . at' rid . rResearchLabs)
             (unlockAllResearchStatus irId)
      . over (research . at' rid . rFuelRefineries)
             (unlockAllResearchStatus irId)
      . over (research . at' rid . rConstructionFactories)
             (unlockAllResearchStatus irId)
      . over (research . at' rid . rMassDrivers)
             (unlockAllResearchStatus irId)
      . over (research . at' rid . rCommercialShipyards)
             (unlockAllResearchStatus irId)
      . over (research . at' rid . rNavalShipyards)
             (unlockAllResearchStatus irId)

unlockAllResearchStatus :: ((ComponentId -> Const ComponentId ComponentId) -> a -> Const ComponentId a) -> ResearchStatus a -> ResearchStatus a
unlockAllResearchStatus idSelector rs@(ResearchStatus unl pend _ lo) = set
  unlocked
  newUnlocked
  (set locked newLocked (set pending newPending rs)) where
  newUnlocked' = Map.union unl lo
  newUnlocked  = if isNothing pend
    then newUnlocked'
    else Map.insert pendId (fromJust pend) newUnlocked'
  pendId     = view idSelector (fromJust pend)
  newLocked  = Map.empty
  newPending = Nothing

performRaceResearch :: Seconds -> RaceResearch -> RaceResearch
performRaceResearch secs = Map.map (performResearch secs)

performResearch :: Seconds -> Research -> Research
performResearch secs resrch = newResearch
 where
  edRS   = researchEngineDesign secs (view rEngineDesigns resrch)
  etrRS  = researchEngineTechnology secs (view rEngineTechnology resrch)
  fcrRS  = researchFuelConsumption secs (view rEngineFuelConsumption resrch)
  pemrRS = researchEngineModifier secs (view rEngineModifier resrch)
  esrRS  = researchEngineSize secs (view rEngineSize resrch)
  mldRS =
    researchMissleLauncherDesign secs (view rMissleLauncherDesigns resrch)
  mlsRS  = researchMissleLauncherSize secs (view rMissleLauncherSize resrch)
  mlrrRS = researchMissleLauncherReloadRate
    secs
    (view rMissleLauncherReloadRate resrch)
  mlrsRS = researchMissleLauncherReducedSize
    secs
    (view rMissleLauncherReducedSize resrch)
  ldRS               = researchLaserDesign secs (view rLaserDesigns resrch)
  lfsRS              = researchLaserFocalSize secs (view rLaserFocalSize resrch)
  lwRS = researchLaserWavelength secs (view rLaserWavelength resrch)
  lrrRS = researchLaserRechargeRate secs (view rLaserRechargeRate resrch)
  lrsRS = researchLaserReducedSize secs (view rLaserReducedSize resrch)
  aRS                = researchArmor secs (view rArmor resrch)
  sRS                = researchShields secs (view rShield resrch)
  fsrRS              = researchFuelStorage secs (view rFuelStorage resrch)
  chsrRS             = researchCargoHandling secs (view rCargoHandling resrch)
  chrRS              = researchCargoHold secs (view rCargoHold resrch)
  jgrRS              = researchJumpGate secs (view rJumpGate resrch)
  sGeoRS             = researchSensor secs (view rGeologicalSensor resrch)
  sGravRS            = researchSensor secs (view rGravitationalSensor resrch)
  irMinesRS          = researchInstallment secs (view rMines resrch)
  irResearchLabsRS   = researchInstallment secs (view rResearchLabs resrch)
  irFuelRefineriesRS = researchInstallment secs (view rFuelRefineries resrch)
  irConstructionFactoriesRS =
    researchInstallment secs (view rConstructionFactories resrch)
  irMassDriversRS = researchInstallment secs (view rMassDrivers resrch)
  newResearch     = resrch { _rEngineDesigns             = edRS
                           , _rEngineTechnology          = etrRS
                           , _rEngineFuelConsumption     = fcrRS
                           , _rEngineModifier            = pemrRS
                           , _rEngineSize                = esrRS
                           , _rMissleLauncherDesigns     = mldRS
                           , _rMissleLauncherSize        = mlsRS
                           , _rMissleLauncherReloadRate  = mlrrRS
                           , _rMissleLauncherReducedSize = mlrsRS
                           , _rLaserDesigns              = ldRS
                           , _rLaserFocalSize            = lfsRS
                           , _rLaserWavelength           = lwRS
                           , _rLaserRechargeRate         = lrrRS
                           , _rLaserReducedSize          = lrsRS
                           , _rArmor                     = aRS
                           , _rShield                    = sRS
                           , _rFuelStorage               = fsrRS
                           , _rCargoHandling             = chsrRS
                           , _rCargoHold                 = chrRS
                           , _rJumpGate                  = jgrRS
                           , _rGeologicalSensor          = sGeoRS
                           , _rGravitationalSensor       = sGravRS
                           , _rMines                     = irMinesRS
                           , _rResearchLabs              = irResearchLabsRS
                           , _rFuelRefineries            = irFuelRefineriesRS
                           , _rConstructionFactories = irConstructionFactoriesRS
                           , _rMassDrivers               = irMassDriversRS
                           }

--
-- Engine Design
--
-- TODO: This should reflect the order in which the user created the engines instead of research cost
nextEngineDesign :: ResearchMap EngineDesign -> Maybe EngineDesign
nextEngineDesign edMap = engineDesign
 where
  engineDesignList       = Map.toList edMap
  sortedEngineDesignList = L.sortBy
    (comparing $ unpack . view edResearchCost)
    (map snd engineDesignList)
  engineDesign = if length sortedEngineDesignList > 0
    then Just $ head sortedEngineDesignList
    else Nothing

unlockPendingEngineDesignResearch
  :: ResearchStatus EngineDesign -> ResearchStatus EngineDesign
unlockPendingEngineDesignResearch rs@(ResearchStatus rsUnlocked rsPending _ rsLocked)
  = newRS
 where
  newRS = if isNothing rsPending
    then rs { _pending = newPending, _locked = newLocked }
    else rs { _pending  = newPending
            , _unlocked = newUnlocked
            , _locked   = newLocked
            }
  newPending       = if Map.size rsLocked > 0 then newEngineDesign' else Nothing
  pend             = fromJust rsPending
  newEngineDesign' = nextEngineDesign rsLocked
  newLocked        = if isNothing newEngineDesign'
    then Map.empty
    else Map.delete (view edId $ fromJust newEngineDesign') rsLocked
  newUnlocked = Map.insert (view edId pend) pend rsUnlocked

researchEngineDesign
  :: Seconds -> ResearchStatus EngineDesign -> ResearchStatus EngineDesign
researchEngineDesign secs researchStatus = newResearchStatus
 where
  maybePending      = view pending researchStatus
  numResearchLabs   = fromIntegral $ researchLabCount researchStatus
  newResearchStatus = if isNothing maybePending || numResearchLabs < 1
    then researchStatus
    else newResearchStatus'
  engineDesign                         = fromJust maybePending
  (ComponentResearch researchCost    ) = view edResearchCost engineDesign
  (ComponentResearch researchProgress) = view edResearchProgress engineDesign
  researchRate' = numResearchLabs * 200 / fromIntegral yearInSeconds
  addedProgress                        = researchRate' * fromIntegral secs
  addedProgressPercentage              = addedProgress / researchCost
  newProgress = researchProgress + addedProgressPercentage
  newResearchStatus'                   = if newProgress >= 1
    then unlockPendingEngineDesignResearch $ set
      (pending . _Just . edResearchProgress)
      (ComponentResearch 1)
      researchStatus
    else set (pending . _Just . edResearchProgress)
             (ComponentResearch newProgress)
             researchStatus

--
-- Engine Technology
--
nextEngineTechnology
  :: ResearchMap EngineTechnologyResearch -> Maybe EngineTechnologyResearch
nextEngineTechnology etrMap = engineTechnology
 where
  engineTechnologyList       = Map.toList etrMap
  sortedEngineTechnologyList = L.sortBy
    (comparing $ unpack . view etrResearchCost)
    (map snd engineTechnologyList)
  engineTechnology = if length sortedEngineTechnologyList > 0
    then Just $ head sortedEngineTechnologyList
    else Nothing

unlockPendingEngineTechnologyResearch
  :: ResearchStatus EngineTechnologyResearch
  -> ResearchStatus EngineTechnologyResearch
unlockPendingEngineTechnologyResearch rs@(ResearchStatus rsUnlocked rsPending _ rsLocked)
  = newRS
 where
  newRS = if isNothing rsPending
    then rs { _pending = newPending, _locked = newLocked }
    else rs { _pending  = newPending
            , _unlocked = newUnlocked
            , _locked   = newLocked
            }
  newPending = if Map.size rsLocked > 0 then newEngineTechnology else Nothing
  pend                = fromJust rsPending
  newEngineTechnology = nextEngineTechnology rsLocked
  newLocked           = if isNothing newEngineTechnology
    then Map.empty
    else Map.delete (view etrId $ fromJust newEngineTechnology) rsLocked
  newUnlocked = Map.insert (view etrId pend) pend rsUnlocked

researchEngineTechnology
  :: Seconds
  -> ResearchStatus EngineTechnologyResearch
  -> ResearchStatus EngineTechnologyResearch
researchEngineTechnology secs researchStatus = newResearchStatus
 where
  maybePending      = view pending researchStatus
  numResearchLabs   = fromIntegral $ researchLabCount researchStatus
  newResearchStatus = if isNothing maybePending || numResearchLabs < 1
    then researchStatus
    else newResearchStatus'
  engineTechnology                 = fromJust maybePending
  (ComponentResearch researchCost) = view etrResearchCost engineTechnology
  (ComponentResearch researchProgress) =
    view etrResearchProgress engineTechnology
  researchRate'           = numResearchLabs * 200 / fromIntegral yearInSeconds
  addedProgress           = researchRate' * fromIntegral secs
  addedProgressPercentage = addedProgress / researchCost
  newProgress             = researchProgress + addedProgressPercentage
  newResearchStatus'      = if newProgress >= 1
    then unlockPendingEngineTechnologyResearch $ set
      (pending . _Just . etrResearchProgress)
      (ComponentResearch 1)
      researchStatus
    else set (pending . _Just . etrResearchProgress)
             (ComponentResearch newProgress)
             researchStatus

--
-- Engine Fuel Consumption
--
nextFuelConsumption
  :: ResearchMap FuelConsumptionResearch -> Maybe FuelConsumptionResearch
nextFuelConsumption fcrMap = fuelConsumption
 where
  fuelConsumptionList       = Map.toList fcrMap
  sortedFuelConsumptionList = L.sortBy
    (comparing $ unpack . view fcrResearchCost)
    (map snd fuelConsumptionList)
  fuelConsumption = if length sortedFuelConsumptionList > 0
    then Just $ head sortedFuelConsumptionList
    else Nothing

unlockPendingFuelConsumptionResearch
  :: ResearchStatus FuelConsumptionResearch
  -> ResearchStatus FuelConsumptionResearch
unlockPendingFuelConsumptionResearch rs@(ResearchStatus rsUnlocked rsPending _ rsLocked)
  = newRS
 where
  newRS = if isNothing rsPending
    then rs { _pending = newPending, _locked = newLocked }
    else rs { _pending  = newPending
            , _unlocked = newUnlocked
            , _locked   = newLocked
            }
  newPending = if Map.size rsLocked > 0 then newFuelConsumption else Nothing
  pend               = fromJust rsPending
  newFuelConsumption = nextFuelConsumption rsLocked
  newLocked          = if isNothing newFuelConsumption
    then Map.empty
    else Map.delete (view fcrId $ fromJust newFuelConsumption) rsLocked
  newUnlocked = Map.insert (view fcrId pend) pend rsUnlocked

researchFuelConsumption
  :: Seconds
  -> ResearchStatus FuelConsumptionResearch
  -> ResearchStatus FuelConsumptionResearch
researchFuelConsumption secs researchStatus = newResearchStatus
 where
  maybePending      = view pending researchStatus
  numResearchLabs   = fromIntegral $ researchLabCount researchStatus
  newResearchStatus = if isNothing maybePending || numResearchLabs < 1
    then researchStatus
    else newResearchStatus'
  fuelConsumption                  = fromJust maybePending
  (ComponentResearch researchCost) = view fcrResearchCost fuelConsumption
  (ComponentResearch researchProgress) =
    view fcrResearchProgress fuelConsumption
  researchRate'           = numResearchLabs * 200 / fromIntegral yearInSeconds
  addedProgress           = researchRate' * fromIntegral secs
  addedProgressPercentage = addedProgress / researchCost
  newProgress             = researchProgress + addedProgressPercentage
  newResearchStatus'      = if newProgress >= 1
    then unlockPendingFuelConsumptionResearch $ set
      (pending . _Just . fcrResearchProgress)
      (ComponentResearch 1)
      researchStatus
    else set (pending . _Just . fcrResearchProgress)
             (ComponentResearch newProgress)
             researchStatus

--
-- Engine Modifier
--
nextEngineModifier
  :: ResearchMap PowerEfficiencyModifierResearch
  -> Maybe PowerEfficiencyModifierResearch
nextEngineModifier pemrMap = engineModifier
 where
  engineModifierList       = Map.toList pemrMap
  sortedEngineModifierList = L.sortBy
    (comparing $ unpack . view pemrResearchCost)
    (map snd engineModifierList)
  engineModifier = if length sortedEngineModifierList > 0
    then Just $ head sortedEngineModifierList
    else Nothing

unlockPendingEngineModifierResearch
  :: ResearchStatus PowerEfficiencyModifierResearch
  -> ResearchStatus PowerEfficiencyModifierResearch
unlockPendingEngineModifierResearch rs@(ResearchStatus rsUnlocked rsPending _ rsLocked)
  = newRS
 where
  newRS = if isNothing rsPending
    then rs { _pending = newPending, _locked = newLocked }
    else rs { _pending  = newPending
            , _unlocked = newUnlocked
            , _locked   = newLocked
            }
  newPending = if Map.size rsLocked > 0 then newEngineModifier else Nothing
  pend              = fromJust rsPending
  newEngineModifier = nextEngineModifier rsLocked
  newLocked         = if isNothing newEngineModifier
    then Map.empty
    else Map.delete (view pemrId $ fromJust newEngineModifier) rsLocked
  newUnlocked = Map.insert (view pemrId pend) pend rsUnlocked

researchEngineModifier
  :: Seconds
  -> ResearchStatus PowerEfficiencyModifierResearch
  -> ResearchStatus PowerEfficiencyModifierResearch
researchEngineModifier secs researchStatus = newResearchStatus
 where
  maybePending      = view pending researchStatus
  numResearchLabs   = fromIntegral $ researchLabCount researchStatus
  newResearchStatus = if isNothing maybePending || numResearchLabs < 1
    then researchStatus
    else newResearchStatus'
  engineModifier                   = fromJust maybePending
  (ComponentResearch researchCost) = view pemrResearchCost engineModifier
  (ComponentResearch researchProgress) =
    view pemrResearchProgress engineModifier
  researchRate'           = numResearchLabs * 200 / fromIntegral yearInSeconds
  addedProgress           = researchRate' * fromIntegral secs
  addedProgressPercentage = addedProgress / researchCost
  newProgress             = researchProgress + addedProgressPercentage
  newResearchStatus'      = if newProgress >= 1
    then unlockPendingEngineModifierResearch $ set
      (pending . _Just . pemrResearchProgress)
      (ComponentResearch 1)
      researchStatus
    else set (pending . _Just . pemrResearchProgress)
             (ComponentResearch newProgress)
             researchStatus

--
-- Engine Size
--
nextEngineSize :: ResearchMap EngineSizeResearch -> Maybe EngineSizeResearch
nextEngineSize esrMap = engineSize
 where
  engineSizeList       = Map.toList esrMap
  sortedEngineSizeList = L.sortBy (comparing $ unpack . view esrResearchCost)
                                  (map snd engineSizeList)
  engineSize = if length sortedEngineSizeList > 0
    then Just $ head sortedEngineSizeList
    else Nothing

unlockPendingEngineSizeResearch
  :: ResearchStatus EngineSizeResearch -> ResearchStatus EngineSizeResearch
unlockPendingEngineSizeResearch rs@(ResearchStatus rsUnlocked rsPending _ rsLocked)
  = newRS
 where
  newRS = if isNothing rsPending
    then rs { _pending = newPending, _locked = newLocked }
    else rs { _pending  = newPending
            , _unlocked = newUnlocked
            , _locked   = newLocked
            }
  newPending    = if Map.size rsLocked > 0 then newEngineSize else Nothing
  pend          = fromJust rsPending
  newEngineSize = nextEngineSize rsLocked
  newLocked     = if isNothing newEngineSize
    then Map.empty
    else Map.delete (view esrId $ fromJust newEngineSize) rsLocked
  newUnlocked = Map.insert (view esrId pend) pend rsUnlocked

researchEngineSize
  :: Seconds
  -> ResearchStatus EngineSizeResearch
  -> ResearchStatus EngineSizeResearch
researchEngineSize secs researchStatus = newResearchStatus
 where
  maybePending      = view pending researchStatus
  numResearchLabs   = fromIntegral $ researchLabCount researchStatus
  newResearchStatus = if isNothing maybePending || numResearchLabs < 1
    then researchStatus
    else newResearchStatus'
  engineSize                           = fromJust maybePending
  (ComponentResearch researchCost    ) = view esrResearchCost engineSize
  (ComponentResearch researchProgress) = view esrResearchProgress engineSize
  researchRate' = numResearchLabs * 200 / fromIntegral yearInSeconds
  addedProgress                        = researchRate' * fromIntegral secs
  addedProgressPercentage              = addedProgress / researchCost
  newProgress = researchProgress + addedProgressPercentage
  newResearchStatus'                   = if newProgress >= 1
    then unlockPendingEngineSizeResearch $ set
      (pending . _Just . esrResearchProgress)
      (ComponentResearch 1)
      researchStatus
    else set (pending . _Just . esrResearchProgress)
             (ComponentResearch newProgress)
             researchStatus

--
-- Laser Design
--
-- TODO: This should reflect the order in which the user created the lasers instead of research cost
nextLaserDesign :: ResearchMap LaserDesign -> Maybe LaserDesign
nextLaserDesign lMap = laserDesign
 where
  laserDesignList       = Map.toList lMap
  sortedLaserDesignList = L.sortBy (comparing $ unpack . view ldResearchCost)
                                   (map snd laserDesignList)
  laserDesign = if length sortedLaserDesignList > 0
    then Just $ head sortedLaserDesignList
    else Nothing

unlockPendingLaserDesignResearch
  :: ResearchStatus LaserDesign -> ResearchStatus LaserDesign
unlockPendingLaserDesignResearch rs@(ResearchStatus rsUnlocked rsPending _ rsLocked)
  = newRS
 where
  newRS = if isNothing rsPending
    then rs { _pending = newPending, _locked = newLocked }
    else rs { _pending  = newPending
            , _unlocked = newUnlocked
            , _locked   = newLocked
            }
  newPending      = if Map.size rsLocked > 0 then newLaserDesign' else Nothing
  pend            = fromJust rsPending
  newLaserDesign' = nextLaserDesign rsLocked
  newLocked       = if isNothing newLaserDesign'
    then Map.empty
    else Map.delete (view ldId $ fromJust newLaserDesign') rsLocked
  newUnlocked = Map.insert (view ldId pend) pend rsUnlocked

researchLaserDesign
  :: Seconds -> ResearchStatus LaserDesign -> ResearchStatus LaserDesign
researchLaserDesign secs researchStatus = newResearchStatus
 where
  maybePending      = view pending researchStatus
  numResearchLabs   = fromIntegral $ researchLabCount researchStatus
  newResearchStatus = if isNothing maybePending || numResearchLabs < 1
    then researchStatus
    else newResearchStatus'
  laserDesign                          = fromJust maybePending
  (ComponentResearch researchCost    ) = view ldResearchCost laserDesign
  (ComponentResearch researchProgress) = view ldResearchProgress laserDesign
  researchRate' = numResearchLabs * 200 / fromIntegral yearInSeconds
  addedProgress                        = researchRate' * fromIntegral secs
  addedProgressPercentage              = addedProgress / researchCost
  newProgress = researchProgress + addedProgressPercentage
  newResearchStatus'                   = if newProgress >= 1
    then unlockPendingLaserDesignResearch $ set
      (pending . _Just . ldResearchProgress)
      (ComponentResearch 1)
      researchStatus
    else set (pending . _Just . ldResearchProgress)
             (ComponentResearch newProgress)
             researchStatus

--
-- Laser Focal Size
--
nextLaserFocalSizeResearch
  :: ResearchMap LaserFocalSizeResearch -> Maybe LaserFocalSizeResearch
nextLaserFocalSizeResearch lfsMap = laserFocalSize
 where
  laserFocalSizeList       = Map.toList lfsMap
  sortedLaserFocalSizeList = L.sortBy
    (comparing $ unpack . view lfsrResearchCost)
    (map snd laserFocalSizeList)
  laserFocalSize = if length sortedLaserFocalSizeList > 0
    then Just $ head sortedLaserFocalSizeList
    else Nothing

unlockPendingLaserFocalSizeResearch
  :: ResearchStatus LaserFocalSizeResearch
  -> ResearchStatus LaserFocalSizeResearch
unlockPendingLaserFocalSizeResearch rs@(ResearchStatus rsUnlocked rsPending _ rsLocked)
  = newRS
 where
  newRS = if isNothing rsPending
    then rs { _pending = newPending, _locked = newLocked }
    else rs { _pending  = newPending
            , _unlocked = newUnlocked
            , _locked   = newLocked
            }
  newPending = if Map.size rsLocked > 0 then newLaserFocalSize' else Nothing
  pend               = fromJust rsPending
  newLaserFocalSize' = nextLaserFocalSizeResearch rsLocked
  newLocked          = if isNothing newLaserFocalSize'
    then Map.empty
    else Map.delete (view lfsrId $ fromJust newLaserFocalSize') rsLocked
  newUnlocked = Map.insert (view lfsrId pend) pend rsUnlocked

researchLaserFocalSize
  :: Seconds
  -> ResearchStatus LaserFocalSizeResearch
  -> ResearchStatus LaserFocalSizeResearch
researchLaserFocalSize secs researchStatus = newResearchStatus
 where
  maybePending      = view pending researchStatus
  numResearchLabs   = fromIntegral $ researchLabCount researchStatus
  newResearchStatus = if isNothing maybePending || numResearchLabs < 1
    then researchStatus
    else newResearchStatus'
  laserFocalSize                   = fromJust maybePending
  (ComponentResearch researchCost) = view lfsrResearchCost laserFocalSize
  (ComponentResearch researchProgress) =
    view lfsrResearchProgress laserFocalSize
  researchRate'           = numResearchLabs * 200 / fromIntegral yearInSeconds
  addedProgress           = researchRate' * fromIntegral secs
  addedProgressPercentage = addedProgress / researchCost
  newProgress             = researchProgress + addedProgressPercentage
  newResearchStatus'      = if newProgress >= 1
    then unlockPendingLaserFocalSizeResearch $ set
      (pending . _Just . lfsrResearchProgress)
      (ComponentResearch 1)
      researchStatus
    else set (pending . _Just . lfsrResearchProgress)
             (ComponentResearch newProgress)
             researchStatus

--
-- Laser Wavelength
--
nextLaserWavelengthResearch
  :: ResearchMap LaserWavelengthResearch -> Maybe LaserWavelengthResearch
nextLaserWavelengthResearch lwMap = laserWavelength
 where
  laserWavelengthList       = Map.toList lwMap
  sortedLaserWavelengthList = L.sortBy
    (comparing $ unpack . view lwrResearchCost)
    (map snd laserWavelengthList)
  laserWavelength = if length sortedLaserWavelengthList > 0
    then Just $ head sortedLaserWavelengthList
    else Nothing

unlockPendingLaserWavelengthResearch
  :: ResearchStatus LaserWavelengthResearch
  -> ResearchStatus LaserWavelengthResearch
unlockPendingLaserWavelengthResearch rs@(ResearchStatus rsUnlocked rsPending _ rsLocked)
  = newRS
 where
  newRS = if isNothing rsPending
    then rs { _pending = newPending, _locked = newLocked }
    else rs { _pending  = newPending
            , _unlocked = newUnlocked
            , _locked   = newLocked
            }
  newPending = if Map.size rsLocked > 0 then newLaserWavelength' else Nothing
  pend                = fromJust rsPending
  newLaserWavelength' = nextLaserWavelengthResearch rsLocked
  newLocked           = if isNothing newLaserWavelength'
    then Map.empty
    else Map.delete (view lwrId $ fromJust newLaserWavelength') rsLocked
  newUnlocked = Map.insert (view lwrId pend) pend rsUnlocked

researchLaserWavelength
  :: Seconds
  -> ResearchStatus LaserWavelengthResearch
  -> ResearchStatus LaserWavelengthResearch
researchLaserWavelength secs researchStatus = newResearchStatus
 where
  maybePending      = view pending researchStatus
  numResearchLabs   = fromIntegral $ researchLabCount researchStatus
  newResearchStatus = if isNothing maybePending || numResearchLabs < 1
    then researchStatus
    else newResearchStatus'
  laserWavelength                  = fromJust maybePending
  (ComponentResearch researchCost) = view lwrResearchCost laserWavelength
  (ComponentResearch researchProgress) =
    view lwrResearchProgress laserWavelength
  researchRate'           = numResearchLabs * 200 / fromIntegral yearInSeconds
  addedProgress           = researchRate' * fromIntegral secs
  addedProgressPercentage = addedProgress / researchCost
  newProgress             = researchProgress + addedProgressPercentage
  newResearchStatus'      = if newProgress >= 1
    then unlockPendingLaserWavelengthResearch $ set
      (pending . _Just . lwrResearchProgress)
      (ComponentResearch 1)
      researchStatus
    else set (pending . _Just . lwrResearchProgress)
             (ComponentResearch newProgress)
             researchStatus

--
-- Laser Recharge Rate
--
nextLaserRechargeRateResearch
  :: ResearchMap LaserRechargeRateResearch -> Maybe LaserRechargeRateResearch
nextLaserRechargeRateResearch lrrMap = laserRechargeRate
 where
  laserRechargeRateList       = Map.toList lrrMap
  sortedLaserRechargeRateList = L.sortBy
    (comparing $ unpack . view lrrrResearchCost)
    (map snd laserRechargeRateList)
  laserRechargeRate = if length sortedLaserRechargeRateList > 0
    then Just $ head sortedLaserRechargeRateList
    else Nothing

unlockPendingLaserRechargeRateResearch
  :: ResearchStatus LaserRechargeRateResearch
  -> ResearchStatus LaserRechargeRateResearch
unlockPendingLaserRechargeRateResearch rs@(ResearchStatus rsUnlocked rsPending _ rsLocked)
  = newRS
 where
  newRS = if isNothing rsPending
    then rs { _pending = newPending, _locked = newLocked }
    else rs { _pending  = newPending
            , _unlocked = newUnlocked
            , _locked   = newLocked
            }
  newPending = if Map.size rsLocked > 0 then newLaserRechargeRate' else Nothing
  pend                  = fromJust rsPending
  newLaserRechargeRate' = nextLaserRechargeRateResearch rsLocked
  newLocked             = if isNothing newLaserRechargeRate'
    then Map.empty
    else Map.delete (view lrrrId $ fromJust newLaserRechargeRate') rsLocked
  newUnlocked = Map.insert (view lrrrId pend) pend rsUnlocked

researchLaserRechargeRate
  :: Seconds
  -> ResearchStatus LaserRechargeRateResearch
  -> ResearchStatus LaserRechargeRateResearch
researchLaserRechargeRate secs researchStatus = newResearchStatus
 where
  maybePending      = view pending researchStatus
  numResearchLabs   = fromIntegral $ researchLabCount researchStatus
  newResearchStatus = if isNothing maybePending || numResearchLabs < 1
    then researchStatus
    else newResearchStatus'
  laserRechargeRate                = fromJust maybePending
  (ComponentResearch researchCost) = view lrrrResearchCost laserRechargeRate
  (ComponentResearch researchProgress) =
    view lrrrResearchProgress laserRechargeRate
  researchRate'           = numResearchLabs * 200 / fromIntegral yearInSeconds
  addedProgress           = researchRate' * fromIntegral secs
  addedProgressPercentage = addedProgress / researchCost
  newProgress             = researchProgress + addedProgressPercentage
  newResearchStatus'      = if newProgress >= 1
    then unlockPendingLaserRechargeRateResearch $ set
      (pending . _Just . lrrrResearchProgress)
      (ComponentResearch 1)
      researchStatus
    else set (pending . _Just . lrrrResearchProgress)
             (ComponentResearch newProgress)
             researchStatus

--
-- Laser Reduced Size
--
nextLaserReducedSizeResearch
  :: ResearchMap LaserReducedSizeResearch -> Maybe LaserReducedSizeResearch
nextLaserReducedSizeResearch lrsMap = laserReducedSize
 where
  laserReducedSizeList       = Map.toList lrsMap
  sortedLaserReducedSizeList = L.sortBy
    (comparing $ unpack . view lrsrResearchCost)
    (map snd laserReducedSizeList)
  laserReducedSize = if length sortedLaserReducedSizeList > 0
    then Just $ head sortedLaserReducedSizeList
    else Nothing

unlockPendingLaserReducedSizeResearch
  :: ResearchStatus LaserReducedSizeResearch
  -> ResearchStatus LaserReducedSizeResearch
unlockPendingLaserReducedSizeResearch rs@(ResearchStatus rsUnlocked rsPending _ rsLocked)
  = newRS
 where
  newRS = if isNothing rsPending
    then rs { _pending = newPending, _locked = newLocked }
    else rs { _pending  = newPending
            , _unlocked = newUnlocked
            , _locked   = newLocked
            }
  newPending = if Map.size rsLocked > 0 then newLaserReducedSize' else Nothing
  pend                 = fromJust rsPending
  newLaserReducedSize' = nextLaserReducedSizeResearch rsLocked
  newLocked            = if isNothing newLaserReducedSize'
    then Map.empty
    else Map.delete (view lrsrId $ fromJust newLaserReducedSize') rsLocked
  newUnlocked = Map.insert (view lrsrId pend) pend rsUnlocked

researchLaserReducedSize
  :: Seconds
  -> ResearchStatus LaserReducedSizeResearch
  -> ResearchStatus LaserReducedSizeResearch
researchLaserReducedSize secs researchStatus = newResearchStatus
 where
  maybePending      = view pending researchStatus
  numResearchLabs   = fromIntegral $ researchLabCount researchStatus
  newResearchStatus = if isNothing maybePending || numResearchLabs < 1
    then researchStatus
    else newResearchStatus'
  laserReducedSize                 = fromJust maybePending
  (ComponentResearch researchCost) = view lrsrResearchCost laserReducedSize
  (ComponentResearch researchProgress) =
    view lrsrResearchProgress laserReducedSize
  researchRate'           = numResearchLabs * 200 / fromIntegral yearInSeconds
  addedProgress           = researchRate' * fromIntegral secs
  addedProgressPercentage = addedProgress / researchCost
  newProgress             = researchProgress + addedProgressPercentage
  newResearchStatus'      = if newProgress >= 1
    then unlockPendingLaserReducedSizeResearch $ set
      (pending . _Just . lrsrResearchProgress)
      (ComponentResearch 1)
      researchStatus
    else set (pending . _Just . lrsrResearchProgress)
             (ComponentResearch newProgress)
             researchStatus

--
-- Missle Launcher Design
--
-- TODO: This should reflect the order in which the user created the missle launchers instead of research cost
nextMissleLauncherDesign
  :: ResearchMap MissleLauncherDesign -> Maybe MissleLauncherDesign
nextMissleLauncherDesign mlMap = missleLauncherDesign
 where
  missleLauncherDesignList       = Map.toList mlMap
  sortedMissleLauncherDesignList = L.sortBy
    (comparing $ unpack . view mldResearchCost)
    (map snd missleLauncherDesignList)
  missleLauncherDesign = if length sortedMissleLauncherDesignList > 0
    then Just $ head sortedMissleLauncherDesignList
    else Nothing

unlockPendingMissleLauncherDesignResearch
  :: ResearchStatus MissleLauncherDesign -> ResearchStatus MissleLauncherDesign
unlockPendingMissleLauncherDesignResearch rs@(ResearchStatus rsUnlocked rsPending _ rsLocked)
  = newRS
 where
  newRS = if isNothing rsPending
    then rs { _pending = newPending, _locked = newLocked }
    else rs { _pending  = newPending
            , _unlocked = newUnlocked
            , _locked   = newLocked
            }
  newPending =
    if Map.size rsLocked > 0 then newMissleLauncherDesign' else Nothing
  pend                     = fromJust rsPending
  newMissleLauncherDesign' = nextMissleLauncherDesign rsLocked
  newLocked                = if isNothing newMissleLauncherDesign'
    then Map.empty
    else Map.delete (view mldId $ fromJust newMissleLauncherDesign') rsLocked
  newUnlocked = Map.insert (view mldId pend) pend rsUnlocked

researchMissleLauncherDesign
  :: Seconds
  -> ResearchStatus MissleLauncherDesign
  -> ResearchStatus MissleLauncherDesign
researchMissleLauncherDesign secs researchStatus = newResearchStatus
 where
  maybePending      = view pending researchStatus
  numResearchLabs   = fromIntegral $ researchLabCount researchStatus
  newResearchStatus = if isNothing maybePending || numResearchLabs < 1
    then researchStatus
    else newResearchStatus'
  missleLauncherDesign             = fromJust maybePending
  (ComponentResearch researchCost) = view mldResearchCost missleLauncherDesign
  (ComponentResearch researchProgress) =
    view mldResearchProgress missleLauncherDesign
  researchRate'           = numResearchLabs * 200 / fromIntegral yearInSeconds
  addedProgress           = researchRate' * fromIntegral secs
  addedProgressPercentage = addedProgress / researchCost
  newProgress             = researchProgress + addedProgressPercentage
  newResearchStatus'      = if newProgress >= 1
    then unlockPendingMissleLauncherDesignResearch $ set
      (pending . _Just . mldResearchProgress)
      (ComponentResearch 1)
      researchStatus
    else set (pending . _Just . mldResearchProgress)
             (ComponentResearch newProgress)
             researchStatus

--
-- Missle Launcher Size
--
nextMissleLauncherSize
  :: ResearchMap MissleLauncherSizeResearch -> Maybe MissleLauncherSizeResearch
nextMissleLauncherSize lsMap = armor
 where
  launcherSizeList       = Map.toList lsMap
  sortedLauncherSizeList = L.sortBy
    (comparing $ unpack . view mlsrResearchCost)
    (map snd launcherSizeList)
  armor = if length sortedLauncherSizeList > 0
    then Just $ head sortedLauncherSizeList
    else Nothing

unlockPendingMissleLauncherSizeResearch
  :: ResearchStatus MissleLauncherSizeResearch
  -> ResearchStatus MissleLauncherSizeResearch
unlockPendingMissleLauncherSizeResearch rs@(ResearchStatus rsUnlocked rsPending _ rsLocked)
  = newRS
 where
  newRS = if isNothing rsPending
    then rs { _pending = newPending, _locked = newLocked }
    else rs { _pending  = newPending
            , _unlocked = newUnlocked
            , _locked   = newLocked
            }
  newPending = if Map.size rsLocked > 0 then newMissleLauncherSize else Nothing
  pend                  = fromJust rsPending
  newMissleLauncherSize = nextMissleLauncherSize rsLocked
  newLocked             = if isNothing newMissleLauncherSize
    then Map.empty
    else Map.delete (view mlsrId $ fromJust newMissleLauncherSize) rsLocked
  newUnlocked = Map.insert (view mlsrId pend) pend rsUnlocked

researchMissleLauncherSize
  :: Seconds
  -> ResearchStatus MissleLauncherSizeResearch
  -> ResearchStatus MissleLauncherSizeResearch
researchMissleLauncherSize secs researchStatus = newResearchStatus
 where
  maybePending      = view pending researchStatus
  numResearchLabs   = fromIntegral $ researchLabCount researchStatus
  newResearchStatus = if isNothing maybePending || numResearchLabs < 1
    then researchStatus
    else newResearchStatus'
  missleLauncherSize               = fromJust maybePending
  (ComponentResearch researchCost) = view mlsrResearchCost missleLauncherSize
  (ComponentResearch researchProgress) =
    view mlsrResearchProgress missleLauncherSize
  researchRate'           = numResearchLabs * 200 / fromIntegral yearInSeconds
  addedProgress           = researchRate' * fromIntegral secs
  addedProgressPercentage = addedProgress / researchCost
  newProgress             = researchProgress + addedProgressPercentage
  newResearchStatus'      = if newProgress >= 1
    then unlockPendingMissleLauncherSizeResearch $ set
      (pending . _Just . mlsrResearchProgress)
      (ComponentResearch 1)
      researchStatus
    else set (pending . _Just . mlsrResearchProgress)
             (ComponentResearch newProgress)
             researchStatus

--
-- Missle Launcher Reload Rate
--
nextMissleLauncherReloadRate
  :: ResearchMap MissleLauncherReloadRateResearch
  -> Maybe MissleLauncherReloadRateResearch
nextMissleLauncherReloadRate lrrMap = armor
 where
  launcherReloadRateList       = Map.toList lrrMap
  sortedLauncherReloadRateList = L.sortBy
    (comparing $ unpack . view mlrrrResearchCost)
    (map snd launcherReloadRateList)
  armor = if length sortedLauncherReloadRateList > 0
    then Just $ head sortedLauncherReloadRateList
    else Nothing

unlockPendingMissleLauncherReloadRateResearch
  :: ResearchStatus MissleLauncherReloadRateResearch
  -> ResearchStatus MissleLauncherReloadRateResearch
unlockPendingMissleLauncherReloadRateResearch rs@(ResearchStatus rsUnlocked rsPending _ rsLocked)
  = newRS
 where
  newRS = if isNothing rsPending
    then rs { _pending = newPending, _locked = newLocked }
    else rs { _pending  = newPending
            , _unlocked = newUnlocked
            , _locked   = newLocked
            }
  newPending =
    if Map.size rsLocked > 0 then newMissleLauncherReloadRate else Nothing
  pend                        = fromJust rsPending
  newMissleLauncherReloadRate = nextMissleLauncherReloadRate rsLocked
  newLocked                   = if isNothing newMissleLauncherReloadRate
    then Map.empty
    else Map.delete (view mlrrrId $ fromJust newMissleLauncherReloadRate)
                    rsLocked
  newUnlocked = Map.insert (view mlrrrId pend) pend rsUnlocked

researchMissleLauncherReloadRate
  :: Seconds
  -> ResearchStatus MissleLauncherReloadRateResearch
  -> ResearchStatus MissleLauncherReloadRateResearch
researchMissleLauncherReloadRate secs researchStatus = newResearchStatus
 where
  maybePending      = view pending researchStatus
  numResearchLabs   = fromIntegral $ researchLabCount researchStatus
  newResearchStatus = if isNothing maybePending || numResearchLabs < 1
    then researchStatus
    else newResearchStatus'
  missleLauncherReloadRate = fromJust maybePending
  (ComponentResearch researchCost) =
    view mlrrrResearchCost missleLauncherReloadRate
  (ComponentResearch researchProgress) =
    view mlrrrResearchProgress missleLauncherReloadRate
  researchRate'           = numResearchLabs * 200 / fromIntegral yearInSeconds
  addedProgress           = researchRate' * fromIntegral secs
  addedProgressPercentage = addedProgress / researchCost
  newProgress             = researchProgress + addedProgressPercentage
  newResearchStatus'      = if newProgress >= 1
    then unlockPendingMissleLauncherReloadRateResearch $ set
      (pending . _Just . mlrrrResearchProgress)
      (ComponentResearch 1)
      researchStatus
    else set (pending . _Just . mlrrrResearchProgress)
             (ComponentResearch newProgress)
             researchStatus

--
-- Missle Launcher Reduced Size
--
nextMissleLauncherReducedSize
  :: ResearchMap MissleLauncherReducedSizeResearch
  -> Maybe MissleLauncherReducedSizeResearch
nextMissleLauncherReducedSize rlsMap = armor
 where
  launcherReducedSizeList       = Map.toList rlsMap
  sortedLauncherReducedSizeList = L.sortBy
    (comparing $ unpack . view mlrsrResearchCost)
    (map snd launcherReducedSizeList)
  armor = if length sortedLauncherReducedSizeList > 0
    then Just $ head sortedLauncherReducedSizeList
    else Nothing

unlockPendingMissleLauncherReducedSizeResearch
  :: ResearchStatus MissleLauncherReducedSizeResearch
  -> ResearchStatus MissleLauncherReducedSizeResearch
unlockPendingMissleLauncherReducedSizeResearch rs@(ResearchStatus rsUnlocked rsPending _ rsLocked)
  = newRS
 where
  newRS = if isNothing rsPending
    then rs { _pending = newPending, _locked = newLocked }
    else rs { _pending  = newPending
            , _unlocked = newUnlocked
            , _locked   = newLocked
            }
  newPending =
    if Map.size rsLocked > 0 then newMissleLauncherReducedSize else Nothing
  pend                         = fromJust rsPending
  newMissleLauncherReducedSize = nextMissleLauncherReducedSize rsLocked
  newLocked                    = if isNothing newMissleLauncherReducedSize
    then Map.empty
    else Map.delete (view mlrsrId $ fromJust newMissleLauncherReducedSize)
                    rsLocked
  newUnlocked = Map.insert (view mlrsrId pend) pend rsUnlocked

researchMissleLauncherReducedSize
  :: Seconds
  -> ResearchStatus MissleLauncherReducedSizeResearch
  -> ResearchStatus MissleLauncherReducedSizeResearch
researchMissleLauncherReducedSize secs researchStatus = newResearchStatus
 where
  maybePending      = view pending researchStatus
  numResearchLabs   = fromIntegral $ researchLabCount researchStatus
  newResearchStatus = if isNothing maybePending || numResearchLabs < 1
    then researchStatus
    else newResearchStatus'
  missleLauncherReducedSize = fromJust maybePending
  (ComponentResearch researchCost) =
    view mlrsrResearchCost missleLauncherReducedSize
  (ComponentResearch researchProgress) =
    view mlrsrResearchProgress missleLauncherReducedSize
  researchRate'           = numResearchLabs * 200 / fromIntegral yearInSeconds
  addedProgress           = researchRate' * fromIntegral secs
  addedProgressPercentage = addedProgress / researchCost
  newProgress             = researchProgress + addedProgressPercentage
  newResearchStatus'      = if newProgress >= 1
    then unlockPendingMissleLauncherReducedSizeResearch $ set
      (pending . _Just . mlrsrResearchProgress)
      (ComponentResearch 1)
      researchStatus
    else set (pending . _Just . mlrsrResearchProgress)
             (ComponentResearch newProgress)
             researchStatus

--
-- Armor
--
nextArmor :: ResearchMap ArmorResearch -> Maybe ArmorResearch
nextArmor arMap = armor
 where
  armorList = Map.toList arMap
  sortedArmorList =
    L.sortBy (comparing $ unpack . view arResearchCost) (map snd armorList)
  armor =
    if length sortedArmorList > 0 then Just $ head sortedArmorList else Nothing

unlockPendingArmorResearch
  :: ResearchStatus ArmorResearch -> ResearchStatus ArmorResearch
unlockPendingArmorResearch rs@(ResearchStatus rsUnlocked rsPending _ rsLocked)
  = newRS
 where
  newRS = if isNothing rsPending
    then rs { _pending = newPending, _locked = newLocked }
    else rs { _pending  = newPending
            , _unlocked = newUnlocked
            , _locked   = newLocked
            }
  newPending = if Map.size rsLocked > 0 then newArmor else Nothing
  pend       = fromJust rsPending
  newArmor   = nextArmor rsLocked
  newLocked  = if isNothing newArmor
    then Map.empty
    else Map.delete (view arId $ fromJust newArmor) rsLocked
  newUnlocked = Map.insert (view arId pend) pend rsUnlocked

researchArmor
  :: Seconds -> ResearchStatus ArmorResearch -> ResearchStatus ArmorResearch
researchArmor secs researchStatus = newResearchStatus
 where
  maybePending      = view pending researchStatus
  numResearchLabs   = fromIntegral $ researchLabCount researchStatus
  newResearchStatus = if isNothing maybePending || numResearchLabs < 1
    then researchStatus
    else newResearchStatus'
  armor                   = fromJust maybePending
  (ComponentResearch researchCost    ) = view arResearchCost armor
  (ComponentResearch researchProgress) = view arResearchProgress armor
  researchRate'           = numResearchLabs * 200 / fromIntegral yearInSeconds
  addedProgress           = researchRate' * fromIntegral secs
  addedProgressPercentage = addedProgress / researchCost
  newProgress             = researchProgress + addedProgressPercentage
  newResearchStatus'      = if newProgress >= 1
    then unlockPendingArmorResearch $ set
      (pending . _Just . arResearchProgress)
      (ComponentResearch 1)
      researchStatus
    else set (pending . _Just . arResearchProgress)
             (ComponentResearch newProgress)
             researchStatus

--
-- Shields
--
nextShield :: ResearchMap ShieldResearch -> Maybe ShieldResearch
nextShield shrMap = shield
 where
  shieldList = Map.toList shrMap
  sortedShieldList =
    L.sortBy (comparing $ unpack . view shrResearchCost) (map snd shieldList)
  shield = if length sortedShieldList > 0
    then Just $ head sortedShieldList
    else Nothing

unlockPendingShieldResearch
  :: ResearchStatus ShieldResearch -> ResearchStatus ShieldResearch
unlockPendingShieldResearch rs@(ResearchStatus rsUnlocked rsPending _ rsLocked)
  = newRS
 where
  newRS = if isNothing rsPending
    then rs { _pending = newPending, _locked = newLocked }
    else rs { _pending  = newPending
            , _unlocked = newUnlocked
            , _locked   = newLocked
            }
  newPending = if Map.size rsLocked > 0 then newShield else Nothing
  pend       = fromJust rsPending
  newShield  = nextShield rsLocked
  newLocked  = if isNothing newShield
    then Map.empty
    else Map.delete (view shrId $ fromJust newShield) rsLocked
  newUnlocked = Map.insert (view shrId pend) pend rsUnlocked

researchShields
  :: Seconds -> ResearchStatus ShieldResearch -> ResearchStatus ShieldResearch
researchShields secs researchStatus = newResearchStatus
 where
  maybePending      = view pending researchStatus
  numResearchLabs   = fromIntegral $ researchLabCount researchStatus
  newResearchStatus = if isNothing maybePending || numResearchLabs < 1
    then researchStatus
    else newResearchStatus'
  shield                               = fromJust maybePending
  (ComponentResearch researchCost    ) = view shrResearchCost shield
  (ComponentResearch researchProgress) = view shrResearchProgress shield
  researchRate' = numResearchLabs * 200 / fromIntegral yearInSeconds
  addedProgress                        = researchRate' * fromIntegral secs
  addedProgressPercentage              = addedProgress / researchCost
  newProgress = researchProgress + addedProgressPercentage
  newResearchStatus'                   = if newProgress >= 1
    then unlockPendingShieldResearch $ set
      (pending . _Just . shrResearchProgress)
      (ComponentResearch 1)
      researchStatus
    else set (pending . _Just . shrResearchProgress)
             (ComponentResearch newProgress)
             researchStatus

--
-- Fuel Storage
--
nextFuelStorage :: ResearchMap FuelStorageResearch -> Maybe FuelStorageResearch
nextFuelStorage fsrMap = fuelStorage
 where
  fuelStorageList       = Map.toList fsrMap
  sortedFuelStorageList = L.sortBy
    (comparing $ unpack . view fsrResearchCost)
    (map snd fuelStorageList)
  fuelStorage = if length sortedFuelStorageList > 0
    then Just $ head sortedFuelStorageList
    else Nothing

unlockPendingFuelStorageResearch
  :: ResearchStatus FuelStorageResearch -> ResearchStatus FuelStorageResearch
unlockPendingFuelStorageResearch rs@(ResearchStatus rsUnlocked rsPending _ rsLocked)
  = newRS
 where
  newRS = if isNothing rsPending
    then rs { _pending = newPending, _locked = newLocked }
    else rs { _pending  = newPending
            , _unlocked = newUnlocked
            , _locked   = newLocked
            }
  newPending     = if Map.size rsLocked > 0 then newFuelStorage else Nothing
  pend           = fromJust rsPending
  newFuelStorage = nextFuelStorage rsLocked
  newLocked      = if isNothing newFuelStorage
    then Map.empty
    else Map.delete (view fsrId $ fromJust newFuelStorage) rsLocked
  newUnlocked = Map.insert (view fsrId pend) pend rsUnlocked

researchFuelStorage
  :: Seconds
  -> ResearchStatus FuelStorageResearch
  -> ResearchStatus FuelStorageResearch
researchFuelStorage secs researchStatus = newResearchStatus
 where
  maybePending      = view pending researchStatus
  numResearchLabs   = fromIntegral $ researchLabCount researchStatus
  newResearchStatus = if isNothing maybePending || numResearchLabs < 1
    then researchStatus
    else newResearchStatus'
  fuelStorage                          = fromJust maybePending
  (ComponentResearch researchCost    ) = view fsrResearchCost fuelStorage
  (ComponentResearch researchProgress) = view fsrResearchProgress fuelStorage
  researchRate' = numResearchLabs * 200 / fromIntegral yearInSeconds
  addedProgress                        = researchRate' * fromIntegral secs
  addedProgressPercentage              = addedProgress / researchCost
  newProgress = researchProgress + addedProgressPercentage
  newResearchStatus'                   = if newProgress >= 1
    then unlockPendingFuelStorageResearch $ set
      (pending . _Just . fsrResearchProgress)
      (ComponentResearch 1)
      researchStatus
    else set (pending . _Just . fsrResearchProgress)
             (ComponentResearch newProgress)
             researchStatus

--
-- Cargo Handling
--
nextCargoHandling
  :: ResearchMap CargoHandlingResearch -> Maybe CargoHandlingResearch
nextCargoHandling chsrMap = cargoHandling
 where
  cargoHandlingList       = Map.toList chsrMap
  sortedCargoHandlingList = L.sortBy
    (comparing $ unpack . view chsrResearchCost)
    (map snd cargoHandlingList)
  cargoHandling = if length sortedCargoHandlingList > 0
    then Just $ head sortedCargoHandlingList
    else Nothing

unlockPendingCargoHandlingResearch
  :: ResearchStatus CargoHandlingResearch
  -> ResearchStatus CargoHandlingResearch
unlockPendingCargoHandlingResearch rs@(ResearchStatus rsUnlocked rsPending _ rsLocked)
  = newRS
 where
  newRS = if isNothing rsPending
    then rs { _pending = newPending, _locked = newLocked }
    else rs { _pending  = newPending
            , _unlocked = newUnlocked
            , _locked   = newLocked
            }
  newPending       = if Map.size rsLocked > 0 then newCargoHandling else Nothing
  pend             = fromJust rsPending
  newCargoHandling = nextCargoHandling rsLocked
  newLocked        = if isNothing newCargoHandling
    then Map.empty
    else Map.delete (view chsrId $ fromJust newCargoHandling) rsLocked
  newUnlocked = Map.insert (view chsrId pend) pend rsUnlocked

researchCargoHandling
  :: Seconds
  -> ResearchStatus CargoHandlingResearch
  -> ResearchStatus CargoHandlingResearch
researchCargoHandling secs researchStatus = newResearchStatus
 where
  maybePending      = view pending researchStatus
  numResearchLabs   = fromIntegral $ researchLabCount researchStatus
  newResearchStatus = if isNothing maybePending || numResearchLabs < 1
    then researchStatus
    else newResearchStatus'
  cargoHandling                    = fromJust maybePending
  (ComponentResearch researchCost) = view chsrResearchCost cargoHandling
  (ComponentResearch researchProgress) =
    view chsrResearchProgress cargoHandling
  researchRate'           = numResearchLabs * 200 / fromIntegral yearInSeconds
  addedProgress           = researchRate' * fromIntegral secs
  addedProgressPercentage = addedProgress / researchCost
  newProgress             = researchProgress + addedProgressPercentage
  newResearchStatus'      = if newProgress >= 1
    then unlockPendingCargoHandlingResearch $ set
      (pending . _Just . chsrResearchProgress)
      (ComponentResearch 1)
      researchStatus
    else set (pending . _Just . chsrResearchProgress)
             (ComponentResearch newProgress)
             researchStatus

--
-- Cargo Hold
--
nextCargoHold :: ResearchMap CargoHoldResearch -> Maybe CargoHoldResearch
nextCargoHold chrMap = cargoHold
 where
  cargoHoldList = Map.toList chrMap
  sortedCargoHoldList =
    L.sortBy (comparing $ unpack . view chrResearchCost) (map snd cargoHoldList)
  cargoHold = if length sortedCargoHoldList > 0
    then Just $ head sortedCargoHoldList
    else Nothing

unlockPendingCargoHoldResearch
  :: ResearchStatus CargoHoldResearch -> ResearchStatus CargoHoldResearch
unlockPendingCargoHoldResearch rs@(ResearchStatus rsUnlocked rsPending _ rsLocked)
  = newRS
 where
  newRS = if isNothing rsPending
    then rs { _pending = newPending, _locked = newLocked }
    else rs { _pending  = newPending
            , _unlocked = newUnlocked
            , _locked   = newLocked
            }
  newPending   = if Map.size rsLocked > 0 then newCargoHold else Nothing
  pend         = fromJust rsPending
  newCargoHold = nextCargoHold rsLocked
  newLocked    = if isNothing newCargoHold
    then Map.empty
    else Map.delete (view chrId $ fromJust newCargoHold) rsLocked
  newUnlocked = Map.insert (view chrId pend) pend rsUnlocked

researchCargoHold
  :: Seconds
  -> ResearchStatus CargoHoldResearch
  -> ResearchStatus CargoHoldResearch
researchCargoHold secs researchStatus = newResearchStatus
 where
  maybePending      = view pending researchStatus
  numResearchLabs   = fromIntegral $ researchLabCount researchStatus
  newResearchStatus = if isNothing maybePending || numResearchLabs < 1
    then researchStatus
    else newResearchStatus'
  cargoHold                            = fromJust maybePending
  (ComponentResearch researchCost    ) = view chrResearchCost cargoHold
  (ComponentResearch researchProgress) = view chrResearchProgress cargoHold
  researchRate' = numResearchLabs * 200 / fromIntegral yearInSeconds
  addedProgress                        = researchRate' * fromIntegral secs
  addedProgressPercentage              = addedProgress / researchCost
  newProgress = researchProgress + addedProgressPercentage
  newResearchStatus'                   = if newProgress >= 1
    then unlockPendingCargoHoldResearch $ set
      (pending . _Just . chrResearchProgress)
      (ComponentResearch 1)
      researchStatus
    else set (pending . _Just . chrResearchProgress)
             (ComponentResearch newProgress)
             researchStatus

--
-- Jump Gate Construction Modules
--
nextJumpGate :: ResearchMap JumpGateResearch -> Maybe JumpGateResearch
nextJumpGate jgrMap = jumpGate
 where
  jumpGateList = Map.toList jgrMap
  sortedJumpGateList =
    L.sortBy (comparing $ unpack . view jgrResearchCost) (map snd jumpGateList)
  jumpGate = if length sortedJumpGateList > 0
    then Just $ head sortedJumpGateList
    else Nothing

unlockPendingJumpGateResearch
  :: ResearchStatus JumpGateResearch -> ResearchStatus JumpGateResearch
unlockPendingJumpGateResearch rs@(ResearchStatus rsUnlocked rsPending _ rsLocked)
  = newRS
 where
  newRS = if isNothing rsPending
    then rs { _pending = newPending, _locked = newLocked }
    else rs { _pending  = newPending
            , _unlocked = newUnlocked
            , _locked   = newLocked
            }
  newPending  = if Map.size rsLocked > 0 then newJumpGate else Nothing
  pend        = fromJust rsPending
  newJumpGate = nextJumpGate rsLocked
  newLocked   = if isNothing newJumpGate
    then Map.empty
    else Map.delete (view jgrId $ fromJust newJumpGate) rsLocked
  newUnlocked = Map.insert (view jgrId pend) pend rsUnlocked

researchJumpGate
  :: Seconds
  -> ResearchStatus JumpGateResearch
  -> ResearchStatus JumpGateResearch
researchJumpGate secs researchStatus = newResearchStatus
 where
  maybePending      = view pending researchStatus
  numResearchLabs   = fromIntegral $ researchLabCount researchStatus
  newResearchStatus = if isNothing maybePending || numResearchLabs < 1
    then researchStatus
    else newResearchStatus'
  jumpGate                             = fromJust maybePending
  (ComponentResearch researchCost    ) = view jgrResearchCost jumpGate
  (ComponentResearch researchProgress) = view jgrResearchProgress jumpGate
  researchRate' = numResearchLabs * 200 / fromIntegral yearInSeconds
  addedProgress                        = researchRate' * fromIntegral secs
  addedProgressPercentage              = addedProgress / researchCost
  newProgress = researchProgress + addedProgressPercentage
  newResearchStatus'                   = if newProgress >= 1
    then unlockPendingJumpGateResearch $ set
      (pending . _Just . jgrResearchProgress)
      (ComponentResearch 1)
      researchStatus
    else set (pending . _Just . jgrResearchProgress)
             (ComponentResearch newProgress)
             researchStatus

--
-- All Sensors
--
nextSensor :: ResearchMap SensorResearch -> Maybe SensorResearch
nextSensor srMap = sen
 where
  senRList = Map.toList srMap
  sortedSRList =
    L.sortBy (comparing $ unpack . view srResearchCost) (map snd senRList)
  sen = if length sortedSRList > 0 then Just $ head sortedSRList else Nothing

unlockPendingSensorResearch
  :: ResearchStatus SensorResearch -> ResearchStatus SensorResearch
unlockPendingSensorResearch rs@(ResearchStatus rsUnlocked rsPending _ rsLocked)
  = newRS
 where
  newRS = if isNothing rsPending
    then rs { _pending = newPending, _locked = newLocked }
    else rs { _pending  = newPending
            , _unlocked = newUnlocked
            , _locked   = newLocked
            }
  newPending = if Map.size rsLocked > 0 then newSenR else Nothing
  pend       = fromJust rsPending
  newSenR    = nextSensor rsLocked
  newLocked  = if isNothing newSenR
    then Map.empty
    else Map.delete (view srId $ fromJust newSenR) rsLocked
  newUnlocked = Map.insert (view srId pend) pend rsUnlocked

researchSensor
  :: Seconds -> ResearchStatus SensorResearch -> ResearchStatus SensorResearch
researchSensor secs researchStatus = newResearchStatus
 where
  maybePending      = view pending researchStatus
  numResearchLabs   = fromIntegral $ researchLabCount researchStatus
  newResearchStatus = if isNothing maybePending || numResearchLabs < 1
    then researchStatus
    else newResearchStatus'
  sen                     = fromJust maybePending
  (ComponentResearch researchCost    ) = view srResearchCost sen
  (ComponentResearch researchProgress) = view srResearchProgress sen
  researchRate'           = numResearchLabs * 200 / fromIntegral yearInSeconds
  addedProgress           = researchRate' * fromIntegral secs
  addedProgressPercentage = addedProgress / researchCost
  newProgress             = researchProgress + addedProgressPercentage
  newResearchStatus'      = if newProgress >= 1
    then unlockPendingSensorResearch $ set
      (pending . _Just . srResearchProgress)
      (ComponentResearch 1)
      researchStatus
    else set (pending . _Just . srResearchProgress)
             (ComponentResearch newProgress)
             researchStatus

--
-- All Installments
--
nextInstallment :: ResearchMap InstallmentResearch -> Maybe InstallmentResearch
nextInstallment irMap = inst
 where
  instList = Map.toList irMap
  sortedInstList =
    L.sortBy (comparing $ unpack . view irResearchCost) (map snd instList)
  inst =
    if length sortedInstList > 0 then Just $ head sortedInstList else Nothing

unlockPendingInstallmentResearch
  :: ResearchStatus InstallmentResearch -> ResearchStatus InstallmentResearch
unlockPendingInstallmentResearch rs@(ResearchStatus rsUnlocked rsPending _ rsLocked)
  = newRS
 where
  newRS = if isNothing rsPending
    then rs { _pending = newPending, _locked = newLocked }
    else rs { _pending  = newPending
            , _unlocked = newUnlocked
            , _locked   = newLocked
            }
  newPending = if Map.size rsLocked > 0 then newInst else Nothing
  pend       = fromJust rsPending
  newInst    = nextInstallment rsLocked
  newLocked  = if isNothing newInst
    then Map.empty
    else Map.delete (view irId $ fromJust newInst) rsLocked
  newUnlocked = Map.insert (view irId pend) pend rsUnlocked

researchInstallment
  :: Seconds
  -> ResearchStatus InstallmentResearch
  -> ResearchStatus InstallmentResearch
researchInstallment secs researchStatus = newResearchStatus
 where
  maybePending      = view pending researchStatus
  numResearchLabs   = fromIntegral $ researchLabCount researchStatus
  newResearchStatus = if isNothing maybePending || numResearchLabs < 1
    then researchStatus
    else newResearchStatus'
  inst                    = fromJust maybePending
  (ComponentResearch researchCost    ) = view irResearchCost inst
  (ComponentResearch researchProgress) = view irResearchProgress inst
  researchRate'           = numResearchLabs * 200 / fromIntegral yearInSeconds
  addedProgress           = researchRate' * fromIntegral secs
  addedProgressPercentage = addedProgress / researchCost
  newProgress             = researchProgress + addedProgressPercentage
  newResearchStatus'      = if newProgress >= 1
    then unlockPendingInstallmentResearch $ set
      (pending . _Just . irResearchProgress)
      (ComponentResearch 1)
      researchStatus
    else set (pending . _Just . irResearchProgress)
             (ComponentResearch newProgress)
             researchStatus
