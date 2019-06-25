module Nebula4x.Ship where

import           Control.Lens                  as L
import           Control.Newtype.Generics
                                         hiding ( over )
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           System.Random

import           Nebula4x.Component.Armor
import           Nebula4x.Component.Engine
import           Nebula4x.Component.FuelStorage
import           Nebula4x.Component.Laser
import           Nebula4x.Component.MissleLauncher
import           Nebula4x.Component.Sensor
import           Nebula4x.Component.Shield
import           Nebula4x.Component.Transport

import           Nebula4x.Mineral
import           Nebula4x.Types
import           Nebula4x.Utils

-- Ship Helper Functions
-- Should definitely clean this up.
newShipDesign
  :: ShipDesignId
  -> ShipName
  -> [Armor]
  -> [Shield]
  -> [Engine]
  -> [MissleLauncher]
  -> [Laser]
  -> [FuelStorage]
  -> [CargoHandlingComponent]
  -> [CargoHoldComponent]
  -> [JumpGateComponent]
  -> [Sensor]
  -> [Sensor]
  -> ShipDesign
newShipDesign sdid nm armLayers shlds es mls lzs fs chc ch jg geos gravs =
  newShipDesign'
 where
  newShipDesign' = ShipDesign sdid
                              nm
                              armLayers
                              shlds
                              sz
                              spd
                              rng
                              cls
                              es
                              mls
                              lzs
                              fs
                              chc
                              ch
                              jg
                              geos
                              gravs
                              cst
  sz    = pack $ shipSize
  speed = power / shipSize * 1000
  spd   = pack speed
  rng   = pack $ totalFuelStorage fs / engineFE * 60 * 60 * speed
  cls   = shipClass es mls gravs
  cst   = Map.filter (> 0) buildMineralCost
  sSize =
    enginesSize es
      + missleLaunchersSize mls
      + lasersSize lzs
      + shieldsSize shlds
      + fuelStoragesSize fs
      + cargoHandlersSize chc
      + cargoHoldsSize ch
      + jumpGatesSize jg
      + geologicalSensorsSize geos
      + gravitationalSensorsSize gravs
  (ShipSize shipSize, armTiles) =
    shipSizeWithArmor armLayers (ShipSize sSize) 0 0
  power              = enginesPower es
  engineFE           = enginesEfficiency es
  eBuildCost         = enginesCost es
  eBuildMineralCost  = Map.map (* eBuildCost) engineMineralCostRatio
  mlBuildCost        = missleLaunchersCost mls
  mlBuildMineralCost = Map.map (* mlBuildCost) missleLauncherMineralCostRatio
  lzBuildCost        = lasersCost lzs
  lzBuildMineralCost = Map.map (* lzBuildCost) laserMineralCostRatio
  aBuildMineralCost  = Map.map (* fromIntegral armTiles) armorMineralCostRatio
  sBuildCost         = shieldsCost shlds
  sBuildMineralCost  = Map.map (* sBuildCost) shieldMineralCostRatio
  fsBuildCost        = fuelStoragesCost fs
  fsBuildMineralCost = Map.map (* fsBuildCost) fuelStorageMineralCostRatio
  cHandlersBuildCost = cargoHandlersCost chc
  cHandlersBuildMineralCost =
    Map.map (* cHandlersBuildCost) cargoHandlingMineralCostRatio
  cHoldsBuildCost = cargoHoldsCost ch
  cHoldsBuildMineralCost =
    Map.map (* cHoldsBuildCost) cargoHoldMineralCostRatio
  jumpGatesBuildCost = jumpGatesCost jg
  jumpGatesBuildMineralCost =
    Map.map (* jumpGatesBuildCost) jumpGateMineralCostRatio
  geoBuildCost = geologicalSensorsCost geos
  geoBuildMineralCost =
    Map.map (* geoBuildCost) geologicalSensorMineralCostRatio
  gravBuildCost = gravitationalSensorsCost gravs
  gravBuildMineralCost =
    Map.map (* gravBuildCost) gravitationalSensorMineralCostRatio
  buildMineralCost = Map.unionsWith
    (+)
    [ eBuildMineralCost
    , mlBuildMineralCost
    , lzBuildMineralCost
    , aBuildMineralCost
    , sBuildMineralCost
    , fsBuildMineralCost
    , cHandlersBuildMineralCost
    , cHoldsBuildMineralCost
    , jumpGatesBuildMineralCost
    , geoBuildMineralCost
    , gravBuildMineralCost
    ]

newShip :: ShipId -> RaceId -> ShipName -> ShipLocation -> ShipDesign -> Ship
newShip sid race name location design = Ship sid
                                             race
                                             name
                                             location
                                             (ShipFuel 0)
                                             (shipMissleLaunchers design)
                                             (shipLasers design)
                                             (shipArmor design)
                                             (shipShields design)
                                             Nothing
                                             []
                                             Nothing
                                             []
                                             emptyCargo
                                             design

shipMissleLaunchers :: ShipDesign -> Maybe ShipMissleLaunchers
shipMissleLaunchers design = maybeShipMissleLaunchers where
  maybeShipMissleLaunchers = if numMissleLaunchers > 0
    then Just (ShipMissleLaunchers reloadRate reloadStatus missleSalvo)
    else Nothing
  numMissleLaunchers  = length missleLaunchers
  missleLaunchers     = view sdMissleLaunchers design
  firstMissleLauncher = missleLaunchers !! 0
  reloadStatus        = (ComponentRating 1)
  reloadRate          = view mlReloadRate firstMissleLauncher
  missleStrengths     = replicate numMissleLaunchers
    $ view (mlMissle . missStrength) firstMissleLauncher
  missleSalvo = MissleSalvo missleStrengths
                            (view (mlMissle . missRange) firstMissleLauncher)
                            (MissleSalvoLocation 0 0)
                            (view (mlMissle . missSpeed) firstMissleLauncher)
                            noTarget

missleSalvoFromShip :: Ship -> MissleSalvoTarget -> (Ship, Maybe MissleSalvo)
missleSalvoFromShip shp sTarget = (newShp, maybeMissleSalvo) where
  maybeShipMissleLaunchers   = view sShipMissleLaunchers shp
  (newShp, maybeMissleSalvo) = if isNothing maybeShipMissleLaunchers
    then (shp, Nothing)
    else reloadStatusCheck
  (ComponentRating reloadStatus) =
    view smlReloadStatus (fromJust maybeShipMissleLaunchers)
  reloadStatusCheck =
    if reloadStatus < 1 then (shp, Nothing) else (shp', Just newMissleSalvo)
  shp' = shp { _sShipMissleLaunchers = Just newShipMissleLaunchers }
  newShipMissleLaunchers =
    set smlReloadStatus (ComponentRating 0) (fromJust maybeShipMissleLaunchers)
  missleSalvo          = view smlSalvo newShipMissleLaunchers
  newMissleSalvo'      = set msLocation salvoLocation missleSalvo
  newMissleSalvo       = set msTarget sTarget newMissleSalvo'
  salvoLocation        = MissleSalvoLocation sX sY
  (ShipLocation sX sY) = view sLocation shp

-- Assumed to have lasers on board within range of enemy ship.
shootShipLasersAtShip
  :: RandomGen g => g -> Ship -> Ship -> (Ship, Maybe Ship, g)
shootShipLasersAtShip gen shp target = (newShp, maybeTarget, newGen) where
  maybeShipLasers       = view sShipLasers shp
  (newShp, maybeTarget) = if isNothing maybeShipLasers || numLasers < 1
    then (shp, Just target)
    else rechargeStatusCheck
  shipLasers'                      = fromJust maybeShipLasers
  (ComponentRating rechargeStatus) = view slRechargeStatus shipLasers'
  numLasers                        = length lasers
  lasers                           = view slLasers shipLasers'
  rechargeStatusCheck =
    if rechargeStatus < 1 then (shp, Just target) else (shp', maybeTarget')
  shp'                   = shp { _sShipLasers = Just newShipLasers }
  newShipLasers          = set slRechargeStatus (ComponentRating 0) shipLasers'
  (maybeTarget', newGen) = lasersAttackShip gen target lasers

shipLasers :: ShipDesign -> Maybe ShipLasers
shipLasers design = maybeShipLasers where
  maybeShipLasers = if numLasers > 0
    then Just (ShipLasers rechargeRate rechargeStatus lasers)
    else Nothing
  numLasers      = length lasers
  lasers         = view sdLasers design
  firstLaser     = lasers !! 0
  rechargeStatus = (ComponentRating 1)
  rechargeRate   = view lRechargeRate firstLaser

shipArmor :: ShipDesign -> ShipArmor
shipArmor design = newShipArmor where
  newShipArmor = ShipArmor armor grid
  armor = if length armorLayers < 1 then conventionalArmor else firstLayer
  firstLayer   = armorLayers !! 0
  armorLayers  = view sdArmor design
  numLayers    = length armorLayers
  numColumns   = shipArmorColumns (unpack $ view sdSize design)
  grid         = replicate numLayers (replicate numColumns ArmorIntact)

shipShields :: ShipDesign -> Maybe ShipShields
shipShields design = newShipShields where
  newShipShields = if length shields > 0
    then Just $ ShipShields False currentCapacity maxCapacity regenRate fuelRate
    else Nothing
  shields         = view sdShields design
  currentCapacity = maxCapacity
  maxCapacity     = ComponentRating maxCap
  maxCap          = shieldsCapacity shields
  regenRate       = ComponentRating (maxCap / 300) -- 5 minutes to fully recharge
  fuelRate        = ComponentRating (maxCap * 10 / 3600) -- Fuel consumption set to 10x rating per hour

enableShipShields :: Ship -> Ship
enableShipShields shp = newShp where
  newShp         = shp { _sShields = newShields }
  currentShields = view sShields shp
  newShields     = if isNothing currentShields
    then currentShields
    else Just $ set ssEnabled True (fromJust currentShields)

shipOptimalAttackRange :: Ship -> Double
shipOptimalAttackRange shp = optimalRange where
  maybeMissleLaunchers = view sShipMissleLaunchers shp
  maybeShipLasers      = view sShipLasers shp
  missleRange          = if isNothing maybeMissleLaunchers
    then -1
    else unpack $ view (smlSalvo . msRange) (fromJust maybeMissleLaunchers)
  laserRange = if isJust maybeShipLasers && length lasers > 0
    then unpack $ view lRange firstLaser
    else -1
  shipLasers'  = fromJust maybeShipLasers
  lasers       = view slLasers shipLasers'
  firstLaser   = lasers !! 0
  ranges       = filter ((<) 0) [missleRange, laserRange]
  optimalRange = if length ranges > 0 then minimum ranges else 0

getNearestEnemyShip :: Ship -> StarSystem -> Maybe Ship
getNearestEnemyShip ship system = maybeShip where
  systemShips = view ssShips system
  sid         = view sId ship
  maybeShip   = Map.foldl (selectNearestShipToOurs ship)
                          Nothing
                          (Map.delete sid systemShips)

-- Cache the distance values in the accumulator so we don't need to recalculate closestShipDistance every fold.
selectNearestShipToOurs :: Ship -> Maybe Ship -> Ship -> Maybe Ship
selectNearestShipToOurs _ Nothing newShipToCheck = Just newShipToCheck
selectNearestShipToOurs ourShip (Just closestShip) newShipToCheck =
  newMaybeClosestShip where
  newMaybeClosestShip = if closestShipDistance > newShipDistance
    then Just newShipToCheck
    else Just closestShip
  (ShipLocation ourX ourY) = view sLocation ourShip
  (ShipLocation csdX csdY) = view sLocation closestShip
  (ShipLocation nsdX nsdY) = view sLocation newShipToCheck
  closestShipDistance      = sqrt $ ((csdX - ourX) ** 2) + ((csdY - ourY) ** 2)
  newShipDistance          = sqrt $ ((nsdX - ourX) ** 2) + ((nsdY - ourY) ** 2)

completeOrder :: Bool -> Ship -> Ship
completeOrder True shp@(Ship _ _ _ _ _ _ _ _ _ _ _ (Just _) _ _ _) =
  shp { _sCurrentConditionalOrder = Nothing }
completeOrder True shp = shp
completeOrder False shp@(Ship _ _ _ _ _ _ _ _ _ _ [] _ _ _ _) =
  shp { _sCurrentOrder = Nothing }
completeOrder False shp@(Ship _ _ _ _ _ _ _ _ _ _ (og : ogs) _ _ _ _) = newShp
 where
  newShp = shp { _sCurrentOrder = newOrder, _sOrderGroups = newOrderGroups }
  (maybeNewOrder, maybeOrderGroup) = nextOrderFromGroup og
  (newOrder, newOrderGroups) = if isNothing maybeOrderGroup
    then (nextOrder, ogs)
    else (maybeNewOrder, fromJust maybeOrderGroup : ogs)
  nextOrder = if length ogs > 0 && length (view sogOrders (ogs !! 0)) > 0
    then Just $ (view sogOrders (ogs !! 0)) !! 0
    else Nothing

nextOrderFromGroup :: ShipOrderGroup -> (Maybe ShipOrder, Maybe ShipOrderGroup)
nextOrderFromGroup (ShipOrderGroup [] _ _) = (Nothing, Nothing)
nextOrderFromGroup (ShipOrderGroup orders repeatCnt currentIdx) =
  (maybeOrder, maybeOrderGroup)
 where
  ordersLength = length orders
  loopBack     = currentIdx >= ordersLength - 1
  (newIdx, newRepeat) =
    if loopBack then (0, repeatCnt - 1) else (currentIdx + 1, repeatCnt)
  order                         = orders !! newIdx
  (maybeOrder, maybeOrderGroup) = if newRepeat < 1
    then (Nothing, Nothing)
    else (Just order, Just $ ShipOrderGroup orders newRepeat newIdx)

addShipOrder :: ShipOrder -> Ship -> Ship
addShipOrder so shp@(Ship _ _ _ _ _ _ _ _ _ Nothing [] _ _ _ _) =
  shp { _sCurrentOrder = Just so, _sOrderGroups = [ShipOrderGroup [so] 1 0] }
addShipOrder so shp = addShipOrderGroup (ShipOrderGroup [so] 1 0) shp

addShipConditionalOrder :: OrderCondition -> ShipOrder -> Ship -> Ship
addShipConditionalOrder oc so =
  over sConditionalOrders ((++) [ShipConditionalOrder oc so])

addShipOrderGroup :: ShipOrderGroup -> Ship -> Ship
addShipOrderGroup sog ship =
  ship { _sOrderGroups = view sOrderGroups ship ++ [sog] }

addOrderToGroup :: ShipOrder -> ShipOrderGroup -> ShipOrderGroup
addOrderToGroup so = over sogOrders (flip (++) [so])

addShipOrderToGroup :: ShipOrder -> Int -> Ship -> Ship
addShipOrderToGroup so _ shp@(Ship _ _ _ _ _ _ _ _ _ Nothing [] _ _ _ _) =
  addShipOrder so shp
addShipOrderToGroup so idx shp@(Ship _ _ _ _ _ _ _ _ _ _ ogs _ _ _ _) = newShp
 where
  newShp = if length ogs <= idx
    then addShipOrder so shp
    else set sOrderGroups newOrderGroups shp
  newOrderGroup  = addOrderToGroup so (ogs !! idx)
  newOrderGroups = ogs & element idx .~ newOrderGroup

cancelShipOrder :: Ship -> Ship
cancelShipOrder shp = shp { _sCurrentOrder = Nothing }

removeShipOrderGroup :: Int -> Ship -> Ship
removeShipOrderGroup _   shp@(Ship _ _ _ _ _ _ _ _ _ _ []     _ _ _ _) = shp
removeShipOrderGroup idx shp@(Ship _ _ _ _ _ _ _ _ _ _ orders _ _ _ _) = newShp
 where
  newShp = if idx == 0
    then shp { _sCurrentOrder = newOrder, _sOrderGroups = newOrderGroups }
    else if idx >= length orders
      then shp { _sOrderGroups = newOrderGroups }
      else over sOrderGroups (filter $ (/=) (orders !! idx)) shp
  newOrderGroups        = tail orders
  firstOrderGroup       = head newOrderGroups
  firstOrderGroupOrders = view sogOrders firstOrderGroup
  newOrder = if length newOrderGroups > 0 && length firstOrderGroupOrders > 0
    then Just $ head firstOrderGroupOrders
    else Nothing

removeOrderFromGroup :: Int -> ShipOrderGroup -> ShipOrderGroup
removeOrderFromGroup _   sog@(ShipOrderGroup []     _ _) = sog
removeOrderFromGroup idx sog@(ShipOrderGroup orders _ _) = newShipOrderGroup
 where
  newShipOrderGroup = if idx >= length orders
    then sog { _sogOrders = tail orders }
    else over sogOrders (filter $ (/=) (orders !! idx)) sog

removeShipOrderFromGroup :: Int -> Int -> Ship -> Ship
removeShipOrderFromGroup ogIdx oIdx shp@(Ship _ _ _ _ _ _ _ _ _ _ ogs _ _ _ _)
  = newShp
 where
  newShp = if ogIdx >= length ogs
    then shp
    else if length newOrderGroups < 1
      then shp { _sOrderGroups = [], _sCurrentOrder = Nothing }
      else shp { _sOrderGroups = newOrderGroups }
  newOrderGroup  = removeOrderFromGroup oIdx (ogs !! ogIdx)
  newOrderGroups = ogs & element ogIdx .~ newOrderGroup

removeShipConditionalOrder :: Int -> Ship -> Ship
removeShipConditionalOrder idx shp = newShp where
  currentConditionalOrders = view sConditionalOrders shp
  toBeRemoved              = currentConditionalOrders !! idx
  newShp                   = if length currentConditionalOrders > idx
    then over sConditionalOrders (filter $ (/=) toBeRemoved)
      $ shp { _sCurrentConditionalOrder = Nothing }
    else shp

updateOrderGroupRepeat :: Int -> Int -> Ship -> Ship
updateOrderGroupRepeat _ _ shp@(Ship _ _ _ _ _ _ _ _ _ _ [] _ _ _ _) = shp
updateOrderGroupRepeat ogIdx newRepeat shp@(Ship _ _ _ _ _ _ _ _ _ _ ogs _ _ _ _)
  = newShp
 where
  newShp =
    if ogIdx >= length ogs then shp else shp { _sOrderGroups = newOrderGroups }
  newOrderGroup  = set sogRepeat newRepeat (ogs !! ogIdx)
  newOrderGroups = ogs & element ogIdx .~ newOrderGroup

shipClass :: [Engine] -> [MissleLauncher] -> [Sensor] -> ShipClass
shipClass engs launchers gravs = shipCls
 where
  shipCls = if length launchers > 0 || militaryEngines > 0 || length gravs > 0
    then MilitaryClass
    else CommercialClass
  militaryEngines =
    length $ filter (\engine -> view eType engine == MilitaryEngine) engs

enginesPower :: [Engine] -> Double
enginesPower = sum . map (unpack . view eRating)

enginesSize :: [Engine] -> Double
enginesSize = sum . map (unpack . view eSize)

enginesCost :: [Engine] -> Double
enginesCost = sum . map (unpack . view eCost)

enginesEfficiency :: [Engine] -> Double
enginesEfficiency = sum . map (unpack . view eEfficiency)

missleLaunchersSize :: [MissleLauncher] -> Double
missleLaunchersSize = sum . map (unpack . view mlSize)

missleLaunchersCost :: [MissleLauncher] -> Double
missleLaunchersCost = sum . map (unpack . view mlCost)

lasersSize :: [Laser] -> Double
lasersSize = sum . map (unpack . view lSize)

lasersCost :: [Laser] -> Double
lasersCost = sum . map (unpack . view lCost)

fuelStoragesSize :: [FuelStorage] -> Double
fuelStoragesSize = sum . map (unpack . view fsSize)

fuelStoragesCost :: [FuelStorage] -> Double
fuelStoragesCost = sum . map (unpack . view fsCost)

totalFuelStorage :: [FuelStorage] -> Double
totalFuelStorage = sum . map (unpack . view fsRating)

-- Recursively call this until ship size stabalizes.
-- TODO: Need to be smarter about calculating armor
-- For now, keep checking if more columns are needed as ship weight increases with armor additions.
shipSizeWithArmor :: [Armor] -> ShipSize -> Double -> Int -> (ShipSize, Int)
shipSizeWithArmor layers os@(ShipSize originalSize) tileSize armorColumns =
  (shipSize, numTiles) where
  (shipSize, numTiles) = if newArmorColumns > armorColumns
    then shipSizeWithArmor layers os newTileSize newArmorColumns
    else (ShipSize newShipSize, nTiles)
  newShipSize     = originalSize + newTileSize
  newTileSize     = fromIntegral nTiles / armorRating
  nTiles          = numRows * newArmorColumns
  numRows         = length layers
  newArmorColumns = shipArmorColumns (originalSize + tileSize)
  (ComponentRating armorRating) =
    if numRows > 0 then view aRating (layers !! 0) else (ComponentRating 1)

-- https://www.reddit.com/r/aurora/comments/43fw1i/is_it_better_to_focus_on_armor_or_shields_or_both
shipArmorColumns :: Double -> Int
shipArmorColumns hs = floor $ 1.2 * (hs ** (2 / 3))

shieldsSize :: [Shield] -> Double
shieldsSize = fromIntegral . length

shieldsCost :: [Shield] -> Double
shieldsCost = (* 4) . sum . map (unpack . view shRating)

shieldsCapacity :: [Shield] -> Double
shieldsCapacity = sum . map (unpack . view shRating)

cargoHandlersSize :: [CargoHandlingComponent] -> Double
cargoHandlersSize = sum . map (unpack . view cHandleSize)

cargoHandlersCost :: [CargoHandlingComponent] -> Double
cargoHandlersCost = sum . map (unpack . view cHandleCost)

cargoHandlersMultiplier :: [CargoHandlingComponent] -> Double
cargoHandlersMultiplier = sum . map (unpack . view cHandleRating)

shipCargoHandlingMultiplier :: Ship -> Double
shipCargoHandlingMultiplier =
  cargoHandlersMultiplier . view (sDesign . sdCargoHandlingSystems)

cargoHoldsSize :: [CargoHoldComponent] -> Double
cargoHoldsSize = sum . map (unpack . view cHoldSize)

cargoHoldsCapacity :: [CargoHoldComponent] -> Double
cargoHoldsCapacity = sum . map (unpack . view cHoldRating)

cargoHoldsCost :: [CargoHoldComponent] -> Double
cargoHoldsCost = sum . map (unpack . view cHoldCost)

jumpGatesSize :: [JumpGateComponent] -> Double
jumpGatesSize = sum . map (unpack . view jgSize)

jumpGatesCost :: [JumpGateComponent] -> Double
jumpGatesCost = sum . map (unpack . view jgCost)

geologicalSensorsSize :: [Sensor] -> Double
geologicalSensorsSize = sum . map (unpack . view senSize)

geologicalSensorsCost :: [Sensor] -> Double
geologicalSensorsCost = sum . map (unpack . view senCost)

gravitationalSensorsSize :: [Sensor] -> Double
gravitationalSensorsSize = geologicalSensorsSize

gravitationalSensorsCost :: [Sensor] -> Double
gravitationalSensorsCost = sum . map (unpack . view senCost)

cargoSpaceTaken :: Ship -> Double
cargoSpaceTaken ship = takenUpSpace
 where
  takenUpSpace =
    (unpack installmentsCargoSize) + mineralStacksCargoSize mineralStacks
  installments          = view (sCargo . scInstallments) ship
  mineralStacks         = view (sCargo . scMinerals) ship
  installmentsCargoSize = InstallmentSize totalSize
  totalSize             = Map.foldr tallyInstallmentSize 0 installments
  tallyInstallmentSize instStack count =
    unpack (installmentStackCargoSize instStack) + count
  installmentStackCargoSize (InstallmentStack (InstallmentCount count) inst) =
    InstallmentSize (count * (unpack $ view iSize inst))

availableCargoSpace :: Ship -> Double
availableCargoSpace ship = totalCapacity - cargoSpaceTaken ship
  where totalCapacity = cargoHoldsCapacity $ view (sDesign . sdCargoHolds) ship

surfaceArea :: Double -> Double
surfaceArea radius = 4 * pi * (radius ** 2)

getShipActiveOrder :: Ship -> Maybe ShipOrder
getShipActiveOrder shp = maybeOrder where
  maybeCurrentOrder            = view sCurrentOrder shp
  maybeCurrentConditionalOrder = view sCurrentConditionalOrder shp
  maybeOrder                   = if isJust maybeCurrentConditionalOrder
    then Just $ view scoOrder (fromJust maybeCurrentConditionalOrder)
    else maybeCurrentOrder

replaceShipActiveOrder :: ShipOrder -> Ship -> Ship
replaceShipActiveOrder so shp = newShp where
  maybeCurrentConditionalOrder = view sCurrentConditionalOrder shp
  condition = view scoCondition (fromJust maybeCurrentConditionalOrder)
  newShp = if isJust maybeCurrentConditionalOrder
    then set sCurrentConditionalOrder
             (Just $ ShipConditionalOrder condition so)
             shp
    else set sCurrentOrder (Just so) shp

setArrivedOrbitOrder :: Ship -> Ship
setArrivedOrbitOrder ship = newShip'
 where
  maybeOrder = getShipActiveOrder ship
  newShip'   = case maybeOrder of
    (Just (OrbitOrder bid False)) ->
      replaceShipActiveOrder (OrbitOrder bid True) ship
    _ -> ship

isOrbiting :: Ship -> Bool
isOrbiting (Ship _ _ _ _ _ _ _ _ _ (Just (OrbitOrder _ arrived)) _ _ _ _ _) =
  arrived
isOrbiting (Ship _ _ _ _ _ _ _ _ _ _ _ (Just (ShipConditionalOrder _ (OrbitOrder _ arrived))) _ _ _)
  = arrived
isOrbiting _ = False

setPickupMineralsInProgress :: Ship -> Ship
setPickupMineralsInProgress ship = newShip'
 where
  maybeOrder = getShipActiveOrder ship
  newShip'   = case maybeOrder of
    (Just (PickupMineralsOrder bid mins False)) ->
      replaceShipActiveOrder (PickupMineralsOrder bid mins True) ship
    _ -> ship

isPickingUpMinerals :: Ship -> Bool
isPickingUpMinerals (Ship _ _ _ _ _ _ _ _ _ (Just (PickupMineralsOrder _ _ inProg)) _ _ _ _ _)
  = inProg
isPickingUpMinerals (Ship _ _ _ _ _ _ _ _ _ _ _ (Just (ShipConditionalOrder _ (PickupMineralsOrder _ _ inProg))) _ _ _)
  = inProg
isPickingUpMinerals _ = False

setDropoffMineralsInProgress :: Ship -> Ship
setDropoffMineralsInProgress ship = newShip'
 where
  maybeOrder = getShipActiveOrder ship
  newShip'   = case maybeOrder of
    (Just (DropoffMineralsOrder bid mins False)) ->
      replaceShipActiveOrder (DropoffMineralsOrder bid mins True) ship
    _ -> ship

isDroppingOffMinerals :: Ship -> Bool
isDroppingOffMinerals (Ship _ _ _ _ _ _ _ _ _ (Just (DropoffMineralsOrder _ _ inProg)) _ _ _ _ _)
  = inProg
isDroppingOffMinerals (Ship _ _ _ _ _ _ _ _ _ _ _ (Just (ShipConditionalOrder _ (DropoffMineralsOrder _ _ inProg))) _ _ _)
  = inProg
isDroppingOffMinerals _ = False

setPickupInstallmentInProgress :: Ship -> Ship
setPickupInstallmentInProgress ship = newShip'
 where
  maybeOrder = getShipActiveOrder ship
  newShip'   = case maybeOrder of
    (Just (PickupInstallmentOrder bid inst prog False)) ->
      replaceShipActiveOrder (PickupInstallmentOrder bid inst prog True) ship
    _ -> ship

isPickingUpInstallment :: Ship -> Bool
isPickingUpInstallment (Ship _ _ _ _ _ _ _ _ _ (Just (PickupInstallmentOrder _ _ _ inProg)) _ _ _ _ _)
  = inProg
isPickingUpInstallment (Ship _ _ _ _ _ _ _ _ _ _ _ (Just (ShipConditionalOrder _ (PickupInstallmentOrder _ _ _ inProg))) _ _ _)
  = inProg
isPickingUpInstallment _ = False

setDropoffInstallmentInProgress :: Ship -> Ship
setDropoffInstallmentInProgress ship = newShip'
 where
  maybeOrder = getShipActiveOrder ship
  newShip'   = case maybeOrder of
    (Just (DropoffInstallmentOrder bid inst prog False)) ->
      replaceShipActiveOrder (DropoffInstallmentOrder bid inst prog True) ship
    _ -> ship

isDroppingOffInstallment :: Ship -> Bool
isDroppingOffInstallment (Ship _ _ _ _ _ _ _ _ _ (Just (DropoffInstallmentOrder _ _ _ inProg)) _ _ _ _ _)
  = inProg
isDroppingOffInstallment (Ship _ _ _ _ _ _ _ _ _ _ _ (Just (ShipConditionalOrder _ (DropoffInstallmentOrder _ _ _ inProg))) _ _ _)
  = inProg
isDroppingOffInstallment _ = False

setShipRefueldAtBody :: Ship -> Body -> (Body, Ship)
setShipRefueldAtBody shp body = fuelShipWithBody body shp

setShipRefueldAtShip :: Ship -> Ship -> (Ship, Ship)
setShipRefueldAtShip shp shipWithFuel = fuelShipWithShip shipWithFuel shp

setShipUnfueldAtBody :: Ship -> Body -> (Body, Ship)
setShipUnfueldAtBody s@(Ship _ _ _ _ _ _ _ _ _ (Just (TransferFuelToBody _ (ShipFuel amount))) _ _ _ _ _) body
  = unfuelShipWithBody body s amount
setShipUnfueldAtBody s@(Ship _ _ _ _ _ _ _ _ _ _ _ (Just (ShipConditionalOrder _ (TransferFuelToBody _ (ShipFuel amount)))) _ _ _) body
  = unfuelShipWithBody body s amount
setShipUnfueldAtBody shp bdy = (bdy, shp)

setShipUnfueldAtShip :: Ship -> Ship -> (Ship, Ship)
setShipUnfueldAtShip s@(Ship _ _ _ _ _ _ _ _ _ (Just (TransferFuelToShip _ (ShipFuel amount))) _ _ _ _ _) shipRequiringFuel
  = fuelShipWithShipAmount s shipRequiringFuel amount
setShipUnfueldAtShip s@(Ship _ _ _ _ _ _ _ _ _ _ _ (Just (ShipConditionalOrder _ (TransferFuelToShip _ (ShipFuel amount)))) _ _ _) shipRequiringFuel
  = fuelShipWithShipAmount s shipRequiringFuel amount
setShipUnfueldAtShip tanker ship = (tanker, ship)

fuelShipWithBody :: Body -> Ship -> (Body, Ship)
fuelShipWithBody bdy ship = (newBody, newShip')
 where
  (FuelReserves availableFuel  ) = view bFuelReserves bdy
  (ShipFuel     currentShipFuel) = view sFuel ship
  shipMaxFuel = totalFuelStorage $ view (sDesign . sdFuelStorages) ship
  missingFuel                    = shipMaxFuel - currentShipFuel
  transferedFuel                 = minimum [missingFuel, availableFuel]
  newAvailableFuel               = availableFuel - transferedFuel
  newShipFuel                    = currentShipFuel + transferedFuel
  newBody = set bFuelReserves (FuelReserves newAvailableFuel) bdy
  newShip'                       = set sFuel (ShipFuel newShipFuel) ship

fuelShipWithShip :: Ship -> Ship -> (Ship, Ship)
fuelShipWithShip shipWithFuel ship = (newShipWithFuel, newShip')
 where
  (ShipFuel availableFuel  ) = view sFuel shipWithFuel
  (ShipFuel currentShipFuel) = view sFuel ship
  shipMaxFuel = totalFuelStorage $ view (sDesign . sdFuelStorages) ship
  missingFuel                = shipMaxFuel - currentShipFuel
  transferedFuel             = minimum [missingFuel, availableFuel]
  newAvailableFuel           = availableFuel - transferedFuel
  newShipFuel                = currentShipFuel + transferedFuel
  newShipWithFuel = set sFuel (ShipFuel newAvailableFuel) shipWithFuel
  newShip'                   = set sFuel (ShipFuel newShipFuel) ship

unfuelShipWithBody :: Body -> Ship -> Double -> (Body, Ship)
unfuelShipWithBody bdy ship amount = (newBody, newShip')
 where
  (FuelReserves availableFuel  ) = view bFuelReserves bdy
  (ShipFuel     currentShipFuel) = view sFuel ship
  transferedFuel                 = minimum [amount, currentShipFuel]
  newAvailableFuel               = availableFuel + transferedFuel
  newShipFuel                    = currentShipFuel - transferedFuel
  newBody = set bFuelReserves (FuelReserves newAvailableFuel) bdy
  newShip'                       = set sFuel (ShipFuel newShipFuel) ship

fuelShipWithShipAmount :: Ship -> Ship -> Double -> (Ship, Ship)
fuelShipWithShipAmount shipWithFuel ship amount = (newShipWithFuel, newShip')
 where
  (ShipFuel availableFuel  ) = view sFuel shipWithFuel
  (ShipFuel currentShipFuel) = view sFuel ship
  shipMaxFuel = totalFuelStorage $ view (sDesign . sdFuelStorages) ship
  missingFuel                = shipMaxFuel - currentShipFuel
  transferedFuel             = minimum [amount, missingFuel, availableFuel]
  newAvailableFuel           = availableFuel - transferedFuel
  newShipFuel                = currentShipFuel + transferedFuel
  newShipWithFuel = set sFuel (ShipFuel newAvailableFuel) shipWithFuel
  newShip'                   = set sFuel (ShipFuel newShipFuel) ship

getNearestBody :: (Body -> Bool) -> ShipLocation -> StarSystem -> Maybe Body
getNearestBody filterFunc shpLoc system =
  Map.foldl (selectNearestBody filterFunc shpLoc) Nothing (view ssBodies system)

getNearestGravAnomalyToShip :: Ship -> StarSystem -> Maybe Wormhole
getNearestGravAnomalyToShip ship system = maybeWormhole where
  shpLoc = view sLocation ship
  maybeWormhole =
    foldl (nearestGravAnomalySelector shpLoc) Nothing (view ssWormholes system)

-- TODO: Cache the distance values in the accumulator so we don't need to recalculate closestShipDistance every fold.
selectNearestBody
  :: (Body -> Bool) -> ShipLocation -> Maybe Body -> Body -> Maybe Body
selectNearestBody filterFunc _ Nothing newBodyToCheck =
  if filterFunc newBodyToCheck then Just newBodyToCheck else Nothing
selectNearestBody filterFunc (ShipLocation ourX ourY) (Just closestBody) newBodyToCheck
  = newMaybeClosestBody where
  newMaybeClosestBody =
    if not (filterFunc newBodyToCheck) && newBodyDistance < closestBodyDistance
      then Just newBodyToCheck
      else Just closestBody
  (BodyLocation cbdX cbdY) = view bLocation closestBody
  (BodyLocation nbdX nbdY) = view bLocation newBodyToCheck
  closestBodyDistance      = sqrt $ ((cbdX - ourX) ** 2) + ((cbdY - ourY) ** 2)
  newBodyDistance          = sqrt $ ((nbdX - ourX) ** 2) + ((nbdY - ourY) ** 2)

getNearestBodyToShip
  :: Ship -> StarSystem -> Bool -> Bool -> Bool -> Bool -> Maybe Body
getNearestBodyToShip shp system plnts mns cmts astrds = nearestBody where
  nearestBodyFilter bdy = (mns && (view bType bdy) == Moon) ||
                          (cmts && (view bType bdy) == Comet) ||
                          (astrds && (view bType bdy) == Asteroid) ||
                          (plnts && elem (view bType bdy) [DwarfPlanet, GasDwarf, GasGiant, IceGiant, Terrestrial])
  shpLoc              = view sLocation shp
  systemBodies        = view ssBodies system
  nearestBody = Map.foldl (selectNearestBody nearestBodyFilter shpLoc) Nothing systemBodies

getNearestSurveyBodyToShip
  :: Ship -> StarSystem -> Bool -> Bool -> Bool -> Bool -> Maybe Body
getNearestSurveyBodyToShip shp system plnts mns cmts astrds =
  nearestBodyToSurvey where
  raceId = view sRace shp
  needsSurveyAndFilteredBody bdy =
    (  isNothing (Map.lookup raceId (view bSurveys bdy))
      || not (_bsSurveyed (view (bSurveys . at' raceId) bdy))
      )
      && (  (mns && (view bType bdy) == Moon)
         || (cmts && (view bType bdy) == Comet)
         || (astrds && (view bType bdy) == Asteroid)
         || (plnts && elem
              (view bType bdy)
              [DwarfPlanet, GasDwarf, GasGiant, IceGiant, Terrestrial]
            )
         )
  shpLoc              = view sLocation shp
  systemBodies        = view ssBodies system
  nearestBodyToSurvey = Map.foldl
    (selectNearestBody needsSurveyAndFilteredBody shpLoc)
    Nothing
    systemBodies

-- Given a ship location and two bodies, return the closest body.
nearestBodySelector :: ShipLocation -> Maybe Body -> Maybe Body -> Maybe Body
nearestBodySelector _ Nothing maybeNewBody = maybeNewBody
nearestBodySelector _ maybeBody Nothing = maybeBody
nearestBodySelector (ShipLocation ourX ourY) (Just current) (Just new) = Just
  nearest where
  nearest              = if currentDistance <= newDistance then current else new
  currentDistance      = sqrt $ ((cX - ourX) ** 2) + ((cY - ourY) ** 2)
  newDistance          = sqrt $ ((nX - ourX) ** 2) + ((nY - ourY) ** 2)
  (BodyLocation cX cY) = view bLocation current
  (BodyLocation nX nY) = view bLocation new

nearestGravAnomalySelector
  :: ShipLocation -> Maybe Wormhole -> Wormhole -> Maybe Wormhole
nearestGravAnomalySelector _ Nothing wormhole =
  if view wSurveyed wormhole then Nothing else Just wormhole
nearestGravAnomalySelector (ShipLocation ourX ourY) (Just current) new = Just
  nearest where
  nearest = if view wSurveyed new || currentDistance <= newDistance
    then current
    else new
  currentDistance          = sqrt $ ((cX - ourX) ** 2) + ((cY - ourY) ** 2)
  newDistance              = sqrt $ ((nX - ourX) ** 2) + ((nY - ourY) ** 2)
  (WormholeLocation cX cY) = view wLocation current
  (WormholeLocation nX nY) = view wLocation new

setGeologicalSurveyInProgress :: Ship -> Ship
setGeologicalSurveyInProgress ship = newShip'
 where
  maybeOrder = getShipActiveOrder ship
  newShip'   = case maybeOrder of
    (Just (GeologicalSurveyOrder bid False)) ->
      replaceShipActiveOrder (GeologicalSurveyOrder bid True) ship
    (Just (ContinuousGeologicalSurveyOrder mbid False plnts mns cmts astrds))
      -> replaceShipActiveOrder
        (ContinuousGeologicalSurveyOrder mbid True plnts mns cmts astrds)
        ship
    _ -> ship

setGravitationalSurveyInProgress :: Ship -> Ship
setGravitationalSurveyInProgress ship = newShip'
 where
  maybeOrder = getShipActiveOrder ship
  newShip'   = case maybeOrder of
    (Just (GravitationalSurveyOrder whid False)) ->
      replaceShipActiveOrder (GravitationalSurveyOrder whid True) ship
    (Just (ContinuousGravitationalSurveyOrder maybeWhid False)) ->
      replaceShipActiveOrder
        (ContinuousGravitationalSurveyOrder maybeWhid True)
        ship
    _ -> ship

setBuildJumpGateInProgress :: Ship -> Ship
setBuildJumpGateInProgress ship = newShip'
 where
  maybeOrder = getShipActiveOrder ship
  newShip'   = case maybeOrder of
    (Just (BuildJumpGateOrder whid False)) ->
      replaceShipActiveOrder (BuildJumpGateOrder whid True) ship
    _ -> ship

updateShipStatus :: Seconds -> Ship -> Ship
updateShipStatus secs =
  (updateShipMissleLaunchers secs)
    . (updateShipLasers secs)
    . (updateShipShields secs)

updateShipShields :: Seconds -> Ship -> Ship
updateShipShields secs shp = newShp where
  newShp             = shp { _sShields = newShpShields, _sFuel = newShpFuel }
  currentShipShields = view sShields shp
  currentShipFuel    = view sFuel shp
  (newShpShields, newShpFuel) =
    if isNothing currentShipShields || (not $ view ssEnabled shpShields)
      then (currentShipShields, currentShipFuel)
      else fuelCheck
  shpShields = fromJust currentShipShields
  fuelCheck  = if not enoughFuel
    then (currentShipShields, currentShipFuel)
    else (Just rechargedShields, rechargedShipFuel)
  enoughFuel                         = currentFuel >= requiredFuel
  currentFuel                        = unpack currentShipFuel
  (ComponentRating consumptionRate ) = view ssFuelConsumptionRate shpShields
  (ComponentRating regenerationRate) = view ssRegenerationRate shpShields
  (ComponentRating currentCapacity ) = view ssCurrentCapacity shpShields
  (ComponentRating maxCapacity     ) = view ssMaxCapacity shpShields
  requiredFuel                       = consumptionRate * fromIntegral secs
  rechargedShields = set ssCurrentCapacity newCapacity shpShields
  newCapacity                        = ComponentRating
    (minimum
      [currentCapacity + (regenerationRate * fromIntegral secs), maxCapacity]
    )
  rechargedShipFuel = ShipFuel (currentFuel - requiredFuel)

updateShipLasers :: Seconds -> Ship -> Ship
updateShipLasers secs shp = newShp where
  newShp            = shp { _sShipLasers = newShpLasers }
  currentShipLasers = view sShipLasers shp
  newShpLasers      = if isNothing currentShipLasers
    then currentShipLasers
    else Just rechargedLasers
  shpLasers                       = fromJust currentShipLasers
  (ComponentRating rechargeRate ) = view slRechargeRate shpLasers
  (ComponentRating currentStatus) = view slRechargeStatus shpLasers
  rechargedLasers                 = set slRechargeStatus newStatus shpLasers
  newStatus                       = ComponentRating
    (minimum [currentStatus + (rechargeRate * fromIntegral secs), 1])

updateShipMissleLaunchers :: Seconds -> Ship -> Ship
updateShipMissleLaunchers secs shp = newShp where
  newShp = shp { _sShipMissleLaunchers = newShipMissleLaunchers }
  currentShipMissleLaunchers = view sShipMissleLaunchers shp
  newShipMissleLaunchers = if isNothing currentShipMissleLaunchers
    then currentShipMissleLaunchers
    else Just reloadedShipMissleLaunchers
  missleLaunchers = fromJust currentShipMissleLaunchers
  reloadedShipMissleLaunchers =
    missleLaunchers { _smlReloadStatus = ComponentRating newReloadStatus }
  newReloadStatus =
    minimum [1, currentReloadStatus + (reloadRate * fromIntegral secs)]
  (ComponentRating currentReloadStatus) = view smlReloadStatus missleLaunchers
  (ComponentRating reloadRate         ) = view smlReloadRate missleLaunchers

processShipTasks
  :: RandomGen g
  => g
  -> Seconds
  -> ShipId
  -> (StarSystem, ShipTransits)
  -> (StarSystem, ShipTransits, g)
processShipTasks gen secs sId' (system, transits) =
  (newStarSystem, newTransits, newGen)
 where
  maybeShip                            = Map.lookup sId' (view ssShips system)
  (newStarSystem, newTransits, newGen) = if isNothing maybeShip
    then (system, transits, gen)
    else (starSystem, transits', gen')
  ship' = updateShipStatus secs (fromJust maybeShip)
  ship  = if isNothing (view sCurrentConditionalOrder ship')
    then foldl (processShipConditionalOrder system)
               ship'
               (reverse $ view sConditionalOrders ship')
    else ship'
  (currentOrder, conditional) =
    if isNothing (view sCurrentConditionalOrder ship)
      then (view sCurrentOrder ship, False)
      else
        ( Just $ view scoOrder (fromJust $ view sCurrentConditionalOrder ship)
        , True
        )
  gen'                    = fromMaybe gen maybeNewGen
  (starSystem, transits') = if isNothing maybeTransit
    then (L.over ssShips (Map.insert sId' newShip') starSystem', transits)
    else
      ( L.over ssShips (Map.delete sId') starSystem'
      , Map.insert sId' (fromJust maybeTransit) transits
      )
  (newShip', starSystem', maybeTransit, maybeNewGen) = case currentOrder of
    Just (MoveOrder x y) ->
      (processShipMove secs conditional x y ship, system, Nothing, Nothing)
    Just (MoveToBodyOrder bid) ->
      ( processShipMoveToBody secs conditional bid system ship
      , system
      , Nothing
      , Nothing
      )
    Just (OrbitOrder bid _) ->
      ( processShipOrbit secs conditional bid system ship
      , system
      , Nothing
      , Nothing
      )
    Just (FollowOrder sid) ->
      ( processShipFollowOrder secs conditional sid system ship
      , system
      , Nothing
      , Nothing
      )
    Just (AttackShipOrder sid) -> (newShp, newSystem, Nothing, Just newGen')     where
      (newShp, newSystem, newGen') =
        processShipAttackShipOrder gen secs conditional sid system ship
    Just AttackNearestShipOrder -> (newShp, newSystem, Nothing, Just newGen')     where
      (newShp, newSystem, newGen') =
        processShipAttackNearestShipOrder gen secs conditional system ship
    Just ContinuousAttackNearestShipOrder ->
      (newShp, newSystem, Nothing, Just newGen')     where
      (newShp, newSystem, newGen') =
        processShipContinuousAttackNearestShipOrder gen
                                                    secs
                                                    conditional
                                                    system
                                                    ship
    Just (AttackBodyOrder bid) -> (newShp, newSystem, Nothing, Nothing)     where
      (newShp, newSystem) =
        processShipAttackBodyOrder secs conditional bid system ship
    Just (PickupMineralsOrder bid mins inProg) ->
      (newShp, newSystem, Nothing, Nothing)
     where
      (newShp, newSystem) = processShipPickupMineralsOrder secs
                                                           conditional
                                                           bid
                                                           system
                                                           mins
                                                           inProg
                                                           ship
    Just (DropoffMineralsOrder bid mins inProg) ->
      (newShp, newSystem, Nothing, Nothing)
     where
      (newShp, newSystem) = processShipDropoffMineralsOrder secs
                                                            conditional
                                                            bid
                                                            system
                                                            mins
                                                            inProg
                                                            ship
    Just (PickupInstallmentOrder bid inst prog inProg) ->
      (newShp, newSystem, Nothing, Nothing)
     where
      (newShp, newSystem) = processShipPickupInstallmentOrder secs
                                                              conditional
                                                              bid
                                                              system
                                                              inst
                                                              prog
                                                              inProg
                                                              ship
    Just (DropoffInstallmentOrder bid inst prog inProg) ->
      (newShp, newSystem, Nothing, Nothing)
     where
      (newShp, newSystem) = processShipDropoffInstallmentOrder secs
                                                               conditional
                                                               bid
                                                               system
                                                               inst
                                                               prog
                                                               inProg
                                                               ship
    Just (RefuelAtBodyOrder bid) -> (newShp, newSystem, Nothing, Nothing)
     where
      (newShp, newSystem) =
        processShipRefuelAtBodyOrder secs conditional bid system ship
    Just (RefuelAtShipOrder sid) -> (newShp, newSystem, Nothing, Nothing)
     where
      (newShp, newSystem) =
        processShipRefuelAtShipOrder secs conditional sid system ship
    Just (TransferFuelToBody bid _) -> (newShp, newSystem, Nothing, Nothing)
     where
      (newShp, newSystem) =
        processShipTransferFuelToBodyOrder secs conditional bid system ship
    Just (TransferFuelToShip sid _) -> (newShp, newSystem, Nothing, Nothing)
     where
      (newShp, newSystem) =
        processShipTransferFuelToShipOrder secs conditional sid system ship
    Just (GeologicalSurveyOrder bid inProg) ->
      (newShp, newSystem, Nothing, Nothing)
     where
      (newShp, newSystem) =
        processShipGeologicalSurveyOrder secs conditional bid system inProg ship
    Just (ContinuousGeologicalSurveyOrder maybeBid inProg plnts mns cmts astrds)
      -> (newShp, newSystem, Nothing, Nothing)
     where
      (newShp, newSystem) = processShipContinuousGeologicalSurveyOrder
        secs
        conditional
        maybeBid
        system
        inProg
        plnts
        mns
        cmts
        astrds
        ship
    Just (GravitationalSurveyOrder whid inProg) ->
      (newShp, newSystem, Nothing, Nothing)
     where
      (newShp, newSystem) = processShipGravitationalSurveyOrder secs
                                                                conditional
                                                                whid
                                                                system
                                                                inProg
                                                                ship
    Just (ContinuousGravitationalSurveyOrder cwhid inProg) ->
      (newShp, newSystem, Nothing, Nothing)
     where
      (newShp, newSystem) = processShipContinuousGravitationalSurveyOrder
        secs
        conditional
        cwhid
        system
        inProg
        ship
    Just (BuildJumpGateOrder whid inProg) ->
      (newShp, newSystem, Nothing, Nothing)
     where
      (newShp, newSystem) =
        processShipBuildJumpGateOrder secs conditional whid system inProg ship
    Just (TransitJumpGateOrder whid _ cooldown) ->
      (newShp, newSystem, maybeTransit', Nothing)     where
      (newShp, newSystem, maybeTransit') = processShipTransitJumpGateOrder
        secs
        conditional
        whid
        cooldown
        system
        ship
    Just (MoveToJumpGateOrder whid) ->
      ( processShipMoveToJumpGateOrder secs conditional whid system ship
      , system
      , Nothing
      , Nothing
      )
    -- See if we can pull another order from the order groups.
    Nothing -> (completeOrder conditional ship, system, Nothing, Nothing)

processShipConditionalOrder
  :: StarSystem -> Ship -> ShipConditionalOrder -> Ship
processShipConditionalOrder _ ship (ShipConditionalOrder EnemyInSightCondition _)
  = ship -- Need AI/teams first.
processShipConditionalOrder system ship sco@(ShipConditionalOrder (FuelCondition (ComponentRating minFuel)) order)
  = newShp where
  (ShipFuel shipFuel) = view sFuel ship
  maxFuel             = totalFuelStorage (view (sDesign . sdFuelStorages) ship)
  fuelPercent         = shipFuel / maxFuel * 100
  newShp              = if validateOrder order system && fuelPercent < minFuel
    then set sCurrentConditionalOrder (Just sco) ship
    else ship
processShipConditionalOrder system ship sco@(ShipConditionalOrder (ShieldCondition (ComponentRating minShield)) order)
  = newShp where
  maybeShipShields = view sShields ship
  shpShields       = fromJust maybeShipShields
  shieldPercent =
    (unpack $ view ssCurrentCapacity shpShields)
      / (unpack $ view ssMaxCapacity shpShields)
      * 100
  newShp = if isNothing maybeShipShields || not (validateOrder order system)
    then ship
    else if shieldPercent < minShield
      then set sCurrentConditionalOrder (Just sco) ship
      else ship

validateOrder :: ShipOrder -> StarSystem -> Bool
validateOrder shipOrder system = valid where
  valid = case shipOrder of
    (MoveOrder _ _      )                         -> True
    (MoveToBodyOrder bid)                         -> validBodyId bid system
    (OrbitOrder bid _   )                         -> validBodyId bid system
    (FollowOrder     sid)                         -> validShipId sid system
    (AttackShipOrder sid)                         -> validShipId sid system
    AttackNearestShipOrder                        -> True
    ContinuousAttackNearestShipOrder              -> True
    (AttackBodyOrder bid                        ) -> validBodyId bid system
    (PickupMineralsOrder  bid _ _               ) -> validBodyId bid system
    (DropoffMineralsOrder bid _ _               ) -> validBodyId bid system
    (PickupInstallmentOrder  bid _ _ _          ) -> validBodyId bid system
    (DropoffInstallmentOrder bid _ _ _          ) -> validBodyId bid system
    (RefuelAtBodyOrder bid                      ) -> validBodyId bid system
    (RefuelAtShipOrder sid                      ) -> validShipId sid system
    (TransferFuelToBody    bid _                ) -> validBodyId bid system
    (TransferFuelToShip    sid _                ) -> validShipId sid system
    (GeologicalSurveyOrder bid _                ) -> validBodyId bid system
    (ContinuousGeologicalSurveyOrder _ _ _ _ _ _) -> True
    (GravitationalSurveyOrder           wid _   ) -> validWormholeId wid system
    (ContinuousGravitationalSurveyOrder _   _   ) -> True
    (BuildJumpGateOrder                 wid _   ) -> validWormholeId wid system
    (MoveToJumpGateOrder wid                    ) -> validWormholeId wid system
    (TransitJumpGateOrder wid _ _               ) -> validWormholeId wid system

validBodyId :: ComponentId -> StarSystem -> Bool
validBodyId bid system = Map.member bid (view ssBodies system)

validShipId :: ShipId -> StarSystem -> Bool
validShipId sid system = Map.member sid (view ssShips system)

validWormholeId :: WormholeId -> StarSystem -> Bool
validWormholeId wid system = Map.member wid (view ssWormholes system)

moveShip :: Seconds -> Double -> Double -> Ship -> Ship
moveShip seconds moveX moveY ship = newShip'
 where
  secs     = fromIntegral seconds
  -- Can't be bothered to figure out effective fuel used, just use up all fuel for the time elapsed.
  newShip' = if availableFuel > 0
    then newShip'' { _sFuel = ShipFuel newShipFuel }
    else ship
  newShip'' = if overshot
    then ship { _sLocation = ShipLocation moveX moveY }
    else ship { _sLocation = ShipLocation newX newY }
  newX                        = locationX + cos angle * shipTravelDistance
  newY                        = locationY + sin angle * shipTravelDistance
  shipMaxTravelDistance       = speed * secs
  shipEffectiveTravelDistance = availableFuel / litresPerSecond * speed
  shipTravelDistance =
    minimum [shipMaxTravelDistance, shipEffectiveTravelDistance]
  distanceToTravel =
    sqrt $ ((moveX - locationX) ** 2) + ((moveY - locationY) ** 2)
  overshot                           = shipTravelDistance >= distanceToTravel
  (ShipLocation locationX locationY) = view sLocation ship
  angle = atan2 (moveY - locationY) (moveX - locationX) :: Double
  (ShipSpeed speed        )          = view (sDesign . sdSpeed) ship
  (ShipFuel  availableFuel)          = view sFuel ship
  fuelEfficiency = enginesEfficiency $ view (sDesign . sdEngines) ship
  litresPerSecond                    = fuelEfficiency / 60 / 60
  newShipFuel                        = if overshot
    then availableFuel - (litresPerSecond * distanceToTravel / speed)
    else availableFuel - (litresPerSecond * shipTravelDistance / speed)

processShipMove :: Seconds -> Bool -> Double -> Double -> Ship -> Ship
processShipMove secs conditional moveX moveY ship = newShip'
 where
  movedShip                    = moveShip secs moveX moveY ship
  (ShipLocation movedX movedY) = view sLocation movedShip
  newShip'                     = if moveX == movedX && moveY == movedY
    then completeOrder conditional movedShip
    else movedShip

processShipMoveToBody :: Seconds -> Bool -> BodyId -> StarSystem -> Ship -> Ship
processShipMoveToBody secs conditional bodyId system ship = newShip'
 where
  maybeBody       = Map.lookup bodyId (view ssBodies system)
  newShip'        = if isNothing maybeBody
    then completeOrder conditional ship -- Make sure we remove the invalid order
    else newShip''
  newShip''            = processShipMove secs conditional bX bY ship
  (BodyLocation bX bY) = view bLocation (fromJust maybeBody)

-- TODO: For the love of god clean up those variable names.
processShipOrbit :: Seconds -> Bool -> BodyId -> StarSystem -> Ship -> Ship
processShipOrbit secs conditional bodyId system ship = newShip'
 where
  maybeBody       = Map.lookup bodyId (view ssBodies system)
  newShip'        = if isNothing maybeBody
    then completeOrder conditional ship -- Make sure we remove the invalid order
    else newShip''
  newShip'' = if isOrbiting ship && length (view sOrderGroups ship) > 1
    then set sLocation (ShipLocation bX bY) (completeOrder conditional ship) -- Make sure we remove the orbiting order and go to next order
    else newShip'''
  newShip''' = if isOrbiting ship
    then set sLocation (ShipLocation bX bY) ship -- Leave the orbit order and update ship position
    else newShip''''
  (BodyLocation bX bY) = view bLocation (fromJust maybeBody)
  movedShip            = moveShip secs bX bY ship
  (ShipLocation sX sY) = view sLocation movedShip
  newShip'''' =
    if sX == bX && sY == bY then setArrivedOrbitOrder movedShip else movedShip

processShipFollowOrder
  :: Seconds -> Bool -> ShipId -> StarSystem -> Ship -> Ship
processShipFollowOrder secs conditional sId' system ship = newShip'
 where
  maybeShip = Map.lookup sId' (view ssShips system)
  newShip'  = if isNothing maybeShip || sId' == view sId ship
    then completeOrder conditional ship -- Make sure we remove the invalid order
    else newShip''
  newShip''            = moveShip secs sX sY ship
  (ShipLocation sX sY) = view sLocation (fromJust maybeShip)

--
-- Load time is 50 hours (180,000 seconds) per cargo bay.
-- This is improved with cargo handling systems.
--
-- TODO: This, along with the other mineral/installment handling orders needs to be cleaned up big time.
--
processShipPickupMineralsOrder
  :: Seconds
  -> Bool
  -> BodyId
  -> StarSystem
  -> MineralStack
  -> Bool
  -> Ship
  -> (Ship, StarSystem)
processShipPickupMineralsOrder secs conditional bid system mins inProgress ship
  = (newShip', newSystem)
 where
  maybeBody                = Map.lookup bid (view ssBodies system)
  (newShip', newSystem)    = if isNothing maybeBody
    then (completeOrder conditional ship, system) -- Make sure we remove the invalid order
    else locationCheck
  locationCheck = if not inProgress then moveAndSetProgress else cargoCheck
  body                 = fromJust maybeBody
  (BodyLocation bX bY) = view bLocation body
  movedShip            = moveShip secs bX bY ship
  (ShipLocation sX sY) = view sLocation movedShip
  moveAndSetProgress   = if sX == bX && sY == bY
    then (setPickupMineralsInProgress movedShip, system)
    else (movedShip, system)
  cargoHolds    = view (sDesign . sdCargoHolds) movedShip
  numCargoHolds = length cargoHolds
  cargoCheck    = if numCargoHolds < 1
    then (completeOrder conditional movedShip, system) -- Make sure we remove the invalid order
    else availableSpaceCheck
  availableSpace      = availableCargoSpace movedShip
  availableSpaceCheck = if availableSpace < 0
    then (completeOrder conditional movedShip, system) -- No more space, order complete
    else availableMineralsCheck
  bodyMinerals                   = view bMinerals body
  element'                       = view minsElement mins
  (MineralCount mineralsInOrder) = view minsCount mins
  maybeMineral                   = Map.lookup element' bodyMinerals
  availableMineralsCheck         = if isNothing maybeMineral
    then (completeOrder conditional movedShip, system) -- No minerals, order complete
    else (loadedShip', newSystem')
  secondsEmptyToFull = fromIntegral $ 180000 * numCargoHolds
  totalCapacity      = cargoHoldsCapacity cargoHolds
  loadingRate =
    totalCapacity / secondsEmptyToFull / (shipCargoHandlingMultiplier ship)
  mineralsLoadable                 = loadingRate * fromIntegral secs
  (MineralCount availableMinerals) = view mStockpile (fromJust maybeMineral)
  mineralsToLoad                   = minimum
    [mineralsLoadable, availableSpace, availableMinerals, mineralsInOrder]
  maybeMineralStack =
    Map.lookup element' $ view (sCargo . scMinerals) movedShip
  shipMins = if isNothing maybeMineralStack
    then 0
    else unpack $ view minsCount (fromJust maybeMineralStack)
  newSystem' = set
    (ssBodies . at' bid . bMinerals . at' element' . mStockpile)
    (MineralCount $ availableMinerals - mineralsToLoad)
    system
  loadedShip = set
    (sCargo . scMinerals . at'' element')
    (Just $ MineralStack element' (MineralCount $ shipMins + mineralsToLoad))
    movedShip
  loadedShip' = if mineralsInOrder - mineralsToLoad <= 0.01 -- Rounding errors when moving minerals around
    then completeOrder conditional loadedShip
    else replaceShipActiveOrder newOrder loadedShip
  newOrder = set (pmoMinerals . minsCount)
                 (MineralCount $ mineralsInOrder - mineralsToLoad)
                 (fromJust $ getShipActiveOrder movedShip)

-- TODO: This, along with the other mineral/installment handling orders needs to be cleaned up big time.
processShipDropoffMineralsOrder
  :: Seconds
  -> Bool
  -> BodyId
  -> StarSystem
  -> MineralStack
  -> Bool
  -> Ship
  -> (Ship, StarSystem)
processShipDropoffMineralsOrder secs conditional bid system mins inProgress ship
  = (newShip', newSystem)
 where
  maybeBody                = Map.lookup bid (view ssBodies system)
  (newShip', newSystem)    = if isNothing maybeBody
    then (completeOrder conditional ship, system) -- Make sure we remove the invalid order
    else locationCheck
  locationCheck =
    if not inProgress then moveAndSetProgress else shipMineralsCheck
  body                 = fromJust maybeBody
  (BodyLocation bX bY) = view bLocation body
  movedShip            = moveShip secs bX bY ship
  (ShipLocation sX sY) = view sLocation movedShip
  moveAndSetProgress   = if sX == bX && sY == bY
    then (setDropoffMineralsInProgress movedShip, system)
    else (movedShip, system)
  bodyMinerals                   = view bMinerals body
  element'                       = view minsElement mins
  (MineralCount mineralsInOrder) = view minsCount mins
  maybeMineralStack =
    Map.lookup element' $ view (sCargo . scMinerals) movedShip
  shipMineralsCheck = if isNothing maybeMineralStack
    then (completeOrder conditional movedShip, system) -- No more minerals in ship
    else (unloadedShip', newSystem')
  cargoHolds            = view (sDesign . sdCargoHolds) movedShip
  numCargoHolds         = length cargoHolds
  secondsFullToEmpty    = fromIntegral $ 180000 * numCargoHolds
  totalCapacity         = cargoHoldsCapacity cargoHolds
  unloadingRate         = totalCapacity / secondsFullToEmpty
  mineralsUnloadable    = unloadingRate * fromIntegral secs
  availableShipMinerals = unpack $ view minsCount (fromJust maybeMineralStack)
  mineralsToUnload =
    minimum [mineralsUnloadable, mineralsInOrder, availableShipMinerals]
  (MineralCount availableBodyMinerals) =
    view (at' element' . mStockpile) bodyMinerals
  newSystem' = set
    (ssBodies . at' bid . bMinerals . at' element' . mStockpile)
    (MineralCount $ availableBodyMinerals + mineralsToUnload)
    system
  unloadedShip = set
    (sCargo . scMinerals . at'' element')
    (Just $ MineralStack
      element'
      (MineralCount $ availableShipMinerals - mineralsToUnload)
    )
    movedShip
  unloadedShip' = if availableShipMinerals - mineralsToUnload < 0.01 -- Rounding errors when moving minerals around
    then completeOrder conditional shipWithoutMineStack
    else replaceShipActiveOrder newOrder unloadedShip
  newOrder = set (dmoMinerals . minsCount)
                 (MineralCount $ mineralsInOrder - mineralsToUnload)
                 (fromJust $ getShipActiveOrder ship)
  shipWithoutMineStack =
    L.over (sCargo . scMinerals) (Map.delete element') unloadedShip

-- TODO: This, along with the other mineral/installment handling orders needs to be cleaned up big time.
processShipPickupInstallmentOrder
  :: Seconds
  -> Bool
  -> BodyId
  -> StarSystem
  -> Installment
  -> TaskProgress
  -> Bool
  -> Ship
  -> (Ship, StarSystem)
processShipPickupInstallmentOrder secs conditional bid system inst (TaskProgress prog) inProg ship
  = (newShip', newSystem)
 where
  maybeBody                = Map.lookup bid (view ssBodies system)
  (newShip', newSystem)    = if isNothing maybeBody
    then (completeOrder conditional ship, system) -- Make sure we remove the invalid order
    else locationCheck
  locationCheck =
    if not inProg then moveAndSetProgress else availableSpaceCheck
  body                 = fromJust maybeBody
  (BodyLocation bX bY) = view bLocation body
  movedShip            = moveShip secs bX bY ship
  (ShipLocation sX sY) = view sLocation movedShip
  moveAndSetProgress   = if sX == bX && sY == bY
    then (setPickupInstallmentInProgress movedShip, system)
    else (movedShip, system)
  (InstallmentSize spaceRequired) = view iSize inst
  availableSpace                  = availableCargoSpace movedShip
  availableSpaceCheck             = if availableSpace < spaceRequired
    then (completeOrder conditional movedShip, system) -- No more space, order complete
    else availableInstallmentCheck
  bodyInstallments      = view bInstallments body
  instId                = view iId inst
  maybeInstallmentStack = Map.lookup instId bodyInstallments
  bodyInstallmentStack  = fromJust maybeInstallmentStack
  (InstallmentCount bodyInstallmentCount) = view isCount bodyInstallmentStack
  availableInstallmentCheck =
    if isNothing maybeInstallmentStack || bodyInstallmentCount < 1
      then (completeOrder conditional movedShip, system) -- No more installments left to pickup, order complete
      else progressCheck
  cargoHolds         = view (sDesign . sdCargoHolds) movedShip
  numCargoHolds      = length cargoHolds
  secondsEmptyToFull = fromIntegral $ 180000 * numCargoHolds
  totalCapacity      = cargoHoldsCapacity cargoHolds
  loadingRate        = totalCapacity / secondsEmptyToFull
  currentOrder       = getShipActiveOrder ship
  progressRate       = loadingRate / spaceRequired
  progressAmount     = progressRate * fromIntegral secs
  newProgress        = prog + progressAmount
  progressCheck      = if newProgress < 1
    then (progressedOrder, system)
    else (shipWithCargo, newSystem')
  progressedOrder = replaceShipActiveOrder
    (set pioProgress (TaskProgress newProgress) (fromJust currentOrder))
    movedShip
  completedOrder = completeOrder conditional movedShip
  shipWithCargo =
    if isNothing
       $ Map.lookup instId (view (sCargo . scInstallments) completedOrder)
    then
      L.over (sCargo . scInstallments)
             (Map.insert instId newInstStack)
             completedOrder
    else
      L.over (sCargo . scInstallments . at' instId . isCount)
             (pack . (+) 1 . unpack)
             completedOrder
  newInstStack = InstallmentStack (InstallmentCount 1) inst
  newSystem'   = L.over
    (ssBodies . at' bid . bInstallments . at' instId . isCount)
    (pack . (flip (-)) 1 . unpack)
    system

-- TODO: This, along with the other mineral/installment handling orders needs to be cleaned up big time.
processShipDropoffInstallmentOrder
  :: Seconds
  -> Bool
  -> BodyId
  -> StarSystem
  -> Installment
  -> TaskProgress
  -> Bool
  -> Ship
  -> (Ship, StarSystem)
processShipDropoffInstallmentOrder secs conditional bid system inst (TaskProgress prog) inProg ship
  = (newShip', newSystem)
 where
  maybeBody                = Map.lookup bid (view ssBodies system)
  (newShip', newSystem)    = if isNothing maybeBody
    then (completeOrder conditional ship, system) -- Make sure we remove the invalid order
    else locationCheck
  locationCheck =
    if not inProg then moveAndSetProgress else availableInstallmentCheck
  body                 = fromJust maybeBody
  (BodyLocation bX bY) = view bLocation body
  movedShip            = moveShip secs bX bY ship
  (ShipLocation sX sY) = view sLocation movedShip
  moveAndSetProgress   = if sX == bX && sY == bY
    then (setDropoffInstallmentInProgress movedShip, system)
    else (movedShip, system)
  shipInstallments      = view (sCargo . scInstallments) movedShip
  instId                = view iId inst
  maybeInstallmentStack = Map.lookup instId shipInstallments
  shipInstallmentStack  = fromJust maybeInstallmentStack
  (InstallmentCount shipInstallmentCount) = view isCount shipInstallmentStack
  availableInstallmentCheck =
    if isNothing maybeInstallmentStack || shipInstallmentCount < 1
      then (completeOrder conditional movedShip, system) -- No more installments left to dropoff, order complete
      else progressCheck
  cargoHolds                        = view (sDesign . sdCargoHolds) movedShip
  numCargoHolds                     = length cargoHolds
  secondsEmptyToFull                = fromIntegral $ 180000 * numCargoHolds
  totalCapacity                     = cargoHoldsCapacity cargoHolds
  unloadingRate                     = totalCapacity / secondsEmptyToFull
  currentOrder                      = getShipActiveOrder ship
  (InstallmentSize installmentSize) = view iSize inst
  progressRate                      = unloadingRate / installmentSize
  progressAmount                    = progressRate * fromIntegral secs
  newProgress                       = prog + progressAmount
  progressCheck                     = if newProgress < 1
    then (progressedOrder, system)
    else (shipWithoutCargo, newSystem')
  progressedOrder = replaceShipActiveOrder
    (set dioProgress (TaskProgress newProgress) (fromJust currentOrder))
    movedShip
  completedOrder   = completeOrder conditional movedShip
  shipWithoutCargo = L.over (sCargo . scInstallments . at' instId . isCount)
                            (pack . (flip (-)) 1 . unpack)
                            completedOrder
  bodyInstallments = view bInstallments body
  newSystem'       = if isNothing $ Map.lookup instId bodyInstallments
    then L.over (ssBodies . at' bid . bInstallments)
                (Map.insert instId newInstStack)
                system
    else L.over (ssBodies . at' bid . bInstallments . at' instId . isCount)
                (pack . (+) 1 . unpack)
                system
  newInstStack = InstallmentStack (InstallmentCount 1) inst

processShipRefuelAtBodyOrder
  :: Seconds -> Bool -> BodyId -> StarSystem -> Ship -> (Ship, StarSystem)
processShipRefuelAtBodyOrder secs conditional bid system ship =
  (newShip', newSystem)
 where
  maybeBody                = Map.lookup bid (view ssBodies system)
  (newShip', newSystem)    = if isNothing maybeBody
    then (completeOrder conditional ship, system) -- Make sure we remove the invalid order
    else locationCheck
  locationCheck =
    if arrivedAtBody then (refueldShip, system') else (movedShip, system)
  body                    = fromJust maybeBody
  (BodyLocation bX bY)    = view bLocation body
  movedShip               = moveShip secs bX bY ship
  (ShipLocation sX sY)    = view sLocation movedShip
  arrivedAtBody           = sX == bX && sY == bY
  (newBody, refueldShip') = setShipRefueldAtBody movedShip body
  refueldShip             = completeOrder conditional refueldShip'
  system'                 = L.over ssBodies (Map.insert bid newBody) system

processShipRefuelAtShipOrder
  :: Seconds -> Bool -> ShipId -> StarSystem -> Ship -> (Ship, StarSystem)
processShipRefuelAtShipOrder secs conditional sid system ship =
  (newShip', newSystem)
 where
  maybeShip             = Map.lookup sid (view ssShips system)
  (newShip', newSystem) = if isNothing maybeShip
    then (completeOrder conditional ship, system) -- Make sure we remove the invalid order
    else locationCheck
  locationCheck =
    if arrivedAtShip then (refueldShip, system') else (movedShip, system)
  shipWithFuel                    = fromJust maybeShip
  (ShipLocation swfX swfY)        = view sLocation shipWithFuel
  movedShip                       = moveShip secs swfX swfY ship
  (ShipLocation sX sY)            = view sLocation movedShip
  arrivedAtShip                   = sX == swfX && sY == swfY
  (newShipWithFuel, refueldShip') = setShipRefueldAtShip movedShip shipWithFuel
  refueldShip                     = completeOrder conditional refueldShip'
  system' = L.over ssShips (Map.insert sid newShipWithFuel) system

processShipTransferFuelToBodyOrder
  :: Seconds -> Bool -> BodyId -> StarSystem -> Ship -> (Ship, StarSystem)
processShipTransferFuelToBodyOrder secs conditional bid system ship =
  (newShip', newSystem)
 where
  maybeBody                = Map.lookup bid (view ssBodies system)
  (newShip', newSystem)    = if isNothing maybeBody
    then (completeOrder conditional ship, system) -- Make sure we remove the invalid order
    else locationCheck
  locationCheck =
    if arrivedAtBody then (shipWithoutFuel, system') else (movedShip, system)
  body                        = fromJust maybeBody
  (BodyLocation bX bY)        = view bLocation body
  movedShip                   = moveShip secs bX bY ship
  (ShipLocation sX sY)        = view sLocation movedShip
  arrivedAtBody               = sX == bX && sY == bY
  (newBody, shipWithoutFuel') = setShipUnfueldAtBody movedShip body
  shipWithoutFuel             = completeOrder conditional shipWithoutFuel'
  system' = L.over ssBodies (Map.insert bid newBody) system

processShipTransferFuelToShipOrder
  :: Seconds -> Bool -> ShipId -> StarSystem -> Ship -> (Ship, StarSystem)
processShipTransferFuelToShipOrder secs conditional sid system ship =
  (newShip', newSystem)
 where
  maybeShip             = Map.lookup sid (view ssShips system)
  (newShip', newSystem) = if isNothing maybeShip || sid == view sId ship
    then (completeOrder conditional ship, system) -- Make sure we remove the invalid order
    else locationCheck
  locationCheck =
    if arrivedAtShip then (unfueldShip, system') else (movedShip, system)
  shipRequiringFuel        = fromJust maybeShip
  (ShipLocation srfX srfY) = view sLocation shipRequiringFuel
  movedShip                = moveShip secs srfX srfY ship
  (ShipLocation sX sY)     = view sLocation movedShip
  arrivedAtShip            = sX == srfX && sY == srfY
  (unfueldShip', newShipRequiringFuel) =
    setShipUnfueldAtShip movedShip shipRequiringFuel
  unfueldShip = completeOrder conditional unfueldShip'
  system'     = L.over ssShips (Map.insert sid newShipRequiringFuel) system

processShipGeologicalSurveyOrder
  :: Seconds
  -> Bool
  -> BodyId
  -> StarSystem
  -> Bool
  -> Ship
  -> (Ship, StarSystem)
processShipGeologicalSurveyOrder secs conditional bid system inProg ship =
  (newShip', newSystem)
 where
  maybeBody                = Map.lookup bid (view ssBodies system)
  (newShip', newSystem)    = if isNothing maybeBody
    then (completeOrder conditional ship, system) -- Make sure we remove the invalid order
    else locationCheck
  locationCheck =
    if not inProg then moveAndSetProgress else availableSensorCheck
  body                 = fromJust maybeBody
  (BodyLocation bX bY) = view bLocation body
  movedShip            = moveShip secs bX bY ship
  (ShipLocation sX sY) = view sLocation movedShip
  moveAndSetProgress   = if sX == bX && sY == bY
    then (setGeologicalSurveyInProgress movedShip, system)
    else (movedShip, system)
  (ComponentRating totalSensorRating) =
    totalGeologicalSensorRating shipGeoSensors
  shipGeoSensors       = view (sDesign . sdGeologicalSensors) ship
  availableSensorCheck = if totalSensorRating > 0
    then progressCheck
    else (completeOrder conditional movedShip, system) -- No sensors, order complete
  raceId                        = view sRace ship
  maybeSurveyStatus             = Map.lookup raceId (view bSurveys body)
  (BodyRadius radius          ) = view bRadius body
  (BodySurvey _ surveyProgress) = if isJust maybeSurveyStatus
    then fromJust maybeSurveyStatus
    else (BodySurvey False 0)
  requiredSurveyPoints = radius / 10
  hoursToCompletion    = requiredSurveyPoints / totalSensorRating
  secondsToCompletion  = hoursToCompletion * 60 * 60
  addedProgress        = fromIntegral secs / secondsToCompletion
  newProgress          = minimum [surveyProgress + addedProgress, 1.0]
  orbitingShip         = set sLocation (ShipLocation bX bY) ship
  progressCheck        = if newProgress < 1
    then (orbitingShip, newSystemNotSurveyed)
    else (shipCompletedOrder, newSystem')
  shipCompletedOrder   = completeOrder conditional orbitingShip
  newSurveyNotSurveyed = BodySurvey False newProgress
  newSystemNotSurveyed = L.set
    (ssBodies . at' bid . bSurveys . at'' raceId)
    (Just newSurveyNotSurveyed)
    system
  newSurvey  = BodySurvey True newProgress
  newSystem' = L.set (ssBodies . at' bid . bSurveys . at'' raceId)
                     (Just newSurvey)
                     system

processShipContinuousGeologicalSurveyOrder
  :: Seconds
  -> Bool
  -> Maybe BodyId
  -> StarSystem
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> Ship
  -> (Ship, StarSystem)
processShipContinuousGeologicalSurveyOrder secs conditional maybeBid system inProg plnts mns cmts astrds ship
  = (newShip', newSystem)
 where
  maybeNearestBody =
    getNearestSurveyBodyToShip ship system plnts mns cmts astrds
  -- There is a scenario where a conditional order sent you through a jump gate while geo survey was in progress.
  -- Need to account for this and check both for nearest body if the already provided body isn't found.
  maybeNewBid = if isNothing maybeBid
    then if isNothing maybeNearestBody
      then Nothing
      else Just $ view bId (fromJust maybeNearestBody)
    else if Map.member (fromJust maybeBid) (view ssBodies system)
      then maybeBid
      else if isNothing maybeNearestBody
        then Nothing
        else Just $ view bId (fromJust maybeNearestBody)
  bid   = fromJust maybeNewBid
  ship' = replaceShipActiveOrder
    (ContinuousGeologicalSurveyOrder maybeNewBid inProg plnts mns cmts astrds)
    ship
  maybeBody = if isNothing maybeNewBid
    then Nothing
    else Map.lookup bid (view ssBodies system)
  (newShip', newSystem) = if isNothing maybeBody
    then (completeOrder conditional ship, system)
    else locationCheck
  locationCheck =
    if not inProg then moveAndSetProgress else availableSensorCheck
  body                 = fromJust maybeBody
  (BodyLocation bX bY) = view bLocation body
  movedShip            = moveShip secs bX bY ship'
  (ShipLocation sX sY) = view sLocation movedShip
  moveAndSetProgress   = if sX == bX && sY == bY
    then (setGeologicalSurveyInProgress movedShip, system)
    else (movedShip, system)
  (ComponentRating totalSensorRating) =
    totalGeologicalSensorRating shipGeoSensors
  shipGeoSensors       = view (sDesign . sdGeologicalSensors) ship
  availableSensorCheck = if totalSensorRating > 0
    then progressCheck
    else (completeOrder conditional movedShip, system) -- No sensors, order complete
  raceId                        = view sRace ship
  maybeSurveyStatus             = Map.lookup raceId (view bSurveys body)
  (BodyRadius radius          ) = view bRadius body
  (BodySurvey _ surveyProgress) = if isJust maybeSurveyStatus
    then fromJust maybeSurveyStatus
    else (BodySurvey False 0)
  requiredSurveyPoints = radius / 10
  hoursToCompletion    = requiredSurveyPoints / totalSensorRating
  secondsToCompletion  = hoursToCompletion * 60 * 60
  addedProgress        = fromIntegral secs / secondsToCompletion
  newProgress          = minimum [surveyProgress + addedProgress, 1.0]
  orbitingShip         = set sLocation (ShipLocation bX bY) ship'
  progressCheck        = if newProgress < 1
    then (orbitingShip, newSystemNotSurveyed)
    else (shipCompletedOrder, newSystem')
  shipCompletedOrder = replaceShipActiveOrder
    (ContinuousGeologicalSurveyOrder Nothing False plnts mns cmts astrds)
    orbitingShip
  newSurveyNotSurveyed = BodySurvey False newProgress
  newSystemNotSurveyed = L.set (ssBodies . at' bid . bSurveys . at'' raceId)
                               (Just newSurveyNotSurveyed)
                               system
  newSurvey  = BodySurvey True newProgress
  newSystem' = L.set (ssBodies . at' bid . bSurveys . at'' raceId)
                     (Just newSurvey)
                     system

processShipGravitationalSurveyOrder
  :: Seconds
  -> Bool
  -> WormholeId
  -> StarSystem
  -> Bool
  -> Ship
  -> (Ship, StarSystem)
processShipGravitationalSurveyOrder secs conditional whid system inProg ship =
  (newShip', newSystem)
 where
  maybeWormhole         = Map.lookup whid (view ssWormholes system)
  (newShip', newSystem) = if isNothing maybeWormhole
    then (completeOrder conditional ship, system) -- Make sure we remove the invalid order
    else locationCheck
  locationCheck =
    if not inProg then moveAndSetProgress else availableSensorCheck
  wormhole                 = fromJust maybeWormhole
  (WormholeLocation wX wY) = view wLocation wormhole
  movedShip                = moveShip secs wX wY ship
  (ShipLocation sX sY)     = view sLocation movedShip
  moveAndSetProgress       = if sX == wX && sY == wY
    then (setGravitationalSurveyInProgress movedShip, system)
    else (movedShip, system)
  (ComponentRating totalSensorRating) =
    totalGravitationalSensorRating shipGravSensors
  shipGravSensors      = view (sDesign . sdGravitationalSensors) ship
  availableSensorCheck = if totalSensorRating > 0
    then progressCheck
    else (completeOrder conditional movedShip, system) -- No sensors, order complete
  requiredSurveyPoints          = 500
  (TaskProgress surveyProgress) = view wSurveyProgress wormhole
  hoursToCompletion             = requiredSurveyPoints / totalSensorRating
  secondsToCompletion           = hoursToCompletion * 60 * 60
  addedProgress                 = fromIntegral secs / secondsToCompletion
  newProgress                   = minimum [surveyProgress + addedProgress, 1.0]
  progressCheck                 = if newProgress < 1
    then (movedShip, newSystemNotSurveyed)
    else (shipCompletedOrder, newSystem')
  shipCompletedOrder   = completeOrder conditional movedShip
  newSystemNotSurveyed = L.set (ssWormholes . at' whid . wSurveyProgress)
                               (TaskProgress newProgress)
                               system
  newSystem' =
    L.set (ssWormholes . at' whid . wSurveyed) True newSystemNotSurveyed

processShipContinuousGravitationalSurveyOrder
  :: Seconds
  -> Bool
  -> Maybe WormholeId
  -> StarSystem
  -> Bool
  -> Ship
  -> (Ship, StarSystem)
processShipContinuousGravitationalSurveyOrder secs conditional maybeWhid system inProg ship
  = (newShip', newSystem)
 where
  maybeNearestWormhole = getNearestGravAnomalyToShip ship system
  -- There is a scenario where a conditional order sent you through a jump gate while grav survey was in progress.
  -- Need to account for this and check both for nearest wormhole if the already provided wormhole isn't found.
  maybeNewWhid         = if isNothing maybeWhid
    then if isNothing maybeNearestWormhole
      then Nothing
      else Just $ view wId (fromJust maybeNearestWormhole)
    else if Map.member (fromJust maybeWhid) (view ssWormholes system)
      then maybeWhid
      else if isNothing maybeNearestWormhole
        then Nothing
        else Just $ view wId (fromJust maybeNearestWormhole)
  whid  = fromJust maybeNewWhid
  ship' = replaceShipActiveOrder
    (ContinuousGravitationalSurveyOrder maybeNewWhid inProg)
    ship
  maybeWormhole = if isNothing maybeNewWhid
    then Nothing
    else Map.lookup whid (view ssWormholes system)
  (newShip', newSystem) = if isNothing maybeWormhole
    then (completeOrder conditional ship, system) -- Make sure we remove the invalid order
    else locationCheck
  locationCheck =
    if not inProg then moveAndSetProgress else availableSensorCheck
  wormhole                 = fromJust maybeWormhole
  (WormholeLocation wX wY) = view wLocation wormhole
  movedShip                = moveShip secs wX wY ship'
  (ShipLocation sX sY)     = view sLocation movedShip
  moveAndSetProgress       = if sX == wX && sY == wY
    then (setGravitationalSurveyInProgress movedShip, system)
    else (movedShip, system)
  (ComponentRating totalSensorRating) =
    totalGravitationalSensorRating shipGravSensors
  shipGravSensors      = view (sDesign . sdGravitationalSensors) ship'
  availableSensorCheck = if totalSensorRating > 0
    then progressCheck
    else (completeOrder conditional movedShip, system) -- No sensors, order complete
  requiredSurveyPoints          = 500
  (TaskProgress surveyProgress) = view wSurveyProgress wormhole
  hoursToCompletion             = requiredSurveyPoints / totalSensorRating
  secondsToCompletion           = hoursToCompletion * 60 * 60
  addedProgress                 = fromIntegral secs / secondsToCompletion
  newProgress                   = minimum [surveyProgress + addedProgress, 1.0]
  progressCheck                 = if newProgress < 1
    then (movedShip, newSystemNotSurveyed)
    else (shipCompletedOrder, newSystem')
  shipCompletedOrder = replaceShipActiveOrder
    (ContinuousGravitationalSurveyOrder Nothing False)
    ship'
  newSystemNotSurveyed = L.set (ssWormholes . at' whid . wSurveyProgress)
                               (TaskProgress newProgress)
                               system
  newSystem' =
    L.set (ssWormholes . at' whid . wSurveyed) True newSystemNotSurveyed

processShipBuildJumpGateOrder
  :: Seconds
  -> Bool
  -> WormholeId
  -> StarSystem
  -> Bool
  -> Ship
  -> (Ship, StarSystem)
processShipBuildJumpGateOrder secs conditional whid system inProg ship =
  (newShip', newSystem)
 where
  maybeWormhole         = Map.lookup whid (view ssWormholes system)
  (newShip', newSystem) = if isNothing maybeWormhole
    then (completeOrder conditional ship, system) -- Make sure we remove the invalid order
    else locationCheck
  locationCheck =
    if not inProg then moveAndSetProgress else availableGatesCheck
  wormhole                 = fromJust maybeWormhole
  (WormholeLocation wX wY) = view wLocation wormhole
  movedShip                = moveShip secs wX wY ship
  (ShipLocation sX sY)     = view sLocation movedShip
  moveAndSetProgress       = if sX == wX && sY == wY
    then (setBuildJumpGateInProgress movedShip, system)
    else (movedShip, system)
  (ComponentRating totalGatesRating) = totalJumpGatesRating shipJumpGates
  shipJumpGates                      = view (sDesign . sdJumpGates) ship
  availableGatesCheck                = if totalGatesRating > 0
    then progressCheck
    else (completeOrder conditional movedShip, system) -- No sensors, order complete
  requiredSurveyPoints         = 9000
  (TaskProgress buildProgress) = view wJumpGateProgress wormhole
  hoursToCompletion            = requiredSurveyPoints / totalGatesRating
  secondsToCompletion          = hoursToCompletion * 60 * 60
  addedProgress                = fromIntegral secs / secondsToCompletion
  newProgress                  = minimum [buildProgress + addedProgress, 1.0]
  progressCheck                = if newProgress < 1
    then (movedShip, newSystemNoGate)
    else (shipCompletedOrder, newSystem')
  shipCompletedOrder = completeOrder conditional movedShip
  newSystemNoGate    = L.set (ssWormholes . at' whid . wJumpGateProgress)
                             (TaskProgress newProgress)
                             system
  newSystem' = L.set (ssWormholes . at' whid . wJumpGate) True newSystemNoGate

processShipTransitJumpGateOrder
  :: Seconds
  -> Bool
  -> WormholeId
  -> TaskProgress
  -> StarSystem
  -> Ship
  -> (Ship, StarSystem, Maybe ShipTransit)
processShipTransitJumpGateOrder secs conditional whid (TaskProgress cooldown) system ship
  = (newShip', newSystem, maybeTransit)
 where
  maybeWormhole = Map.lookup whid (view ssWormholes system)
  (newShip', newSystem, maybeTransit) =
    if isNothing maybeWormhole && cooldown > 0
      then (cooldownUpdatedShip, system, Nothing)
      else if isNothing maybeWormhole
        then (completeOrder conditional ship, system, Nothing) -- Make sure we remove the invalid order
        else moveAndTransit
  currentOrder = fromJust $ getShipActiveOrder ship
  cooldownUpdatedShip' = replaceShipActiveOrder cooldownTransitOrder ship
  (cooldownTransitOrder, currentCooldown) = case currentOrder of
    (TransitJumpGateOrder whid' sysid (TaskProgress cd)) ->
      ( TransitJumpGateOrder whid' sysid (TaskProgress (cd - fromIntegral secs))
      , cd - fromIntegral secs
      )
    order -> (order, 0)
  cooldownUpdatedShip = if currentCooldown <= 0
    then completeOrder conditional cooldownUpdatedShip'
    else cooldownUpdatedShip'
  wormhole                 = fromJust maybeWormhole
  (WormholeLocation wX wY) = view wLocation wormhole
  movedShip                = moveShip secs wX wY ship
  (ShipLocation sX sY)     = view sLocation movedShip
  moveAndTransit           = if sX == wX && sY == wY
    then availableGatesCheck
    else (movedShip, system, Nothing)
  availableGatesCheck = if (view wJumpGate wormhole)
    then (shipCooldownStarted, newSystemNoShip, mTran)
    else (completeOrder conditional movedShip, system, Nothing)
  shipCooldownStarted = if isJust cooldownStarted
    then replaceShipActiveOrder (fromJust cooldownStarted) movedShip
    else movedShip
  cooldownStarted = case currentOrder of
    (TransitJumpGateOrder whid' sysid _) ->
      Just $ TransitJumpGateOrder whid' sysid (TaskProgress 60)
    _ -> Nothing
  shipId          = view sId shipCooldownStarted
  newSystemNoShip = L.set (ssShips . at'' shipId) Nothing system
  mTran =
    if isNothing (view wDestinationId wormhole)
         || isNothing (view wDestinationStarId wormhole)
         || isNothing (view wDestinationLocation wormhole)
      then Nothing
      else Just ShipTransit
        { _tsDestinationId = fromJust (view wDestinationStarId wormhole)
        , _tsWormholeId    = fromJust (view wDestinationId wormhole)
        , _tsShip          = newTransitShip
        }
  (WormholeLocation dX dY) = fromJust $ view wDestinationLocation wormhole
  newTransitShip = set sLocation (ShipLocation dX dY) shipCooldownStarted

processShipMoveToJumpGateOrder
  :: Seconds -> Bool -> WormholeId -> StarSystem -> Ship -> Ship
processShipMoveToJumpGateOrder secs conditional whid system ship = newShip'
 where
  maybeWormhole = Map.lookup whid (view ssWormholes system)
  newShip'      = if isNothing maybeWormhole
    then (completeOrder conditional ship) -- Make sure we remove the invalid order
    else processShipMove secs conditional wX wY ship
  (WormholeLocation wX wY) = view wLocation (fromJust maybeWormhole)

processShipAttackShipOrder
  :: RandomGen g
  => g
  -> Seconds
  -> Bool
  -> ShipId
  -> StarSystem
  -> Ship
  -> (Ship, StarSystem, g)
processShipAttackShipOrder gen secs conditional sid system ship =
  (newShp, newSystem, newGen) where
  maybeEnemyShip = Map.lookup sid (view ssShips system)
  (newShp, newSystem, newGen) =
    if isNothing maybeEnemyShip || sid == view sId ship
      then (completeOrder conditional ship, system, gen) -- Make sure we remove the invalid order
      else weaponsCheck
  maybeMissleLaunchers = view sShipMissleLaunchers ship
  maybeShipLasers      = view sShipLasers ship
  noMissleLaunchers    = isNothing maybeMissleLaunchers
  noLasers             = isNothing maybeShipLasers
  weaponsCheck         = if noMissleLaunchers && noLasers
    then (completeOrder conditional ship, system, gen) -- Make sure we remove the invalid order
    else locationCheck
  optimalAttackRange     = shipOptimalAttackRange ship
  (ShipLocation sX sY)   = view sLocation ship
  enemyShip              = fromJust maybeEnemyShip
  (ShipLocation esX esY) = view sLocation enemyShip
  currentEnemyDistance   = sqrt $ ((esX - sX) ** 2) + ((esY - sY) ** 2)
  movedShip              = if currentEnemyDistance <= optimalAttackRange
    then ship
    else moveShip secs esX esY ship
  (ShipLocation msX msY) = view sLocation movedShip
  newEnemyDistance       = sqrt $ ((esX - msX) ** 2) + ((esY - msY) ** 2)
  locationCheck =
    if newEnemyDistance > missleRange && newEnemyDistance > laserRange
      then (movedShip, system, gen)
      else (shipAfterLasers, system', gen')
  missleRange = if noMissleLaunchers
    then 0
    else unpack $ view (smlSalvo . msRange) (fromJust maybeMissleLaunchers)
  laserRange = if noLasers || length lasers < 1
    then 0
    else unpack $ view lRange firstLaser
  shipLasers' = fromJust maybeShipLasers
  lasers      = view slLasers shipLasers'
  firstLaser  = lasers !! 0
  (shipAfterMissleLaunchers, maybeNewMissleSalvo) = if noMissleLaunchers
    then (movedShip, Nothing)
    else missleLauncherRangeCheck
  missleLauncherRangeCheck = if newEnemyDistance > missleRange
    then (movedShip, Nothing)
    else missleSalvoFromShip movedShip salvoTarget
  enemyId     = view sId enemyShip
  salvoTarget = MissleSalvoTarget ShipTarget enemyId
  (shipAfterLasers, maybeEnemyShip', gen') = if noLasers
    then (shipAfterMissleLaunchers, Just enemyShip, gen)
    else laserRangeCheck
  laserRangeCheck = if newEnemyDistance > laserRange
    then (shipAfterMissleLaunchers, Just enemyShip, gen)
    else shootShipLasersAtShip gen shipAfterMissleLaunchers enemyShip
  system' = if isNothing maybeNewMissleSalvo
    then system''
    else L.over ssMissleSalvos ((:) (fromJust maybeNewMissleSalvo)) system''
  system'' = if isNothing maybeEnemyShip'
    then L.over ssShips (Map.delete enemyId) system
    else L.over ssShips (Map.insert enemyId $ fromJust maybeEnemyShip') system

processShipAttackNearestShipOrder
  :: RandomGen g
  => g
  -> Seconds
  -> Bool
  -> StarSystem
  -> Ship
  -> (Ship, StarSystem, g)
processShipAttackNearestShipOrder gen secs conditional system ship =
  (newShp, newSystem, newGen) where
  maybeEnemyShip              = getNearestEnemyShip ship system
  (newShp, newSystem, newGen) = if isNothing maybeEnemyShip
    then (completeOrder conditional ship, system, gen) -- Make sure we remove the invalid order
    else processShipAttackShipOrder gen
                                    secs
                                    conditional
                                    sid
                                    system
                                    newAttackingOrder
  sid               = view sId (fromJust maybeEnemyShip)
  newAttackingOrder = replaceShipActiveOrder (AttackShipOrder sid) ship

processShipContinuousAttackNearestShipOrder
  :: RandomGen g
  => g
  -> Seconds
  -> Bool
  -> StarSystem
  -> Ship
  -> (Ship, StarSystem, g)
processShipContinuousAttackNearestShipOrder gen secs conditional system ship =
  (newShp, newSystem, newGen) where
  maybeEnemyShip              = getNearestEnemyShip ship system
  (newShp, newSystem, newGen) = if isNothing maybeEnemyShip
    then (completeOrder conditional ship, system, gen) -- Make sure we remove the invalid order
    else processShipAttackShipOrder gen secs conditional sid system ship
  sid = view sId (fromJust maybeEnemyShip)

processShipAttackBodyOrder
  :: Seconds -> Bool -> BodyId -> StarSystem -> Ship -> (Ship, StarSystem)
processShipAttackBodyOrder secs conditional bid system ship =
  (newShp, newSystem) where
  maybeBody           = Map.lookup bid (view ssBodies system)
  (newShp, newSystem) = if isNothing maybeBody
    then (completeOrder conditional ship, system) -- Make sure we remove the invalid order
    else missleLauncherCheck
  maybeMissleLaunchers = view sShipMissleLaunchers ship
  missleLauncherCheck  = if isNothing maybeMissleLaunchers
    then (completeOrder conditional ship, system) -- Make sure we remove the invalid order
    else locationCheck
  (ShipLocation sX sY) = view sLocation ship
  currentBodyDistance  = sqrt $ ((bX - sX) ** 2) + ((bY - sY) ** 2)
  body                 = fromJust maybeBody
  (BodyLocation bX bY) = view bLocation body
  -- TODO: Need to allow lasers to attack body as well.
  movedShip            = if currentBodyDistance < missleRange
    then ship
    else moveShip secs bX bY ship
  (ShipLocation msX msY) = view sLocation movedShip
  (MissleRange missleRange) =
    view (smlSalvo . msRange) (fromJust maybeMissleLaunchers)
  newBodyDistance = sqrt $ ((bX - msX) ** 2) + ((bY - msY) ** 2)
  locationCheck   = if newBodyDistance > missleRange
    then (movedShip, system)
    else (firedShip, system')
  salvoTarget = MissleSalvoTarget BodyTarget (view bId body)
  (firedShip, maybeNewMissleSalvo) = missleSalvoFromShip movedShip salvoTarget
  system' = if isNothing maybeNewMissleSalvo
    then system
    else L.over ssMissleSalvos ((:) (fromJust maybeNewMissleSalvo)) system
