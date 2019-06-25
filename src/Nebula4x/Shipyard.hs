module Nebula4x.Shipyard where

import           Control.Lens
import           Control.Newtype.Generics
import qualified Data.Map.Strict               as Map
                                         hiding ( drop )
import           Data.Maybe

import           Nebula4x.Mineral
import           Nebula4x.Ship
import           Nebula4x.Time                  ( yearInSeconds )
import           Nebula4x.Types

addCapacityCostRatio :: MineralCost
addCapacityCostRatio = Map.fromList [(Duranium, 0.5), (Neutronium, 0.5)]

-- Mineral cost breakdown of adding slipway to shipyards.
addSlipwayCostRatio :: MineralCost
addSlipwayCostRatio = Map.fromList [(Duranium, 0.5), (Neutronium, 0.5)]

noShipyards :: Shipyards
noShipyards = Map.empty

newShipyard :: ShipyardId -> ShipyardName -> ShipyardType -> Shipyard
newShipyard syid sn st = Shipyard
  syid
  sn
  Nothing
  [ ShipyardSlipway False (TaskProgress 0) Nothing
  , ShipyardSlipway False (TaskProgress 0) Nothing
  ]
  (ShipyardCapacity 50)
  st
  (ShipyardBuildRate 400)
  Nothing
  []

addBodyCommercialShipyard :: Body -> Body
addBodyCommercialShipyard bdy = newBody
 where
  newBody       = bdy { _bShipyards = newSYs }
  newSYs        = Map.insert (view syId newSY) newSY bodyShipyards
  bodyShipyards = view bShipyards bdy
  newSY = newShipyard sid
                      (ShipyardName $ bodyName ++ " Commercial Shipyard")
                      CommercialShipyard
  sid                 = (length $ Map.keys bodyShipyards) + 1 -- Increment ID by 1
  (BodyName bodyName) = view bName bdy

addBodyNavalShipyard :: Body -> Body
addBodyNavalShipyard bdy = newBody
 where
  newBody       = bdy { _bShipyards = newSYs }
  newSYs        = Map.insert (view syId newSY) newSY bodyShipyards
  bodyShipyards = view bShipyards bdy
  newSY =
    newShipyard sid (ShipyardName $ bodyName ++ " Naval Shipyard") NavyShipyard
  sid                 = (length $ Map.keys bodyShipyards) + 1 -- Increment ID by 1
  (BodyName bodyName) = view bName bdy

shipyardIdle :: Shipyard -> Bool
shipyardIdle sy = length activeSlipways < 1
  where activeSlipways = Prelude.filter (view sysActive) (view sySlipways sy)

shipyardFull :: Shipyard -> Bool
shipyardFull sy = length idleSlipways < 1
 where
  idleSlipways = Prelude.filter (not . view sysActive) (view sySlipways sy)

setShipyardTask :: ShipyardTask -> Shipyard -> Shipyard
setShipyardTask = set syCurrentTask . Just

processShipyardTask
  :: Seconds -> AvailableMinerals -> Shipyard -> (AvailableMinerals, Shipyard)
processShipyardTask _ funds sy@(Shipyard _ _ _ _ _ _ _ Nothing _) = (funds, sy)
processShipyardTask secs funds sy = (newFunds, newShipyard')
 where
  task                     = fromJust $ view syCurrentTask sy
  (newFunds, newShipyard') = case task of
    ShipyardRetool sd rc rr pg pgr ->
      processShipyardRetool secs funds sy sd rc rr pg pgr
    ShipyardAddCapacity tcap scap rr cr ->
      processShipyardCapacity secs funds sy tcap scap rr cr
    ShipyardAddSlipway cr pgr pg ->
      processShipyardSlipway secs funds sy cr pgr pg

--
-- Building
--
addShipyardBuild :: ShipName -> Shipyard -> Shipyard
addShipyardBuild sn sy = newShipyard'
 where
  newShipyard' = if shipyardFull sy
    then set syBuildQueue newBuildQueue sy
    else set sySlipways newSlipways sy
  newBuildQueue = view syBuildQueue sy ++ [sn]
  newSlipways   = activateSlipway sn (view sySlipways sy)

activateSlipway :: ShipName -> [ShipyardSlipway] -> [ShipyardSlipway]
activateSlipway _  []         = []
activateSlipway sn (sw : sws) = if view sysActive sw
  then sw : activateSlipway sn sws
  else set sysShipName (Just sn) (set sysActive True sw) : sws

processShipyardBuild
  :: Seconds
  -> AvailableMinerals
  -> Shipyard
  -> (AvailableMinerals, Shipyard, [Ship])
processShipyardBuild seconds funds shipyard =
  (newFunds, newShipyard', newShips)
 where
  shipDesign                        = view syShipDesign shipyard
  (newFunds, newSlipways, newShips) = case shipDesign of
    Just sd -> processSlipways seconds
                               funds
                               (view syBuildRate shipyard)
                               sd
                               []
                               (view sySlipways shipyard)
                               []
    Nothing -> (funds, view sySlipways shipyard, [])
  newShipyard' = set sySlipways newSlipways shipyard

processShipyardQueue :: Shipyard -> Shipyard
processShipyardQueue = until
  (\sy' -> null (view syBuildQueue sy') || shipyardFull sy')
  processShipyardQueue'

processShipyardQueue' :: Shipyard -> Shipyard
processShipyardQueue' sy = newShipyard'
 where
  buildQueue   = view syBuildQueue sy
  newShipyard' = if not (null buildQueue) && not (shipyardFull sy)
    then sy { _syBuildQueue = tail buildQueue, _sySlipways = newSlipways }
    else sy
  currentSlipways = view sySlipways sy
  newSlipways     = activateSlipway (head buildQueue) currentSlipways

processSlipways
  :: Seconds
  -> AvailableMinerals
  -> ShipyardBuildRate
  -> ShipDesign
  -> [Ship]
  -> [ShipyardSlipway]
  -> [ShipyardSlipway]
  -> (AvailableMinerals, [ShipyardSlipway], [Ship])
processSlipways _ funds _ _ shps [] acc = (funds, acc, shps)
processSlipways seconds funds buildRate shipDesign shps (slipway : slipways) acc
  = results
 where
  results = if view sysActive slipway
    then processSlipways seconds
                         newFunds
                         buildRate
                         shipDesign
                         newShips
                         slipways
                         newSlipways
    else processSlipways seconds
                         funds
                         buildRate
                         shipDesign
                         shps
                         slipways
                         (acc ++ [slipway])
  (newFunds, sw, maybeShip) =
    processSlipway seconds funds shipDesign buildRate slipway
  newSlipways = acc ++ [sw]
  newShips    = case maybeShip of
    Just ship -> shps ++ [ship]
    Nothing   -> shps

-- The larger the ship, the faster the build rate.
-- Else we'd have to wait decades on bigger ships.
-- TODO: Need to figure out why we need to divide by 50 to make it reasonable.
-- Probably has something to do with me treating ship size (HS) as tonnage.
adjustedBuildRate :: Double -> ShipSize -> ShipClass -> Double
adjustedBuildRate buildRate (ShipSize shipSize) CommercialClass =
  buildRate * (1 + (((shipSize / 400) - 1) / 2)) / 50
adjustedBuildRate buildRate (ShipSize shipSize) MilitaryClass =
  buildRate * (1 + (((shipSize / 100) - 1) / 2)) / 50

adjustedCapacityRate :: Double -> Double -> ShipyardType -> Double
adjustedCapacityRate buildRate currentCapacity CommercialShipyard =
  buildRate * (1 + (((currentCapacity / 400) - 1) / 2))
adjustedCapacityRate buildRate currentCapacity NavyShipyard =
  buildRate * (1 + (((currentCapacity / 100) - 1) / 2))

adjustedRetoolRate :: Double -> ShipSize -> ShipClass -> Double
adjustedRetoolRate = adjustedBuildRate

adjustedSlipwayRate :: Double -> Double -> ShipyardType -> Double
adjustedSlipwayRate buildRate currentCapacity CommercialShipyard =
  buildRate * (1 + (((currentCapacity / 400) - 1) / 2)) / 50
adjustedSlipwayRate buildRate currentCapacity NavyShipyard =
  buildRate * (1 + (((currentCapacity / 100) - 1) / 2)) / 50

processSlipway
  :: Seconds
  -> AvailableMinerals
  -> ShipDesign
  -> ShipyardBuildRate
  -> ShipyardSlipway
  -> (AvailableMinerals, ShipyardSlipway, Maybe Ship)
processSlipway seconds funds shipDesign shipyardBuildRate slipway =
  (newFunds, newSlipway, maybeShip)
 where
  secs          = fromIntegral seconds :: Double
  addedProgress = progressRate * secs
  buildRate     = unpack shipyardBuildRate / fromIntegral yearInSeconds
  adjustedBR    = adjustedBuildRate buildRate
                                    (view sdSize shipDesign)
                                    (view sdClass shipDesign)
  progressRate    = adjustedBR / totalBuildCost * 100
  shipBuildCost   = view sdBuildCost shipDesign
  totalBuildCost  = Map.foldr' (+) 0.0 shipBuildCost
  currentProgress = unpack $ view sysProgress slipway
  newProgress     = currentProgress + addedProgress
  secondsNeeded   = minimum [(1 - currentProgress) / progressRate, secs]
  totalCost =
    Map.map (\cost -> cost * progressRate * secondsNeeded) shipBuildCost
  ((newFunds, newSlipway), maybeShip) = if canAfford totalCost funds
    then ((expendMinerals totalCost funds, updatedSlipway), possiblyShip)
    else ((funds, slipway), Nothing)
  (updatedSlipway, possiblyShip) = if newProgress >= 1
    then
      ( slipway { _sysActive   = False
                , _sysProgress = TaskProgress 0
                , _sysShipName = Nothing
                }
      , Just $ newShip 0
                       0
                       (fromMaybe (ShipName "") $ view sysShipName slipway)
                       (ShipLocation 0 0)
                       shipDesign
      )
    else (slipway { _sysProgress = TaskProgress newProgress }, Nothing)

--
-- Retooling
--
getShipyardRetoolTask :: ShipDesign -> Shipyard -> ShipyardTask
getShipyardRetoolTask sd shipyard = retoolTask
 where
  retoolTask = ShipyardRetool sd
                              totalMineralCost
                              mineralRetoolRate
                              (TaskProgress 0)
                              (TaskProgress progressRate)
  totalRetoolCost               = retoolingCost sd shipyard
  totalMineralCost              = retoolingMineralCost totalRetoolCost sd
  (ShipyardBuildRate buildRate) = view syBuildRate shipyard
  adjBuildRate =
    adjustedRetoolRate buildRate (view sdSize sd) (view sdClass sd)
  retoolRate        = adjBuildRate / fromIntegral yearInSeconds
  progressRate      = retoolRate / totalRetoolCost * 100
  mineralRetoolRate = Map.map (* progressRate) totalMineralCost

processShipyardRetool
  :: Seconds
  -> AvailableMinerals
  -> Shipyard
  -> ShipDesign
  -> MineralCost
  -> MineralCost
  -> TaskProgress
  -> TaskProgress
  -> (AvailableMinerals, Shipyard)
processShipyardRetool seconds funds shipyard sd researchCost researchRate (TaskProgress progress) tpr@(TaskProgress progressRate)
  = (newFunds, newShipyard')
 where
  secs                     = fromIntegral seconds :: Double
  addedProgress            = progressRate * secs
  newProgress              = progress + addedProgress
  secondsNeeded            = minimum [(1 - progress) / progressRate, secs]
  totalCost                = Map.map (* secondsNeeded) researchRate
  (newFunds, newShipyard') = if canAfford totalCost funds
    then (expendMinerals totalCost funds, updatedShipyard)
    else (funds, shipyard)
  updatedShipyard = if newProgress >= 1
    then shipyard { _syShipDesign = Just sd, _syCurrentTask = Nothing }
    else shipyard { _syCurrentTask = Just updatedTask }
  updatedTask =
    ShipyardRetool sd researchCost researchRate (TaskProgress newProgress) tpr

retoolingCost :: ShipDesign -> Shipyard -> Double
retoolingCost shipDesign shipyard = retCost
 where
  retCost =
    0.5 * totalBuildCost + 0.25 * fromIntegral numSlipways * totalBuildCost
  shipBuildCost  = view sdBuildCost shipDesign
  totalBuildCost = Map.foldr' (+) 0.0 shipBuildCost
  numSlipways    = length $ view sySlipways shipyard

retoolingMineralCost :: Double -> ShipDesign -> MineralCost
retoolingMineralCost retoolingCost' shipDesign = retoolMineralCost
 where
  shipBuildCost     = view sdBuildCost shipDesign
  shipMineralCost   = Map.foldr' (+) 0.0 shipBuildCost
  shipMineralRatio  = Map.map (/ shipMineralCost) shipBuildCost
  retoolMineralCost = Map.map (* retoolingCost') shipMineralRatio

canRetoolToDesign :: ShipDesign -> Shipyard -> Bool
canRetoolToDesign shipDesign shipyard = allowed
 where
  allowed = shipyardIdle shipyard && shipyardBigEnough && shipyardRightType
  shipyardBigEnough =
    unpack (view syCapacity shipyard) >= unpack (view sdSize shipDesign)
  shipyardType    = view syType shipyard
  shipDesignClass = view sdClass shipDesign
  shipyardRightType =
    (shipyardType == CommercialShipyard && shipDesignClass == CommercialClass)
      || shipyardType
      == NavyShipyard

beginRetool :: ShipDesign -> Shipyard -> Shipyard
beginRetool sd sy =
  sy { _syCurrentTask = Just (getShipyardRetoolTask sd sy), _syBuildQueue = [] }

--
-- Adding Capacity
--
-- For navy shipyards, each 20 capacity (ship tons supported) costs 240 build points (and resources) per shipyard.
-- Commercial shipyards cost a tenth the resources and time.
getShipyardCapacityTask :: CapacityAmount -> Shipyard -> ShipyardTask
getShipyardCapacityTask cap shipyard = capacityTask
 where
  capacityTask = ShipyardAddCapacity (ShipyardCapacity targetCapacity)
                                     (ShipyardCapacity capacity)
                                     mineralCapacityRate
                                     (TaskRate capacityPerSecond)
  capacity       = unpack $ view syCapacity shipyard
  targetCapacity = capacity + (fromIntegral cap)
  slipways       = fromIntegral $ length (view sySlipways shipyard)
  navy           = case view syType shipyard of
    CommercialShipyard -> False
    NavyShipyard       -> True
  capacityBuildPointRatio       = 0.08333333333 :: Double -- 20 capacity / 240 build points
  shipyardMultiplier            = if navy then 1 else 10
  (ShipyardBuildRate buildRate) = view syBuildRate shipyard
  adjBuildRate = adjustedCapacityRate buildRate capacity (view syType shipyard)
  capacityRate =
    adjBuildRate / fromIntegral yearInSeconds / slipways * shipyardMultiplier
  capacityPerSecond   = capacityRate * capacityBuildPointRatio
  mineralCapacityRate = Map.map
    (* (capacityRate / shipyardMultiplier * slipways))
    addCapacityCostRatio

processShipyardCapacity
  :: Seconds
  -> AvailableMinerals
  -> Shipyard
  -> ShipyardCapacity
  -> ShipyardCapacity
  -> MineralCost
  -> TaskRate
  -> (AvailableMinerals, Shipyard)
processShipyardCapacity secs funds sy (ShipyardCapacity targetCap) _ researchRate (TaskRate cr)
  = (newFunds, newShipyard')
 where
  currentCapacity          = unpack $ view syCapacity sy
  capacityLeft             = targetCap - currentCapacity
  secondsNeeded            = minimum [capacityLeft / cr, fromIntegral secs]
  totalCost                = Map.map (* secondsNeeded) researchRate
  capacityToAdd            = cr * secondsNeeded
  (newFunds, newShipyard') = if canAfford totalCost funds
    then (expendMinerals totalCost funds, updatedShipyard)
    else (funds, sy)
  newCapacity     = currentCapacity + capacityToAdd
  updatedShipyard = if newCapacity >= targetCap
    then sy { _syCapacity    = ShipyardCapacity newCapacity
            , _syCurrentTask = Nothing
            }
    else sy { _syCapacity = ShipyardCapacity newCapacity }

beginCapacityExpand :: CapacityAmount -> Shipyard -> Shipyard
beginCapacityExpand capacity sy =
  sy { _syCurrentTask = Just (getShipyardCapacityTask capacity sy) }

--
-- Adding Slipway
--
-- For navy shipyards, each 20 capacity adds 240 build points (and resources) for a new slipway.
-- Commercial shipyards cost a tenth the resources and time.
getShipyardSlipwayTask :: Shipyard -> ShipyardTask
getShipyardSlipwayTask shipyard = addSlipwayTask
 where
  addSlipwayTask = ShipyardAddSlipway mineralRate
                                      (TaskProgress addSlipwayRatePerSecond)
                                      (TaskProgress 0)
  navy = case view syType shipyard of
    CommercialShipyard -> False
    NavyShipyard       -> True
  buildPointRatio               = 0.08333333333 :: Double -- 20 capacity / 240 build points
  shipyardMultiplier            = if navy then 1 else 10
  (ShipyardBuildRate buildRate) = view syBuildRate shipyard
  capacity                      = unpack $ view syCapacity shipyard
  adjBuildRate = adjustedSlipwayRate buildRate capacity (view syType shipyard)
  addSlipwayRate =
    adjBuildRate / fromIntegral yearInSeconds * shipyardMultiplier
  addSlipwayRatePerSecond = addSlipwayRate * buildPointRatio
  mineralRate = Map.map (* (addSlipwayRate / shipyardMultiplier * capacity))
                        addSlipwayCostRatio

processShipyardSlipway
  :: Seconds
  -> AvailableMinerals
  -> Shipyard
  -> MineralCost
  -> TaskProgress
  -> TaskProgress
  -> (AvailableMinerals, Shipyard)
processShipyardSlipway seconds funds shipyard costRate (TaskProgress progressRate) (TaskProgress progress)
  = (newFunds, newShipyard')
 where
  secs                     = fromIntegral seconds :: Double
  addedProgress            = progressRate * secs
  newProgress              = progress + addedProgress
  secondsNeeded            = minimum [(1 - progress) / progressRate, secs]
  totalCost                = Map.map (* secondsNeeded) costRate
  (newFunds, newShipyard') = if canAfford totalCost funds
    then (expendMinerals totalCost funds, updatedShipyard)
    else (funds, shipyard)
  updatedShipyard = if newProgress >= 1
    then shipyard { _sySlipways = newSlipways, _syCurrentTask = Nothing }
    else shipyard { _syCurrentTask = Just updatedTask }
  newSlipways =
    view sySlipways shipyard ++ [ShipyardSlipway False (TaskProgress 0) Nothing]
  updatedTask = ShipyardAddSlipway costRate
                                   (TaskProgress progressRate)
                                   (TaskProgress newProgress)

beginAddSlipway :: Shipyard -> Shipyard
beginAddSlipway sy = sy { _syCurrentTask = Just (getShipyardSlipwayTask sy) }
