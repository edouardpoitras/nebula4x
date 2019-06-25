{-# LANGUAGE ScopedTypeVariables #-}

module Nebula4x.Body.Body where

import           Control.Lens
import           Control.Monad
import           Control.Newtype.Generics hiding (over)
import           Data.Fixed                     ( mod' )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     (fromJust, isJust)
import           System.Random

import           Nebula4x.Config

import           Nebula4x.Installment
import           Nebula4x.Mineral
import           Nebula4x.Ship

import           Nebula4x.Shipyard
import           Nebula4x.Time
import           Nebula4x.Types
import           Nebula4x.Utils

rollNumPlanets :: RandomGen g => g -> StarSystemConfig -> (Int, g)
rollNumPlanets g ssConf = (planetCount'', newGen)
 where
  (g', newGen)                  = split g
  randomChances                 = randomRs (0.0, 1.0) g'
  generationChance              = unpack $ view sscPlanetGenerationChance ssConf
  (Range minPlanets maxPlanets) = view sscPlanets ssConf
  planetCount = length $ takeWhile (< generationChance) randomChances
  planetCount'                  = minimum [planetCount, maxPlanets]
  planetCount''                 = maximum [planetCount', minPlanets]

rollNumMoons :: RandomGen g => g -> BodyConfig -> (Int, g)
rollNumMoons g bConf = (moonCount', newGen)
 where
  (g', newGen)     = split g
  randomChances    = randomRs (0.0, 1.0) g'
  generationChance = unpack $ view bcMoonGenerationChance bConf
  moonCount        = length $ takeWhile (< generationChance) randomChances
  moonCount'       = minimum [moonCount, view bcMaxMoons bConf]

rollNumComets :: RandomGen g => g -> CometConfig -> (Int, g)
rollNumComets g ccConf = (cometCount'', newGen)
 where
  (g', newGen)                = split g
  randomChances               = randomRs (0.0, 1.0) g'
  generationChance            = unpack $ view ccGenerationChance ccConf
  (Range minComets maxComets) = view ccComets ccConf
  cometCount = length $ takeWhile (< generationChance) randomChances
  cometCount'                 = minimum [cometCount, maxComets]
  cometCount''                = maximum [cometCount', minComets]

rollNumAsteroidBelts :: RandomGen g => g -> AsteroidConfig -> (Int, g)
rollNumAsteroidBelts g acConf = (beltCount'', newGen)
 where
  (g', newGen)              = split g
  randomChances             = randomRs (0.0, 1.0) g'
  generationChance          = unpack $ view acBeltGenerationChance acConf
  (Range minBelts maxBelts) = view acAsteroidBelts acConf
  beltCount = length $ takeWhile (< generationChance) randomChances
  beltCount'                = minimum [beltCount, maxBelts]
  beltCount''               = maximum [beltCount', minBelts]

rollNumAsteroids :: RandomGen g => g -> AsteroidConfig -> (Int, g)
rollNumAsteroids g acConf = (asteroidCount'', newGen)
 where
  (g', newGen)                      = split g
  randomChances                     = randomRs (0.0, 1.0) g'
  generationChance                  = unpack $ view acGenerationChance acConf
  (Range minAsteroids maxAsteroids) = view acAsteroidsPerBelt acConf
  asteroidCount = length $ takeWhile (< generationChance) randomChances
  asteroidCount'                    = minimum [asteroidCount, maxAsteroids]
  asteroidCount''                   = maximum [asteroidCount', minAsteroids]

generatePlanetsAndMoons :: RandomGen g => g -> Config -> (Bodies, g)
generatePlanetsAndMoons g conf =
  (Map.union generatedPlanets generatedMoons, planetsG)
 where
  ssConfig                  = view starSystemConfig conf
  (numPlanets, numPlanetsG) = rollNumPlanets g ssConfig
  (generatedPlanets, generatedMoons, planetsG) =
    generatePlanets numPlanetsG conf numPlanets Map.empty Map.empty

generatePlanets
  :: RandomGen g
  => g
  -> Config
  -> Int
  -> Bodies
  -> Bodies
  -> (Bodies, Bodies, g)
generatePlanets g conf num generatedPs generatedMs = results
 where
  results = if num < 1
    then (generatedPs, generatedMs, g)
    else generatePlanets planetG conf (num - 1) newGeneratedPs newGeneratedMs
  (planet, moons, planetG) = generatePlanet g conf
  newGeneratedPs           = Map.insert (view bId planet) planet generatedPs
  newGeneratedMs           = Map.union generatedMs moons

generatePlanet :: RandomGen g => g -> Config -> (Body, Bodies, g)
generatePlanet g conf = (newPlanet, moons, moonsG)
 where
  (bid       , bidG )   = randomId g
  (name      , nameG)   = generateBodyName bidG
  (planetType, ptG  )   = rollPlanetType nameG
  bodyConfig = fromJust $ Map.lookup planetType (view bodyConfigs conf)
  moonConfig            = fromJust $ Map.lookup Moon (view bodyConfigs conf)
  (mass    , massG    ) = generateBodyMass ptG bodyConfig
  (radius  , radiusG  ) = generateBodyRadius massG bodyConfig
  (distance, distanceG) = generateBodyOrbitalDistance radiusG bodyConfig
  (minerals, mineralsG) = generateBodyMinerals distanceG bodyConfig
  (moons, moonsG) =
    generatePlanetMoons mineralsG newPlanet bodyConfig moonConfig
  newPlanet = Body bid
                   noRace
                   (BodyName name)
                   (BodyMass mass)
                   (BodyLocation 0 0)
                   (BodyRadius radius)
                   distance
                   planetType
                   Map.empty
                   (FuelReserves 0)
                   notRefining
                   noInstallments
                   minerals
                   Nothing
                   noSurveys
                   Nothing

generatePlanetMoons
  :: RandomGen g => g -> Body -> BodyConfig -> BodyConfig -> (Bodies, g)
generatePlanetMoons g pBody bConf mConf = (moons, moonsG)
 where
  (numMoons, numMoonsG) = rollNumMoons g bConf
  (moons, moonsG) =
    generateMoons numMoonsG pBody bConf mConf numMoons Map.empty

generateMoons
  :: RandomGen g
  => g
  -> Body
  -> BodyConfig
  -> BodyConfig
  -> Int
  -> Bodies
  -> (Bodies, g)
generateMoons g pBody bConf mConf num generated = results
 where
  results = if num < 1
    then (generated, g)
    else generateMoons moonG pBody bConf mConf (num - 1) newGenerated
  (moon, moonG) = generateMoon g num pBody bConf mConf
  newGenerated  = Map.insert (view bId moon) moon generated

generateMoon
  :: RandomGen g => g -> Int -> Body -> BodyConfig -> BodyConfig -> (Body, g)
generateMoon g idx pBody bConf mConf = (newMoon, mineralsG)
 where
  (bid, bidG)           = randomId g
  (BodyName planetName) = view bName pBody
  name                  = planetName ++ " - " ++ show idx
  (mass    , massG    ) = generateMoonMass bidG pBody bConf mConf
  (radius  , radiusG  ) = generateBodyRadius massG mConf
  (distance, distanceG) = generateBodyOrbitalDistance radiusG mConf
  (minerals, mineralsG) = generateBodyMinerals distanceG mConf
  newMoon               = Body bid
                               noRace
                               (BodyName name)
                               (BodyMass mass)
                               (BodyLocation 0 0)
                               (BodyRadius radius)
                               distance
                               Moon
                               Map.empty
                               (FuelReserves 0)
                               notRefining
                               noInstallments
                               minerals
                               Nothing
                               noSurveys
                               (Just $ view bId pBody)

generateComets :: RandomGen g => g -> Config -> (Bodies, g)
generateComets g conf = (generatedComets, cometsG)
 where
  ccConfig                = view cometConfig conf
  (numComets, numCometsG) = rollNumComets g ccConfig
  (generatedComets, cometsG) =
    generateComets' numCometsG conf numComets Map.empty

generateComets' :: RandomGen g => g -> Config -> Int -> Bodies -> (Bodies, g)
generateComets' g conf num generated = results
 where
  results = if num < 1
    then (generated, g)
    else generateComets' cometG conf (num - 1) newGenerated
  (comet, cometG) = generateComet g conf
  newGenerated    = Map.insert (view bId comet) comet generated

generateComet :: RandomGen g => g -> Config -> (Body, g)
generateComet g conf = (newComet, mineralsG)
 where
  (bid , bidG )         = randomId g
  (name, nameG)         = generateBodyName bidG
  bodyConfig            = fromJust $ Map.lookup Comet (view bodyConfigs conf)
  (mass    , massG    ) = generateBodyMass nameG bodyConfig
  (radius  , radiusG  ) = generateBodyRadius massG bodyConfig
  (distance, distanceG) = generateBodyOrbitalDistance radiusG bodyConfig
  (minerals, mineralsG) = generateBodyMinerals distanceG bodyConfig
  newComet              = Body bid
                               noRace
                               (BodyName name)
                               (BodyMass mass)
                               (BodyLocation 0 0)
                               (BodyRadius radius)
                               distance
                               Comet
                               Map.empty
                               (FuelReserves 0)
                               notRefining
                               noInstallments
                               minerals
                               Nothing
                               noSurveys
                               Nothing

generateAsteroidBelts :: RandomGen g => g -> Config -> (Bodies, g)
generateAsteroidBelts g conf = (generatedAsteroidBelts, asteroidBeltsG)
 where
  acConfig              = view asteroidConfig conf
  (numBelts, numBeltsG) = rollNumAsteroidBelts g acConfig
  (generatedAsteroidBelts, asteroidBeltsG) =
    generateAsteroidBelts' numBeltsG conf numBelts Map.empty

generateAsteroidBelts'
  :: RandomGen g => g -> Config -> Int -> Bodies -> (Bodies, g)
generateAsteroidBelts' g conf num generated = results
 where
  results = if num < 1
    then (generated, g)
    else generateAsteroidBelts' asteroidBeltG conf (num - 1) newGeneratedBelt
  (asteroids, asteroidBeltG) = generateAsteroids g conf
  newGeneratedBelt           = Map.union generated asteroids

generateAsteroids :: RandomGen g => g -> Config -> (Bodies, g)
generateAsteroids g conf = (generatedAsteroids, asteroidsG)
 where
  acConf                        = view asteroidConfig conf
  bodyConfig = fromJust $ Map.lookup Asteroid (view bodyConfigs conf)
  (numAsteroids, numAsteroidsG) = rollNumAsteroids g acConf
  (beltName    , nameG        ) = generateBodyName numAsteroidsG
  (Range _ beltDistance, distanceG) =
    generateBodyOrbitalDistance nameG bodyConfig
  (generatedAsteroids, asteroidsG) = generateAsteroids' distanceG
                                                        beltName
                                                        beltDistance
                                                        conf
                                                        numAsteroids
                                                        Map.empty

generateAsteroids'
  :: RandomGen g
  => g
  -> String
  -> Double
  -> Config
  -> Int
  -> Bodies
  -> (Bodies, g)
generateAsteroids' g beltName beltDistance conf num generated = results
 where
  results = if num < 1
    then (generated, g)
    else generateAsteroids' asteroidG
                            beltName
                            beltDistance
                            conf
                            (num - 1)
                            newGenerated
  (asteroid, asteroidG) = generateAsteroid g beltName beltDistance num conf
  newGenerated          = Map.insert (view bId asteroid) asteroid generated

generateAsteroid
  :: RandomGen g => g -> String -> Double -> Int -> Config -> (Body, g)
generateAsteroid g beltName beltDistance idx conf = (newAsteroid, mineralsG)
 where
  (bid, bidG)       = randomId g
  name              = beltName ++ " - " ++ show idx
  bodyConfig        = fromJust $ Map.lookup Asteroid (view bodyConfigs conf)
  (mass  , massG  ) = generateBodyMass bidG bodyConfig
  (radius, radiusG) = generateBodyRadius massG bodyConfig
  acConf            = view asteroidConfig conf
  (distance, distanceG) =
    generateAsteroidOrbitalDistance radiusG beltDistance acConf
  (minerals, mineralsG) = generateBodyMinerals distanceG bodyConfig
  newAsteroid           = Body bid
                               noRace
                               (BodyName name)
                               (BodyMass mass)
                               (BodyLocation 0 0)
                               (BodyRadius radius)
                               distance
                               Asteroid
                               Map.empty
                               (FuelReserves 0)
                               notRefining
                               noInstallments
                               minerals
                               Nothing
                               noSurveys
                               Nothing

generateBodyMass :: RandomGen g => g -> BodyConfig -> (Double, g)
generateBodyMass g bConf = randomR (minM, maxM) g
  where (Range (BodyMass minM) (BodyMass maxM)) = view bcMass bConf

generateMoonMass
  :: RandomGen g => g -> Body -> BodyConfig -> BodyConfig -> (Double, g)
generateMoonMass g pBody bConf mConf = randomR (adjustedMinM, adjustedMaxM) g
 where
  (BodyMass pBodyMass) = view bMass pBody
  (MassRatio massRatio) = view bcMaxMoonMassRelativeToParent bConf
  (Range (BodyMass minM) (BodyMass maxM)) = view bcMass mConf
  adjustedMaxM = minimum [maxM, pBodyMass * massRatio]
  adjustedMinM = minimum [minM, adjustedMaxM]

generateBodyRadius :: RandomGen g => g -> BodyConfig -> (Double, g)
generateBodyRadius g bConf = randomR (minR, maxR) g
  where (Range (BodyRadius minR) (BodyRadius maxR)) = view bcRadius bConf

generateBodyOrbitalDistance
  :: RandomGen g => g -> BodyConfig -> (OrbitalDistance, g)
generateBodyOrbitalDistance g bConf = (Range minDistance maxDistance, minG)
 where
  (Range minOD maxOD) = view bcOrbitalDistance bConf
  (maxDistance, maxG) = randomR (minOD, maxOD) g
  (minDistance, minG) = case view bcType bConf of
    Comet -> randomR (maxDistance * 0.1, maxDistance * 0.5) maxG
    _     -> randomR (maxDistance * 0.9, maxDistance) maxG

generateAsteroidOrbitalDistance
  :: RandomGen g => g -> Double -> AsteroidConfig -> (OrbitalDistance, g)
generateAsteroidOrbitalDistance g beltDistance acConf =
  (Range minDistance maxDistance, devG)
 where
  (Range (OrbitDeviation minDeviation) (OrbitDeviation maxDeviation)) =
    view acAsteroidOrbitDeviation acConf
  (deviationAmount, devG) = randomR (minDeviation, maxDeviation) g
  multiplier              = beltDistance * deviationAmount
  minDistance             = beltDistance - multiplier
  maxDistance             = beltDistance + multiplier

rollPlanetType :: RandomGen g => g -> (BodyType, g)
rollPlanetType g = (pType, newG)
 where
  (randomRange, newG) = randomR (0 :: Int, 4 :: Int) g
  pType               = case randomRange of
    0 -> DwarfPlanet
    1 -> GasDwarf
    2 -> GasGiant
    3 -> IceGiant
    _ -> Terrestrial

updateBodyPosition :: BodyLocation -> BodyMass -> Seconds -> Body -> Body
updateBodyPosition (BodyLocation pbX pbY) (BodyMass pbM) time p@(Body _ _ _ _ _ _ (Range rmin rmax) btype _ _ _ _ _ _ _ _)
  = body'
 where
  time' = case btype of
    Asteroid -> time + (floor $ rmax - rmin)
    --Comet    -> time + (floor rmin)
    _        -> time
  (x, y) = getBodyCoordinates pbM rmin rmax time'
  body'  = set bLocation (BodyLocation (x + pbX) (y + pbY)) p

updateBodyProduction
  :: Seconds -> BodyProduction -> Body -> (BodyProduction, Body)
updateBodyProduction secs bodyProd body = (newBodyProduction, newBody)
 where
  (newBodyProduction, body') =
    foldr (updateBodyProduction' secs) (bodyProd, body) (Map.keys bodyProd)
  newBody = refineFuel secs body'

updateBodyProduction'
  :: Seconds
  -> InstallmentId
  -> (BodyProduction, Body)
  -> (BodyProduction, Body)
updateBodyProduction' secs instId (bodyProd, body) = (newBodyProd, newBody)
 where
  instProd               = fromJust $ Map.lookup instId bodyProd
  (newInstProd, newBody) = processFactories secs instProd body
  newBodyProd            = Map.insert instId newInstProd bodyProd

updateBodyMiners :: Seconds -> Body -> Body
updateBodyMiners secs body@(Body _ _ _ _ _ _ _ _ _ _ _ instllmnts mnrls _ _ _)
  = body { _bMinerals = mineMinerals secs instllmnts mnrls }

generateNewMineralPacket :: Seconds -> Body -> Maybe (MineralPacket, Body)
generateNewMineralPacket secs body = maybeNewMineralPacket
 where
  maybeNewMineralPacket =
    if hasMassDriver && isJust maybeDriverDestination && Map.size minStacks > 0
      then Just (newMineralPacket, newBody)
      else Nothing
  hasMassDriver              = mdRate > 0
  (InstallmentRating mdRate) = totalMassDriverRate (view bInstallments body)
  totalMinerals = mdRate / (fromIntegral yearInSeconds) * (fromIntegral secs)
  mineralsPerElement         = totalMinerals / (fromIntegral numElements)
  numElements                = Map.size bodyMinerals
  maybeDriverDestination     = view bMassDriverBody body
  newMineralPacket           = MineralPacket minStacks
                                             (fromJust maybeDriverDestination)
                                             packetSpeed
                                             packetLocation
  packetSpeed                = PacketSpeed 1000
  packetLocation             = PacketLocation bodyX bodyY
  (BodyLocation bodyX bodyY) = view bLocation body
  bodyMinerals               = view bMinerals body
  minStacks' = Map.map (createMineralStack mineralsPerElement) bodyMinerals
  minStacks =
    Map.filter (\(MineralStack _ (MineralCount cnt)) -> cnt > 0) minStacks'
  newBody     = set bMinerals newMinerals body
  newMinerals = Map.differenceWith subtractMinerals bodyMinerals minStacks
  subtractMinerals m@(Mineral _ _ _ (MineralCount stckpl)) (MineralStack _ (MineralCount cnt))
    = Just (m { _mStockpile = MineralCount (stckpl - cnt) })

-- TODO: Mineral packet will not be at max capacity if stockpile is below requested minerals.
-- Should be smart enough to double-back and pickup more of the other minerals if one stockpile is at 0.
createMineralStack :: Double -> Mineral -> MineralStack
createMineralStack minerals (Mineral elmnt _ _ (MineralCount stckpl)) =
  newMineralStack
 where
  newMineralStack = MineralStack elmnt (MineralCount minCount)
  minCount        = minimum [stckpl, minerals]

-- TODO: Currently calling this once for each body type. Should make this more efficient somehow.
updateBodiesMineralPackets
  :: Seconds -> Bodies -> MineralPackets -> (Bodies, MineralPackets)
updateBodiesMineralPackets secs bodies minPacks =
  (newBodies, newMineralPackets)
 where
  (newBodies, newMineralPackets) =
    foldl (updateBodyMineralPacket secs) (bodies, []) minPacks

updateBodyMineralPacket
  :: Seconds
  -> (Bodies, MineralPackets)
  -> MineralPacket
  -> (Bodies, MineralPackets)
updateBodyMineralPacket secs (bodies, minPackets) minPacket =
  (newBodies, newMineralPackets)
 where
  destBodyId                     = view mpDestinationBody minPacket
  maybeBody                      = Map.lookup destBodyId bodies
  (newBodies, newMineralPackets) = if isJust maybeBody
    then (bodies', minPackets')
    else (bodies, minPacket : minPackets)
  (newBody, maybeMinPacket) =
    moveMineralPacket secs (fromJust maybeBody) minPacket
  bodies'     = Map.insert destBodyId newBody bodies
  minPackets' = if isJust maybeMinPacket
    then (fromJust maybeMinPacket) : minPackets
    else minPackets

moveMineralPacket
  :: Seconds -> Body -> MineralPacket -> (Body, Maybe MineralPacket)
moveMineralPacket secs bdy minPacket = (newBody, maybeNewMinPacket)
 where
  seconds           = fromIntegral secs
  maybeNewMinPacket = if overshot
    then Nothing
    else Just $ minPacket { _mpLocation = PacketLocation newX newY }
  (BodyLocation   moveX     moveY    ) = view bLocation bdy
  (PacketLocation locationX locationY) = view mpLocation minPacket
  newX                 = locationX + cos angle * packetTravelDistance
  newY                 = locationY + sin angle * packetTravelDistance
  (PacketSpeed speed)  = view mpSpeed minPacket
  packetTravelDistance = speed * seconds
  distanceToTravel =
    sqrt $ ((moveX - locationX) ** 2) + ((moveY - locationY) ** 2)
  overshot = packetTravelDistance >= distanceToTravel
  angle    = atan2 (moveY - locationY) (moveX - locationX) :: Double
  newBody  = if overshot then addMineralPacket minPacket bdy else bdy

addMineralPacket :: MineralPacket -> Body -> Body
addMineralPacket minPacket body = newBody
 where
  minStacks           = view mpMineralStacks minPacket
  minStacksToMinerals = Map.map
    (\(MineralStack elm (MineralCount cnt)) ->
      Mineral elm (Accessibility 0.0) (MineralCount 0) (MineralCount cnt)
    )
  bodyMinerals = view bMinerals body
  newMinerals =
    Map.unionWith addMinerals bodyMinerals (minStacksToMinerals minStacks)
  addMinerals bdyM@(Mineral _ _ _ (MineralCount stckpl)) (Mineral _ _ _ (MineralCount cnt))
    = bdyM { _mStockpile = MineralCount (stckpl + cnt) }
  newBody = set bMinerals newMinerals body

updateBodyShipyards :: Seconds -> StarSystemId -> BodyId -> Nebula4x ()
updateBodyShipyards secs ssid bid = do
  gameState <- getGameState
  let body = view (systems . at' ssid . ssBodies . at' bid) gameState
  let maybeRace = view bRace body
  when (isJust maybeRace) $ do
    let minerals = view bMinerals body
    let funds = toMineralCost minerals
    let shipyards = view bShipyards body
    let (BodyLocation bodyX bodyY) = view bLocation body
    let (newFunds, newShipyards, ships') = updateShipyards secs funds shipyards
    let (fueldBody, fueldShips)          = foldr fuelShipHelper (body, []) ships'
    let newBody = fueldBody { _bMinerals  = updateMinerals newFunds minerals
                             , _bShipyards = newShipyards
                             }
    let newShips = map ( set sLocation (ShipLocation bodyX bodyY)
                        . set sRace     (fromJust maybeRace)
                        . addShipOrder (OrbitOrder (view bId newBody) False)
                        )
                        fueldShips
    forM_ newShips $ \shp -> do
      gen <- getGen
      let (shpId, newGen) = randomId gen
      let shp' = set sId shpId shp
      modifyGameState $ over (systems . at' ssid . ssShips) (Map.insert shpId shp')
      setGen newGen
    modifyGameState $ over (systems . at' ssid . ssBodies) (Map.insert bid newBody)

fuelShipHelper :: Ship -> (Body, [Ship]) -> (Body, [Ship])
fuelShipHelper shp (bdy, shps) = let (nBdy, nShp) = fuelShipWithBody bdy shp in (nBdy, nShp : shps)

updateShipyards
  :: Seconds -> MineralCost -> Shipyards -> (MineralCost, Shipyards, [Ship])
updateShipyards secs funds shipyards = (newFunds, updatedShipyards, newShips)
 where
  shipyardKeys = Map.keys shipyards
  (newFunds, updatedShipyards, newShips) =
    updateShipyards' secs funds [] shipyards shipyardKeys Map.empty

updateShipyards'
  :: Seconds
  -> MineralCost
  -> [Ship]
  -> Shipyards
  -> [ShipyardId]
  -> Shipyards
  -> (MineralCost, Shipyards, [Ship])
updateShipyards' _ funds ships' _ [] acc = (funds, acc, ships')
updateShipyards' secs funds ships' shipyards [yardId] acc =
  (newFunds, newShipyards, ships' ++ newShips)
 where
  (newFunds, newShipyard', newShips) =
    updateShipyard secs funds (fromJust $ Map.lookup yardId shipyards)
  newShipyards = Map.insert (view syId newShipyard') newShipyard' acc
updateShipyards' secs funds ships' shipyards (yardId : yIds) acc =
  updateShipyards' secs newFunds (ships' ++ newShips) shipyards yIds newAcc
 where
  (newFunds, newShipyard', newShips) =
    updateShipyard secs funds (fromJust $ Map.lookup yardId shipyards)
  newAcc = Map.insert (view syId newShipyard') newShipyard' acc

updateShipyard
  :: Seconds -> MineralCost -> Shipyard -> (MineralCost, Shipyard, [Ship])
updateShipyard secs funds shipyard = (newFunds, newShipyard', newShips)
 where
  (funds', shipyard')              = processShipyardTask secs funds shipyard
  (newFunds, shipyard'', newShips) = processShipyardBuild secs funds' shipyard'
  newShipyard'                     = processShipyardQueue shipyard''

orbitLength :: Double -> Double -> (Double, Double)
orbitLength minDistance maxDistance = (ellipseWidth, ellipseHeight)
 where
  paramA        = (minDistance + maxDistance) / 2
  e             = (paramA - minDistance) / paramA
  paramB        = paramA * sqrt (1 - e ** 2)
  ellipseWidth  = 2 * paramA
  ellipseHeight = 2 * paramB

solveBisection :: (Double -> Double) -> Double -> Double -> Double -> Double
solveBisection fn xmin xmax precision = bisection
 where
  xmid = (xmin + xmax) * 0.5
  bisection | xmax - xmin < precision = xmid
            | fn xmin * fn xmid < 0   = solveBisection fn xmin xmid precision
            | otherwise               = solveBisection fn xmid xmax precision

-- TODO: Not sure this is accurate any more. Seems to have awefully slow moon orbits.
-- TODO: Need to find more efficient way of solving orbits.
solveOrbit
  :: Double -> Double -> Double -> Seconds -> Double -> (Double, Double)
solveOrbit gravitySourceMass minRadius maxRadius secs precision = (theta', r)
 where
  newtonGravConstant = 6.6740831e-11
  mu                 = gravitySourceMass * newtonGravConstant
  eccentricity       = (maxRadius - minRadius) / (maxRadius + minRadius)
  semiLatusRectum    = minRadius * (1 + eccentricity)
  semiMajorAxis      = semiLatusRectum / (1 - eccentricity ** 2)
  period             = sqrt (semiMajorAxis ** 3 / mu)
  meanAnomaly        = (fromIntegral secs / period) `mod'` (2 * pi)
  eccentricityAnomaly temp = meanAnomaly - (temp - eccentricity * sin temp)
  paramE = solveBisection eccentricityAnomaly 0 (2 * pi) precision
  trueAnomaly theta'' =
    (1 - eccentricity)
      *  tan (theta'' / 2)
      ** 2
      -  ((1 + eccentricity) * tan (paramE / 2) ** 2)
  theta  = solveBisection trueAnomaly 0 pi precision
  theta' = if paramE > pi then 2 * pi - theta else theta
  r      = semiMajorAxis * (1 - eccentricity * cos paramE)

getBodyCoordinates :: Double -> Double -> Double -> Seconds -> (Double, Double)
getBodyCoordinates gravitySourceMass minRadius maxRadius secs = (x, y)
 where
  scale      = 1e3
  (theta, r) = solveOrbit gravitySourceMass
                          (minRadius * scale)
                          (maxRadius * scale)
                          secs
                          1e-12
  x = -r * cos theta / scale
  y = r * sin theta / scale
