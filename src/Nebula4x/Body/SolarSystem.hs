module Nebula4x.Body.SolarSystem where

import           Control.Lens
import qualified Data.Map.Strict               as Map

import           Nebula4x.Mineral
import           Nebula4x.Shipyard
import           Nebula4x.Types

solarSystem :: StarSystem
solarSystem = StarSystem
  1
  sol
  (Map.fromList
    [ (view bId mercury, mercury)
    , (view bId venus  , venus)
    , (view bId earth  , earth)
    , (view bId mars   , mars)
    , (view bId jupyter, jupyter)
    , (view bId saturn , saturn)
    , (view bId uranus , uranus)
    , (view bId neptune, neptune)
    ]
  )
  []
  noShips
  noMissleSalvos
  noWormholes
  notDiscovered

sol :: Star
sol = Star 1 (BodyName "Sol") (BodyMass 1.9891e30) (BodyRadius 695700)

mercury :: Body
mercury = Body 1
               noRace
               (BodyName "Mercury")
               (BodyMass 3.3011e23)
               (BodyLocation 0 0)
               (BodyRadius 2439.7)
               (Range 46001200 69816900)
               Terrestrial
               noShipyards
               (FuelReserves 0)
               notRefining
               noInstallments
               noMinerals
               Nothing
               noSurveys
               Nothing

venus :: Body
venus = Body 2
             noRace
             (BodyName "Venus")
             (BodyMass 4.8675e24)
             (BodyLocation 0 0)
             (BodyRadius 6051.8)
             (Range 107477000 108939000)
             Terrestrial
             noShipyards
             (FuelReserves 0)
             notRefining
             noInstallments
             noMinerals
             Nothing
             noSurveys
             Nothing

earth :: Body
earth = Body 3
             noRace
             (BodyName "Earth")
             (BodyMass 5.97237e24)
             (BodyLocation 0 0)
             (BodyRadius 6371)
             (Range 147095000 152100000)
             Terrestrial
             noShipyards
             (FuelReserves 0)
             notRefining
             noInstallments
             noMinerals
             Nothing
             noSurveys
             Nothing

mars :: Body
mars = Body 4
            noRace
            (BodyName "Mars")
            (BodyMass 6.4171e23)
            (BodyLocation 0 0)
            (BodyRadius 3389.5)
            (Range 206700000 249200000)
            Terrestrial
            noShipyards
            (FuelReserves 0)
            notRefining
            noInstallments
            noMinerals
            Nothing
            noSurveys
            Nothing

jupyter :: Body
jupyter = Body 5
               noRace
               (BodyName "Jupiter")
               (BodyMass 1.8982e27)
               (BodyLocation 0 0)
               (BodyRadius 69911)
               (Range 740520000 816620000)
               GasGiant
               noShipyards
               (FuelReserves 0)
               notRefining
               noInstallments
               noMinerals
               Nothing
               noSurveys
               Nothing

saturn :: Body
saturn = Body 6
              noRace
              (BodyName "Saturn")
              (BodyMass 5.6834e26)
              (BodyLocation 0 0)
              (BodyRadius 58232)
              (Range 1352550000 1514500000)
              GasGiant
              noShipyards
              (FuelReserves 0)
              notRefining
              noInstallments
              noMinerals
              Nothing
              noSurveys
              Nothing

uranus :: Body
uranus = Body 7
              noRace
              (BodyName "Uranus")
              (BodyMass 8.681e25)
              (BodyLocation 0 0)
              (BodyRadius 25362)
              (Range 2742000000 3008000000)
              IceGiant
              noShipyards
              (FuelReserves 0)
              notRefining
              noInstallments
              noMinerals
              Nothing
              noSurveys
              Nothing

neptune :: Body
neptune = Body 8
               noRace
               (BodyName "Neptune")
               (BodyMass 1.0243e26)
               (BodyLocation 0 0)
               (BodyRadius 24622)
               (Range 4460000000 4540000000)
               IceGiant
               noShipyards
               (FuelReserves 0)
               notRefining
               noInstallments
               noMinerals
               Nothing
               noSurveys
               Nothing
