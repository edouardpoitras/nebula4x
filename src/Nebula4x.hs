-- |
-- Module    : Nebula4x
-- Copyright : (c) Edouard Poitras
-- License : BSD3
-- Maintainer: Edouard Poitras <edouardpoitras@gmail.com>
--
module Nebula4x where

import           Nebula4x.API

--import Web.Browser (openBrowser)
startGameServer :: IO ()
startGameServer = do
  let port = 2025
    --success <- openBrowser $ "http://localhost:" ++ show port
  startServer port routes

routes :: WebRoute ()
routes = do
  armorResearchRoute
  cargoHandlingResearchRoute
  cargoHoldResearchRoute
  engineDesignRoute
  engineFuelConsumptionResearchRoute
  engineModifierResearchRoute
  engineSizeResearchRoute
  engineTechnologyResearchRoute
  fuelStorageResearchRoute
  installmentsResearchRoute
  installmentsRoute
  jumpGateResearchRoute
  laserDesignRoute
  laserFocalSizeResearchRoute
  laserRechargeRateResearchRoute
  laserReducedSizeResearchRoute
  laserWavelengthResearchRoute
  massDriverRoute
  missleLauncherDesignRoute
  missleLauncherReducedSizeResearchRoute
  missleLauncherReloadRateResearchRoute
  missleLauncherSizeResearchRoute
  productionRoute
  raceRoute
  researchRoute
  sensorsResearchRoute
  shipDesignRoute
  shieldsResearchRoute
  shipOrderRoute
  shipyardRoute
  stateRoute
  systemRoute
  tickRoute
  toggleRefiningRoute
  toggleShieldsRoute
