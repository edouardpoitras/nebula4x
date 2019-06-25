module Nebula4x.Component.Shield where

import qualified Data.Map.Strict               as Map

import           Nebula4x.Types

--
-- For now we will combine Shield and Regeneration Rate into a single research for shield type.
-- All shield research will be doubled in cost.
-- All shields will have size 1.
-- All shields will cost 4x the rating value.
-- All shields will fully recharge in 300 seconds.
-- All shields will consume fuel at 10 times their component rating per hour when active.
-- Shield fuel consumption will not be affected by fuel consumption research (for now).
--
shieldMineralCostRatio :: MineralCost
shieldMineralCostRatio =
  Map.fromList [(Corbomite, 0.375), (Boronide, 0.375), (Duranium, 0.25)]

alphaShield :: Shield
alphaShield = Shield (ComponentName "Alpha Shields") (ComponentRating 1)

alphaShieldResearch :: ShieldResearch
alphaShieldResearch =
  ShieldResearch 1 (ComponentResearch 2000) (ComponentResearch 0) alphaShield

betaShield :: Shield
betaShield = Shield (ComponentName "Beta Shields") (ComponentRating 1.5)

betaShieldResearch :: ShieldResearch
betaShieldResearch =
  ShieldResearch 2 (ComponentResearch 4000) (ComponentResearch 0) betaShield

gammaShield :: Shield
gammaShield = Shield (ComponentName "Gamma Shields") (ComponentRating 2)

gammaShieldResearch :: ShieldResearch
gammaShieldResearch =
  ShieldResearch 3 (ComponentResearch 8000) (ComponentResearch 0) gammaShield

deltaShield :: Shield
deltaShield = Shield (ComponentName "Delta Shields") (ComponentRating 2.5)

deltaShieldResearch :: ShieldResearch
deltaShieldResearch =
  ShieldResearch 4 (ComponentResearch 16000) (ComponentResearch 0) deltaShield

epsilonShield :: Shield
epsilonShield = Shield (ComponentName "Epsilon Shields") (ComponentRating 3)

epsilonShieldResearch :: ShieldResearch
epsilonShieldResearch =
  ShieldResearch 5 (ComponentResearch 30000) (ComponentResearch 0) epsilonShield

thetaShield :: Shield
thetaShield = Shield (ComponentName "Theta Shields") (ComponentRating 4)

thetaShieldResearch :: ShieldResearch
thetaShieldResearch =
  ShieldResearch 6 (ComponentResearch 60000) (ComponentResearch 0) thetaShield

xiShield :: Shield
xiShield = Shield (ComponentName "Xi Shields") (ComponentRating 5)

xiShieldResearch :: ShieldResearch
xiShieldResearch =
  ShieldResearch 7 (ComponentResearch 125000) (ComponentResearch 0) xiShield

omicronShield :: Shield
omicronShield = Shield (ComponentName "Omicron Shields") (ComponentRating 6)

omicronShieldResearch :: ShieldResearch
omicronShieldResearch = ShieldResearch 8
                                       (ComponentResearch 250000)
                                       (ComponentResearch 0)
                                       omicronShield

sigmaShield :: Shield
sigmaShield = Shield (ComponentName "Sigma Shields") (ComponentRating 8)

sigmaShieldResearch :: ShieldResearch
sigmaShieldResearch =
  ShieldResearch 9 (ComponentResearch 500000) (ComponentResearch 0) sigmaShield

tauShield :: Shield
tauShield = Shield (ComponentName "Tau Shields") (ComponentRating 10)

tauShieldResearch :: ShieldResearch
tauShieldResearch =
  ShieldResearch 10 (ComponentResearch 1000000) (ComponentResearch 0) tauShield

psiShield :: Shield
psiShield = Shield (ComponentName "Psi Shields") (ComponentRating 12)

psiShieldResearch :: ShieldResearch
psiShieldResearch =
  ShieldResearch 11 (ComponentResearch 2000000) (ComponentResearch 0) psiShield

omegaShield :: Shield
omegaShield = Shield (ComponentName "Omega Shields") (ComponentRating 15)

omegaShieldResearch :: ShieldResearch
omegaShieldResearch = ShieldResearch 12
                                     (ComponentResearch 4000000)
                                     (ComponentResearch 0)
                                     omegaShield
