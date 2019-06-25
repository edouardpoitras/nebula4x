import           Control.Lens
import           Data.Map.Strict               as Map
import           Data.Time.Clock.POSIX
import           System.Random

import           Nebula4x.GameState
import           Nebula4x.StarSystem
import           Nebula4x.Types

-- Temporary benchmark test
main :: IO ()
main = do
  let g = mkStdGen 8888888888
  let (randomStarSystems, g') =
        generateStarSystems g (view config startingGameState) 100
  let
    gs =
      startingGameState { _systems = randomStarSystems, _randomSeed = show g' }
  --let gs = GameState randomStarSystems
                     --tempDesigns
                     --tempResearch
                     --tempProduction
                     --(addSeconds (-1) defaultStartTime)
                     --startingConfig
                     --startingStatus
                     --(show g')
  currentTime <- getPOSIXTime
  (newGS, newLogs) <- tickFromString "30d" gs
  print (Map.keys (view systems newGS))
  newTime <- getPOSIXTime
  print (newTime - currentTime)
