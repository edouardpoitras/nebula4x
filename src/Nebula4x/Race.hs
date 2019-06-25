module Nebula4x.Race where

import           Control.Lens
import qualified Data.Map.Strict               as Map

import           Nebula4x.Types

getPlayerRaceId :: GameState -> ComponentId
getPlayerRaceId gs = head
  (Map.keys
    (Map.filter (\race -> view rController race == Human) $ view races gs)
  )
