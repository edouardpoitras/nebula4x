{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Nebula4x.API.Production where

import           Control.Lens
import           Data.Aeson           (FromJSON, ToJSON)
import qualified Data.Map.Strict      as Map
import           Data.Maybe
import           GHC.Generics         (Generic)
import           Web.Scotty.Trans     (get, json, jsonData, post)

import           Nebula4x.API.Server
import           Nebula4x.Installment
import qualified Nebula4x.Types       as Types
import           Nebula4x.Utils       (at')

type ProductionPercentages = Map.Map Types.InstallmentId Double

data UpdateProduction = UpdateProduction
  { updateProductionBodyId      :: Types.BodyId
  , updateProductionPercentages :: ProductionPercentages
  } deriving (Show, Eq, Generic)

instance ToJSON UpdateProduction

instance FromJSON UpdateProduction

productionRoute :: WebRoute ()
productionRoute = do
  get "/api/production" $ do
    gameState <- webM readState
    json $ view Types.production gameState
  post "/api/production/update" $ do
    updateProduction <- jsonData
    let bodyId = updateProductionBodyId updateProduction
    let productionPercentages = updateProductionPercentages updateProduction
    if totalProductionPercentages productionPercentages > 100
      then webM readState >>= json . view Types.production
      else webM (modify $ updateBodyProduction bodyId productionPercentages) >>
           webM readState >>=
           json . view Types.production

totalProductionPercentages :: ProductionPercentages -> Double
totalProductionPercentages pp = total
  where
    total = Map.foldr (+) 0 pp

updateBodyProduction ::
     Types.BodyId -> ProductionPercentages -> Types.GameState -> Types.GameState
updateBodyProduction bId pp gs = newGameState
  where
    prod = view Types.production gs
    maybeBodyProd = Map.lookup bId prod
    newBodyProd =
      if isNothing maybeBodyProd
        then Map.foldrWithKey processInstallments populatedBodyProd pp
        else Map.foldrWithKey processInstallments (fromJust maybeBodyProd) pp
    processInstallments instId allocation bProd =
      set
        (at' instId . Types.ipAllocation)
        (Types.ProductionAllocation allocation)
        bProd
    populatedBodyProd = Map.map getInstallmentProduction availableInstallments
    getInstallmentProduction installment =
      Types.InstallmentProduction
        (Types.ProductionProgress 0)
        (Types.ProductionAllocation 0)
        installment
    newProd = Map.insert bId newBodyProd prod
    newGameState = set Types.production newProd gs
