{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Nebula4x.API.Installments where

import           Control.Lens
import           Data.Map.Strict               as Map
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Maybe
import           GHC.Generics                   ( Generic )
import           Web.Scotty.Trans               ( get
                                                , json
                                                , param
                                                )

import           Nebula4x.API.Server
import           Nebula4x.Installment
import           Nebula4x.Utils
import           Nebula4x.Types

data BodyInstallmentResponse = BodyInstallmentResponse
  { instSystemId :: StarSystemId
  , instBodyId :: BodyId
  , instInstallments :: Installments
  } deriving (Show, Eq, Generic)

instance ToJSON BodyInstallmentResponse

instance FromJSON BodyInstallmentResponse

installmentsRoute :: WebRoute ()
installmentsRoute = do
  get "/api/installments" $ json availableInstallments
  get "/api/installments/:systemId/:bodyId" $ do
    systemIdStr <- param "systemId"
    bodyIdStr   <- param "bodyId"
    let systemId = read systemIdStr
    let bodyId   = read bodyIdStr
    gameState <- webM readState
    let bodies       = view (systems . at' systemId . ssBodies) gameState
    let body         = fromJust $ Map.lookup bodyId bodies
    let installments = view bInstallments body
    json $ BodyInstallmentResponse systemId bodyId installments
