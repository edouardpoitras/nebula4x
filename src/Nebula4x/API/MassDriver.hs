{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Nebula4x.API.MassDriver where

import           Control.Lens
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           GHC.Generics                   ( Generic )
import           Web.Scotty.Trans               ( json
                                                , jsonData
                                                , post
                                                )

import           Nebula4x.API.Server
import           Nebula4x.Types
import           Nebula4x.Utils

data MassDriverRequest = MassDriverRequest
  { massDriverSystemId      :: StarSystemId
  , massDriverBodyId        :: BodyId
  , massDriverDestinationId :: BodyId
  } deriving (Show, Eq, Generic)

instance ToJSON MassDriverRequest

instance FromJSON MassDriverRequest

massDriverRoute :: WebRoute ()
massDriverRoute = do
  post "/api/mass-driver" $ do
    mdRequest <- jsonData
    let systemId = massDriverSystemId mdRequest
    let bodyId   = massDriverBodyId mdRequest
    let destId   = massDriverDestinationId mdRequest
    webM $ modify $ set
      (systems . at' systemId . ssBodies . at' bodyId . bMassDriverBody)
      (Just destId)
    json True
