{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Nebula4x.API.ActiveRefining where

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

data ToggleRefiningRequest = ToggleRefiningRequest
  { toggleRefiningSystemId      :: StarSystemId
  , toggleRefiningBodyId        :: BodyId
  } deriving (Show, Eq, Generic)

instance ToJSON ToggleRefiningRequest

instance FromJSON ToggleRefiningRequest

toggleRefiningRoute :: WebRoute ()
toggleRefiningRoute = do
  post "/api/toggle-refining" $ do
    trRequest <- jsonData
    let systemId   = toggleRefiningSystemId trRequest
    let bodyId     = toggleRefiningBodyId trRequest
    webM $ modify $ over
      (systems . at' systemId . ssBodies . at' bodyId . bRefiningFuel)
      (\(ActiveRefining currentlyRefining) ->
        ActiveRefining $ not currentlyRefining
      )
    json True
