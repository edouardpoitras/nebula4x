{-# LANGUAGE OverloadedStrings #-}

module Nebula4x.API.Tick
  ( tickRoute
  )
where

import           Control.Lens                   ( view )
import           Control.Monad.IO.Class         (liftIO)
import           Web.Scotty.Trans               ( get
                                                , json
                                                , param
                                                , post
                                                )

import           Nebula4x.API.Server            ( WebRoute
                                                , modify
                                                , readState
                                                , webM
                                                )
import           Nebula4x.GameState             ( tickFromString )
import           Nebula4x.Types

tickRoute :: WebRoute ()
tickRoute = do
  post "/api/tick/:time" $ do
    timeString   <- param "time"
    state        <- webM readState
    -- TODO: Return the logs to the UI as well.
    (newState, _) <- liftIO $ tickFromString timeString state
    webM $ modify (\_ -> newState)
    json $ view universeTime newState
  get "/api/get-time" $ do
    state <- webM readState
    json $ view universeTime state