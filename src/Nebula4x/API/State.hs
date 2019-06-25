{-# LANGUAGE OverloadedStrings #-}

module Nebula4x.API.State
  ( stateRoute
  ) where

import qualified Data.Aeson                 as A
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Network.Wai.Parse
import           Web.Scotty.Trans           (files, get, json, jsonData, post,
                                             raise, setHeader, stringError)

import           Nebula4x.API.Server        (WebRoute, modify, readState, webM)

stateRoute :: WebRoute ()
stateRoute = do
  get "/api/get-state" $ do
    gameState <- webM readState
    json gameState
  get "/api/export-state" $ do
    gameState <- webM readState
    setHeader "Content-Type" "application/octet-stream"
    setHeader "Content-Disposition" "attachment; filename=\"nebula4x.save\";"
    json gameState
  post "/api/load-state" $ do
    gameState <- jsonData
    webM $ modify (\_ -> gameState)
    gs <- webM readState
    json gs
  post "/api/import-state" $ do
    fs <- files
    let fs' =
          [ (fieldName, BS.unpack (fileName fi), fileContent fi)
          | (fieldName, fi) <- fs
          ]
    let (_, _, uploadedContent) = head fs'
    newGameState <-
      either
        (\e ->
           raise $
           stringError $
           "jsonData - no parse: " ++
           e ++ ". Data was:" ++ BL.unpack uploadedContent)
        return $
      A.eitherDecode uploadedContent
    webM $ modify (\_ -> newGameState)
    json newGameState
