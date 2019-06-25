{-# LANGUAGE OverloadedStrings #-}

module Nebula4x.Utils.Middlewares where

import           Network.Wai                 (Middleware)
import           Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors)

corsified :: Middleware
corsified = cors (const $ Just appCorsResourcePolicy)

appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy =
  CorsResourcePolicy
    { corsOrigins = Nothing
    , corsMethods = ["GET", "PUT", "POST", "DELETE"]
    , corsRequestHeaders = ["Content-Type", "X-Requested-With"]
    , corsExposedHeaders = Nothing
    , corsMaxAge = Nothing
    , corsVaryOrigin = False
    , corsRequireOrigin = False
    , corsIgnoreFailures = False
    }
